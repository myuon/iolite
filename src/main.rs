use std::{
    collections::HashMap,
    io::{Read, Write},
};

use anyhow::{anyhow, bail, Result};
use clap::{Parser, Subcommand};
use compiler::{byte_code_emitter::emit_disassemble, runtime::Runtime, CompileOptions};
use dap_server::{DapContext, DapImpl};
use lsp_server::LspImpl;
use tui_debbuger::start_tui_debugger;
use utils::{dap::server::Dap, lsp::server::Lsp, server_process::ServerProcess};

use crate::compiler::ir::Value;

mod compiler;
mod dap_server;
mod lsp_server;
mod tui_debbuger;
mod utils;

#[derive(Parser, Debug)]
#[clap(name = "iolite")]
struct Cli {
    #[clap(subcommand)]
    command: CliCommands,
}

#[derive(Debug, Subcommand)]
enum CliCommands {
    Run {
        file: Option<String>,

        #[clap(long)]
        stdin: bool,
        #[clap(long = "print-stacks")]
        print_stacks: bool,
        #[clap(long = "print-memory-store")]
        print_memory_store: bool,
        #[clap(long = "emit-ir")]
        emit_ir: Option<String>,
        #[clap(long = "emit-vm")]
        emit_vm: Option<String>,
        #[clap(long = "emit-linked-vm")]
        emit_linked_vm: Option<String>,
        #[clap(long = "emit-asm-labels")]
        emit_asm_labels: Option<String>,
        #[clap(long = "emit-asm")]
        emit_asm: Option<String>,
        #[clap(long = "verbose", short = 'v')]
        verbose: bool,
    },
    Test {
        file: Option<String>,
    },
    Lsp {
        #[clap(long)]
        port: Option<usize>,
    },
    Dap {},
    Debugger {
        file: String,
    },
    Version {},
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut compiler = compiler::Compiler::new();

    match cli.command {
        CliCommands::Run {
            file,
            stdin,
            print_stacks,
            print_memory_store,
            emit_ir,
            emit_vm,
            emit_linked_vm,
            emit_asm_labels,
            emit_asm,
            verbose,
        } => {
            let cwd = match file.clone() {
                Some(file) => std::path::Path::new(&file)
                    .parent()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                None => std::env::current_dir()
                    .map_err(|err| anyhow!("Failed to get current directory: {}", err))?
                    .to_str()
                    .unwrap()
                    .to_string(),
            };
            compiler.set_cwd(cwd);

            let source_code = match file {
                Some(file) => std::fs::read_to_string(&file)
                    .map_err(|err| anyhow!("Failed to read file {}: {}", file, err))?,
                None => {
                    if stdin {
                        let mut buf = Vec::new();
                        std::io::stdin().read_to_end(&mut buf).unwrap();

                        String::from_utf8(buf).unwrap()
                    } else {
                        bail!("No input file or stdin provided");
                    }
                }
            };

            let main = "main".to_string();

            measure_time!(
                "Parsed: {}ms",
                {
                    compiler.parse_with_code(main.clone(), source_code.clone(), false)?;
                },
                verbose
            );

            measure_time!(
                "Typechecked: {}ms",
                {
                    compiler.typecheck(main.clone())?;
                },
                verbose
            );

            measure_time!(
                "IR generated: {}ms",
                {
                    compiler.ir_code_gen(main.clone(), false)?;
                    if let Some(file) = emit_ir {
                        std::fs::write(
                            file,
                            format!("{:#?}", compiler.result_ir.as_ref().unwrap()),
                        )?;
                    }
                },
                verbose
            );

            measure_time!(
                "VM code generated: {}ms",
                { compiler.vm_code_gen()? },
                verbose
            );
            let vm = compiler.result_vm.as_ref().unwrap();
            if let Some(file_path) = emit_vm {
                let mut file = std::fs::File::create(file_path.clone())?;

                for module in &vm.modules {
                    file.write(format!(".module: {}\n", module.name).as_bytes())?;
                    if !module.global_section.is_empty() {
                        file.write(
                            format!(
                                ".globals: {}\n",
                                module
                                    .global_section
                                    .iter()
                                    .map(|t| format!("{}", t))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )
                            .as_bytes(),
                        )?;
                    }

                    for (_id, offset, value) in &module.data_section {
                        file.write(
                            format!(
                                ".data: {} {:?}\n",
                                offset,
                                String::from_utf8(value.clone()).unwrap()
                            )
                            .as_bytes(),
                        )?;
                    }

                    for (i, code) in module.instructions.iter().enumerate() {
                        file.write(format!("{}: {:?}\n", i, code).as_bytes())?;
                    }
                }

                eprintln!("Wrote to {}", file_path);
            }

            measure_time!("Linked: {}ms", { compiler.link()? }, verbose);
            if let Some(file_path) = emit_linked_vm {
                let mut file = std::fs::File::create(file_path.clone())?;

                let linked = compiler.result_link.as_ref().unwrap();
                for (i, code) in linked.iter().enumerate() {
                    file.write(format!("{}: {:?}\n", i, code).as_bytes())?;
                }

                eprintln!("Wrote to {}", file_path);
            }

            measure_time!(
                "Byte code generated: {}ms",
                { compiler.byte_code_gen()? },
                verbose
            );
            let emitter = compiler.result_codegen.as_ref().unwrap();
            if let Some(file_path) = emit_asm_labels {
                let mut file = std::fs::File::create(file_path.clone())?;

                let mut labels = emitter.labels.iter().collect::<Vec<_>>();
                labels.sort_by(|x, y| x.1.cmp(&y.1));

                for (label, position) in labels {
                    writeln!(file, "[{}:0x{:x}] .{}", position, position, label)?;
                }
            }

            if let Some(file_path) = emit_asm {
                let mut file = std::fs::File::create(file_path.clone())?;
                emit_disassemble(&mut file, emitter.buffer.clone())?;

                eprintln!("Wrote to {}", file_path);
            }

            measure_time!(
                "Executed: {}ms",
                { compiler.execute(print_stacks, print_memory_store, None)? },
                verbose
            );
            let result = compiler.result_runtime.as_mut().unwrap().pop_i64();
            println!("result: {:?}", Value::from_u64(result as u64));
        }
        CliCommands::Test { file } => {
            let cwd = match file.clone() {
                Some(file) => std::path::Path::new(&file)
                    .parent()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                None => std::env::current_dir()
                    .map_err(|err| anyhow!("Failed to get current directory: {}", err))?
                    .to_str()
                    .unwrap()
                    .to_string(),
            };
            compiler.set_cwd(cwd);

            let source_code = match file {
                Some(file) => std::fs::read_to_string(&file)
                    .map_err(|err| anyhow!("Failed to read file {}: {}", file, err))?,
                None => todo!(),
            };

            let main = "std".to_string();

            compiler.compile(
                Some(main),
                source_code,
                CompileOptions {
                    testing_mode: true,
                    ..Default::default()
                },
            )?;
            compiler.execute(false, false, None)?;
            let result = compiler.result_runtime.as_mut().unwrap().pop_i64();
            println!("result: {:?}", Value::from_u64(result as u64));
        }
        CliCommands::Lsp { port } => {
            Lsp::new(LspImpl).start((), port.unwrap_or(3030)).await?;
        }
        CliCommands::Dap {} => {
            Dap::new(DapImpl)
                .start(
                    DapContext::new(Runtime::new(1024, vec![], HashMap::new())),
                    3031,
                )
                .await?;
        }
        CliCommands::Version {} => {
            println!("0.1.0");
        }
        CliCommands::Debugger { file } => {
            start_tui_debugger(file)?;
        }
    }

    Ok(())
}
