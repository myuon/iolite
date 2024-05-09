use std::io::{Read, Write};

use anyhow::{anyhow, bail, Result};
use clap::{Parser, Subcommand};
use compiler::runtime::Runtime;
use dap_server::{DapContext, DapImpl};
use lsp_server::LspImpl;
use utils::{dap::server::Dap, lsp::server::Lsp, server_process::ServerProcess};

use crate::compiler::ir::Value;

mod compiler;
mod dap_server;
mod lsp_server;
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
        #[clap(long = "emit-ir")]
        emit_ir: Option<String>,
        #[clap(long = "emit-vm")]
        emit_vm: Option<String>,
    },
    Lsp {},
    Dap {},
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
            emit_ir,
            emit_vm,
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

            compiler.parse_with_code(main.clone(), source_code)?;
            eprintln!("Parsed");

            compiler.typecheck(main.clone())?;
            eprintln!("Typechecked");

            let ir = compiler.ir_code_gen(main.clone())?;
            if let Some(file) = emit_ir {
                std::fs::write(file, format!("{:#?}", ir))?;
            }
            eprintln!("IR generated");

            let code = compiler::Compiler::vm_code_gen(ir)?;
            if let Some(file) = emit_vm {
                let mut file = std::fs::File::create(file)?;
                for (i, code) in code.iter().enumerate() {
                    file.write(format!("{}: {:?}\n", i, code).as_bytes())?;
                }
            }
            eprintln!("VM code generated");

            let binary = compiler::Compiler::byte_code_gen(code)?;
            eprintln!("Byte code generated");

            let result = compiler::Compiler::run_vm(binary, print_stacks)?;
            println!("result: {:?}", Value::from_u64(result as u64));
        }
        CliCommands::Lsp {} => {
            Lsp::new(LspImpl).start((), 3030).await?;
        }
        CliCommands::Dap {} => {
            Dap::new(DapImpl)
                .start(DapContext::new(Runtime::new(1024, vec![])), 3031)
                .await?;
        }
    }

    Ok(())
}
