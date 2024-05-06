use std::io::Read;

use anyhow::{anyhow, bail, Result};
use clap::{Parser, Subcommand};
use compiler::runtime::Runtime;
use dap_server::{DapContext, DapImpl};
use lsp_server::LspImpl;
use utils::{dap::server::Dap, lsp::server::Lsp, server_process::ServerProcess};

use crate::compiler::{ast::Module, vm::Instruction};

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
    Compile {
        file: Option<String>,

        #[clap(long)]
        stdin: bool,
    },
    Run {
        file: Option<String>,

        #[clap(long)]
        stdin: bool,
        #[clap(long = "print-stacks")]
        print_stacks: bool,
    },
    Lsp {},
    Dap {},
}

fn compile(input: String) -> Result<Vec<u8>> {
    let decls = compiler::Compiler::parse_decls(compiler::Compiler::create_input(input.clone()))?;
    let mut module = Module {
        name: "main".to_string(),
        declarations: decls,
    };
    let types = compiler::Compiler::typecheck_module(&mut module, &input).unwrap();

    let ir = compiler::Compiler::ir_code_gen_module(module, types).unwrap();
    let code = compiler::Compiler::vm_code_gen(ir).unwrap();
    for inst in &code {
        match inst {
            Instruction::Label(label) => {
                println!("\n{}:", label);
            }
            _ => {
                println!("    {:?}", inst);
            }
        }
    }
    let binary = compiler::Compiler::byte_code_gen(code).unwrap();

    Ok(binary)
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut compiler = compiler::Compiler::new();

    match cli.command {
        CliCommands::Compile { file, stdin } => {
            let input = if stdin {
                let mut buf = Vec::new();
                std::io::stdin().read_to_end(&mut buf).unwrap();

                String::from_utf8(buf)?
            } else {
                file.unwrap()
            };

            compile(input)?;
        }
        CliCommands::Run {
            file,
            stdin,
            print_stacks,
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
            compiler.typecheck(main.clone())?;

            let ir = compiler.ir_code_gen(main.clone())?;
            let code = compiler::Compiler::vm_code_gen(ir)?;
            let binary = compiler::Compiler::byte_code_gen(code)?;

            let result = compiler::Compiler::run_vm(binary, print_stacks)?;

            println!("result: {}", result);
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
