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
        #[clap(long = "emit-linked-vm")]
        emit_linked_vm: Option<String>,
        #[clap(long = "emit-asm")]
        emit_asm: Option<String>,
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
            emit_linked_vm,
            emit_asm,
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

            let vm = compiler::Compiler::vm_code_gen(ir)?;
            if let Some(file) = emit_vm {
                let mut file = std::fs::File::create(file)?;

                for module in &vm.modules {
                    file.write(format!(".module: {}\n", module.name).as_bytes())?;

                    for (i, code) in module.instructions.iter().enumerate() {
                        file.write(format!("{}: {:?}\n", i, code).as_bytes())?;
                    }
                }
            }
            eprintln!("VM code generated");

            let linked = compiler::Compiler::link(vm)?;
            eprintln!("Linked");
            if let Some(file) = emit_linked_vm {
                let mut file = std::fs::File::create(file)?;

                for (i, code) in linked.iter().enumerate() {
                    file.write(format!("{}: {:?}\n", i, code).as_bytes())?;
                }
            }

            let binary = compiler::Compiler::byte_code_gen(linked)?;
            eprintln!("Byte code generated");
            if let Some(file) = emit_asm {
                let mut file = std::fs::File::create(file)?;
                emit_disassemble(&mut file, binary.clone())?;
            }

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

fn emit_disassemble(writer: &mut impl std::io::Write, binary: Vec<u8>) -> Result<()> {
    let mut position = 0;
    let consume = |position: &mut usize| -> u8 {
        let byte = binary[*position];
        *position += 1;

        byte
    };
    let consume_u64 = |position: &mut usize| -> u64 {
        u64::from_le_bytes([
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
        ])
    };

    while position < binary.len() {
        write!(writer, "{:x}: {:02x} ", position, binary[position])?;

        match consume(&mut position) {
            0x01 => {
                let value = consume_u64(&mut position);
                write!(
                    writer,
                    "{} ",
                    binary[position - 8..position]
                        .iter()
                        .map(|t| format!("{:02x}", t))
                        .collect::<Vec<_>>()
                        .join(" "),
                )?;
                writeln!(writer, ";; PUSH {:?}", Value::from_u64(value))?;
            }
            0x02 => {
                writeln!(writer, ";; CALL")?;
            }
            0x03 => {
                let value = consume_u64(&mut position);
                writeln!(writer, ";; EXTCALL {}", value)?;
            }
            0x04 => {
                writeln!(writer, ";; RET")?;
            }
            0x05 => {
                writeln!(writer, ";; JUMP")?;
            }
            0x06 => {
                writeln!(writer, ";; JUMP_IF")?;
            }
            0x07 => {
                writeln!(writer, ";; NOP")?;
            }
            0x08 => {
                let offset = consume_u64(&mut position);
                let length = consume_u64(&mut position);
                let data = &binary[position..(position + length as usize)];
                write!(
                    writer,
                    "{:02x} {:02x} {} ",
                    offset,
                    length,
                    data.iter()
                        .map(|t| format!("{:02x}", t))
                        .collect::<Vec<_>>()
                        .join(" ")
                )?;

                write!(writer, ";; DATA {} {} [", offset, length)?;
                for i in 0..length {
                    if i > 0 {
                        write!(writer, " ")?;
                    }
                    let b = consume(&mut position);
                    write!(
                        writer,
                        "{}",
                        std::char::from_u32(b as u32)
                            .map(|t| if t.is_ascii_graphic() {
                                format!("{}", t)
                            } else {
                                format!("{:02x}", b)
                            })
                            .unwrap_or(format!("{:02x}", b))
                    )?;
                }
                writeln!(writer, "]")?;
            }
            0x10 => {
                write!(writer, ";; ")?;
                writeln!(writer, "ADD")?;
            }
            0x11 => {
                write!(writer, ";; ")?;
                writeln!(writer, "SUB")?;
            }
            0x12 => {
                write!(writer, ";; ")?;
                writeln!(writer, "MUL")?;
            }
            0x13 => {
                write!(writer, ";; ")?;
                writeln!(writer, "DIV")?;
            }
            0x14 => {
                write!(writer, ";; ")?;
                writeln!(writer, "ADD_FLOAT")?;
            }
            0x15 => {
                write!(writer, ";; ")?;
                writeln!(writer, "SUB_FLOAT")?;
            }
            0x16 => {
                write!(writer, ";; ")?;
                writeln!(writer, "MUL_FLOAT")?;
            }
            0x17 => {
                write!(writer, ";; ")?;
                writeln!(writer, "DIV_FLOAT")?;
            }
            0x20 => {
                write!(writer, ";; ")?;
                writeln!(writer, "XOR")?;
            }
            0x21 => {
                write!(writer, ";; ")?;
                writeln!(writer, "AND")?;
            }
            0x22 => {
                write!(writer, ";; ")?;
                writeln!(writer, "OR")?;
            }
            0x23 => {
                write!(writer, ";; ")?;
                writeln!(writer, "NOT")?;
            }
            0x30 => {
                write!(writer, ";; ")?;
                writeln!(writer, "EQ")?;
            }
            0x31 => {
                write!(writer, ";; ")?;
                writeln!(writer, "NOT_EQ")?;
            }
            0x32 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LT")?;
            }
            0x33 => {
                write!(writer, ";; ")?;
                writeln!(writer, "GT")?;
            }
            0x34 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LE")?;
            }
            0x35 => {
                write!(writer, ";; ")?;
                writeln!(writer, "GE")?;
            }
            0x40 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LOAD")?;
            }
            0x41 => {
                write!(writer, ";; ")?;
                match consume(&mut position) {
                    0x01 => writeln!(writer, "LOAD_BP")?,
                    0x02 => writeln!(writer, "LOAD_SP")?,
                    _ => bail!("Invalid LOAD instruction"),
                }
            }
            0x42 => {
                write!(writer, ";; ")?;
                writeln!(writer, "STORE")?;
            }
            0x43 => {
                write!(writer, ";; ")?;
                match consume(&mut position) {
                    0x01 => writeln!(writer, "STORE_BP")?,
                    0x02 => writeln!(writer, "STORE_SP")?,
                    _ => bail!("Invalid STORE instruction"),
                }
            }
            0x44 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LOAD_8")?;
            }
            0x45 => {
                write!(writer, ";; ")?;
                writeln!(writer, "STORE_8")?;
            }
            0x46 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LOAD_32")?;
            }
            0x47 => {
                write!(writer, ";; ")?;
                writeln!(writer, "STORE_32")?;
            }
            0x50 => {
                write!(writer, ";; ")?;
                writeln!(writer, "INT_TO_FLOAT")?;
            }
            0x51 => {
                write!(writer, ";; ")?;
                writeln!(writer, "FLOAT_TO_INT")?;
            }
            0x52 => {
                write!(writer, ";; ")?;
                writeln!(writer, "INT_TO_BYTE")?;
            }
            0x61 => {
                let start = consume_u64(&mut position);
                let end = consume_u64(&mut position);

                write!(writer, ";; ")?;
                writeln!(writer, "SOURCE_MAP {}:{}", start, end)?;
            }
            p => todo!("Unknown instruction: {:02x}", p),
        }
    }

    Ok(())
}
