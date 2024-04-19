use clap::{Parser, Subcommand};

use crate::compiler::{ast::Module, vm::Instruction};

mod compiler;

#[derive(Parser, Debug)]
#[clap(name = "iolite")]
struct Cli {
    #[clap(subcommand)]
    command: CliCommands,
}

#[derive(Debug, Subcommand)]
enum CliCommands {
    Compile { input: String },
    Run { input: String },
}

fn compile(input: String) -> Vec<u8> {
    let decls = compiler::Compiler::parse(compiler::Compiler::create_input(input.clone())).unwrap();
    let mut module = Module {
        name: "main".to_string(),
        declarations: decls,
    };
    let types = compiler::Compiler::typecheck(&mut module, &input).unwrap();

    let ir = compiler::Compiler::ir_code_gen(module, types).unwrap();
    println!("= IR_CODE_GEN");
    println!("{:#?}", ir);

    let code = compiler::Compiler::vm_code_gen(ir).unwrap();
    println!("= VM_CODE_GEN");

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
    println!("= BYTE_CODE_GEN");
    println!("{:x?}", binary);

    binary
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        CliCommands::Compile { input } => {
            compile(input);
        }
        CliCommands::Run { input } => {
            let result = compiler::Compiler::run(input).unwrap();
            println!("result: {}", result);
        }
    }
}
