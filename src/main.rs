use std::env;

use crate::compiler::{ast::Module, vm::Instruction};

mod compiler;

fn compile(input: String) -> Vec<Instruction> {
    let block = compiler::Compiler::parse(input).unwrap();

    let ir = compiler::Compiler::ir_code_gen(Module {
        name: "main".to_string(),
        declarations: block,
    })
    .unwrap();
    println!("= IR_CODE_GEN");
    println!("{:#?}", ir);

    let code = compiler::Compiler::vm_code_gen(ir).unwrap();
    println!("= VM_CODE_GEN");

    code
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args[1] == "compile" {
        let input = args[2].clone();
        let code = compile(input);

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
    } else if args[1] == "run" {
        let input = args[2].clone();
        let code = compile(input);

        let result = compiler::Compiler::run_vm(code).unwrap();
        println!("result: {}", result);
    } else {
        println!("Unknown command");
    }
}
