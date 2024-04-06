use std::env;

use crate::compiler::vm::Instruction;

mod compiler;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args[1] == "compile" {
        let input = args[2].clone();
        let block = compiler::Compiler::parse_block(input).unwrap();

        let ir = compiler::Compiler::ir_code_gen(block).unwrap();
        println!("= IR_CODE_GEN");
        println!("{:#?}", ir);

        let code = compiler::Compiler::vm_code_gen(ir).unwrap();
        println!("= VM_CODE_GEN");

        for inst in code {
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
        let code = compiler::Compiler::compile_block(input).unwrap();
        println!("{:?}", code);

        let result = compiler::Compiler::run_vm(code).unwrap();
        println!("result: {}", result);
    } else {
        println!("Unknown command");
    }
}
