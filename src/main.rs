use std::env;

mod compiler;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args[1] == "compile" {
        let input = args[2].clone();
        let code = compiler::Compiler::compile_block(input).unwrap();
        println!("{:?}", code);
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
