use self::vm::Instruction;

mod ast;
mod ir;
mod ir_code_gen;
mod lexer;
mod parser;
pub mod vm;
mod vm_code_gen;

pub struct Compiler {
    lexer: lexer::Lexer,
    parser: parser::Parser,
    ir_code_gen: ir_code_gen::IrCodeGenerator,
    vm_code_gen: vm_code_gen::VmCodeGenerator,
}

impl Compiler {
    pub fn compile(input: String) -> Result<Vec<Instruction>, Box<dyn std::error::Error>> {
        let mut lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer.run().unwrap());
        let ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();

        let expr = parser.expr().unwrap();
        let ir = ir_code_gen.expr(expr).unwrap();

        vm_code_gen.term(ir).unwrap();

        Ok(vm_code_gen.code)
    }

    pub fn run(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let code = Self::compile(input)?;

        let mut vm = vm::Vm::new(40, code);
        vm.exec();

        Ok(vm.pop())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile() {
        let cases = vec![
            ("1 + 3 * 4", 13),
            ("1 * 3 - 4", -1),
            ("4 * 3 - 2", 10),
            ("4 * (3 - 2)", 4),
        ];

        for (input, expected) in cases {
            let actual = Compiler::run(input.to_string()).unwrap();
            assert_eq!(actual, expected);
        }
    }
}
