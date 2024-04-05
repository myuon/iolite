use self::vm::Instruction;

pub mod ast;
pub mod ir;
pub mod ir_code_gen;
pub mod lexer;
pub mod parser;
pub mod vm;
pub mod vm_code_gen;

pub struct Compiler {
    lexer: lexer::Lexer,
    parser: parser::Parser,
    ir_code_gen: ir_code_gen::IrCodeGenerator,
    vm_code_gen: vm_code_gen::VmCodeGenerator,
}

impl Compiler {
    pub fn compile_expr(input: String) -> Result<Vec<Instruction>, Box<dyn std::error::Error>> {
        let mut lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer.run().unwrap());
        let ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();

        let expr = parser.expr().unwrap();
        let ir = ir_code_gen.expr(expr).unwrap();

        vm_code_gen.term(ir).unwrap();

        Ok(vm_code_gen.code)
    }

    pub fn compile_block(input: String) -> Result<Vec<Instruction>, Box<dyn std::error::Error>> {
        let mut lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer.run().unwrap());
        let ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();

        let expr = parser.block().unwrap();
        let ir = ir_code_gen.block(expr).unwrap();

        vm_code_gen.term(ir).unwrap();

        Ok(vm_code_gen.code)
    }

    pub fn run_expr(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let code = Self::compile_expr(input)?;

        let mut vm = vm::Vm::new(40, code);
        vm.exec();

        Ok(vm.pop())
    }

    pub fn run_block(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let code = Self::compile_block(input)?;

        let mut vm = vm::Vm::new(40, code);
        vm.exec();

        Ok(vm.pop())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_expr() {
        let cases = vec![
            ("1 + 3 * 4", 13),
            ("1 * 3 - 4", -1),
            ("4 * 3 - 2", 10),
            ("4 * (3 - 2)", 4),
        ];

        for (input, expected) in cases {
            let actual = Compiler::run_expr(input.to_string()).unwrap();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_compile_block() {
        let cases = vec![
            ("let x = 1 + 2 * 4; let y = x + 2; y;", 11),
            ("let x = 1; x = x + 2; x;", 3),
        ];

        for (input, expected) in cases {
            let actual = Compiler::run_block(input.to_string()).unwrap();
            assert_eq!(actual, expected);
        }
    }
}
