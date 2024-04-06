use self::{ast::Block, vm::Instruction};

pub mod ast;
pub mod ir;
pub mod ir_code_gen;
pub mod lexer;
pub mod parser;
pub mod vm;
pub mod vm_code_gen;

pub struct Compiler {}

impl Compiler {
    pub fn parse_block(input: String) -> Result<Block, Box<dyn std::error::Error>> {
        let mut lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer.run().unwrap());
        let expr = parser.block(None).unwrap();

        Ok(expr)
    }

    pub fn ir_code_gen(block: Block) -> Result<ir::IrTerm, Box<dyn std::error::Error>> {
        let ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        let ir = ir_code_gen.block(block).unwrap();

        Ok(ir)
    }

    pub fn vm_code_gen(ir: ir::IrTerm) -> Result<Vec<Instruction>, Box<dyn std::error::Error>> {
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();
        vm_code_gen.term(ir).unwrap();

        Ok(vm_code_gen.code)
    }

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

        let expr = parser.block(None).unwrap();
        let ir = ir_code_gen.block(expr).unwrap();

        vm_code_gen.term(ir).unwrap();

        Ok(vm_code_gen.code)
    }

    pub fn run_expr(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let code = Self::compile_expr(input)?;

        let mut vm = vm::Vm::new(40, code);
        vm.exec().unwrap();

        Ok(vm.pop())
    }

    pub fn run_block(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let code = Self::compile_block(input)?;

        let mut vm = vm::Vm::new(1024, code);
        vm.exec().unwrap();

        Ok(vm.pop())
    }

    pub fn run_vm(program: Vec<Instruction>) -> Result<i32, Box<dyn std::error::Error>> {
        let mut vm = vm::Vm::new(1024, program);
        vm.exec().unwrap();

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
            ("1 < 2", 1),
            ("1 > 2", 0),
            ("1 <= 2", 1),
            ("1 >= 2", 0),
            ("1 == 2", 0),
            ("2 == 2", 1),
            ("false && false", 0),
            ("true && false", 0),
            ("true && true", 1),
            ("false || false", 0),
            ("true || false", 1),
            ("true || true", 1),
            ("1 != 10", 1),
            ("if true { 2 } else { 5 }", 2),
            ("if false { 2 } else { 5 }", 5),
            ("if false { 2 } else if true { 3 } else { 4 }", 3),
            ("if false { 2 } else if false { 3 } else { 4 }", 4),
            ("if false { 2 } else if false { 3 } else if false { 4 } else if true { 5 } else { 6 }", 5),
            ("if false { 2 } else if false { 3 } else if false { 4 } else if false { 5 } else { 6 }", 6),
        ];

        for (input, expected) in cases {
            println!("====== {}", input);
            let actual = Compiler::run_expr(input.to_string()).unwrap();
            assert_eq!(actual, expected, "input: {}", input);
        }
    }

    #[test]
    fn test_compile_block() {
        let cases = vec![
            ("let x = 1 + 2 * 4; let y = x + 2; y", 11),
            ("let x = 1; x = x + 2; x", 3),
            ("let a = 2; { let a = 3; }; a", 2),
            (
                "let c = 0; let n = 1; while (c < 5) { c = c + 1; n = n * 2; }; n",
                32,
            ),
            // (
            //     "let a = if true { 1 } else { 2 }; if a == 1 { let b = 10; b }",
            //     10,
            // ),
            // (
            //     "let a = 2; if a == 1 { let b = 10; b } else { let b = 20; b }",
            //     20,
            // ),
            // (
            //     "let a = 2; if a == 1 { let b = 10; b } else if a == 2 { let b = 20; b } else { let b = 30; b }",
            //     20,
            // ),
            // (
            //     "let a = 2; { let a = 3; a = 4; }; a",
            //     2,
            // ),
            // (
            //     "let a = 2; { let a = 3; a = 4; } a",
            //     2,
            // ),
            // (
            //     "let a = 2; { let a = 3; let b = 4; let c = 5; } a",
            //     2,
            // ),
            // (
            //     "let a = 2; { a = 3; a = 4; }; a",
            //     4,
            // ),
        ];

        for (input, expected) in cases {
            let actual = Compiler::run_block(input.to_string()).unwrap();
            assert_eq!(actual, expected, "input: {}", input);
        }
    }
}
