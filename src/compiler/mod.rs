use self::{
    ast::{Block, Declaration, Module},
    byte_code_emitter::ByteCodeEmitter,
    runtime::Runtime,
    vm::Instruction,
};

pub mod ast;
pub mod byte_code_emitter;
pub mod ir;
pub mod ir_code_gen;
pub mod lexer;
pub mod parser;
pub mod runtime;
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

    pub fn parse(input: String) -> Result<Vec<Declaration>, Box<dyn std::error::Error>> {
        let mut lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer.run().unwrap());
        let expr = parser.decls().unwrap();

        Ok(expr)
    }

    pub fn ir_code_gen(block: Module) -> Result<ir::IrModule, Box<dyn std::error::Error>> {
        let ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        let ir = ir_code_gen.module(block).unwrap();

        Ok(ir)
    }

    pub fn vm_code_gen(ir: ir::IrModule) -> Result<Vec<Instruction>, Box<dyn std::error::Error>> {
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();
        vm_code_gen.program(ir).unwrap();

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

    pub fn compile(input: String) -> Result<Vec<Instruction>, Box<dyn std::error::Error>> {
        let mut lexer = lexer::Lexer::new(input);
        let mut parser = parser::Parser::new(lexer.run().unwrap());
        let ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();

        let decls = parser.decls().unwrap();
        let ir = ir_code_gen
            .module(Module {
                name: "main".to_string(),
                declarations: decls,
            })
            .unwrap();

        vm_code_gen.program(ir).unwrap();

        Ok(vm_code_gen.code)
    }

    pub fn run_expr(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let mut emitter = ByteCodeEmitter::new();
        let code = Self::compile_expr(input)?;
        emitter.exec(code).unwrap();

        let mut vm = Runtime::new(1024, emitter.buffer);
        vm.exec().unwrap();

        Ok(vm.pop())
    }

    pub fn run_block(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let mut emitter = ByteCodeEmitter::new();
        let code = Self::compile_block(input)?;
        emitter.exec(code).unwrap();

        let mut vm = Runtime::new(1024, emitter.buffer);
        vm.exec().unwrap();

        Ok(vm.pop())
    }

    pub fn run_vm(program: Vec<Instruction>) -> Result<i32, Box<dyn std::error::Error>> {
        let mut emitter = ByteCodeEmitter::new();
        emitter.exec(program).unwrap();

        let mut vm = Runtime::new(1024, emitter.buffer);
        vm.exec().unwrap();

        Ok(vm.pop())
    }

    pub fn run(input: String) -> Result<i32, Box<dyn std::error::Error>> {
        let mut emitter = ByteCodeEmitter::new();

        let code = Self::compile(input)?;
        emitter.exec(code).unwrap();

        let mut vm = Runtime::new(1024, emitter.buffer);
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
            ("10 - 2 - 5", 3),
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
            ("match true { true => 2, false => 5 }", 2),
            ("match false { true => 2, false => 5 }", 5),
            ("match false { true => 2, false => 5 }", 5),
            (
                "match false { true => 2, false => match true { true => 3, false => 4 } }",
                3,
            ),
            (
                "match false { true => 2, false => match false { true => 3, false => 4 } }",
                4,
            ),
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
            (
                "let c = 0; let n = 1; while (c < 5) { c = c + 1; n = n * 2; }; n",
                32,
            ),
            (
                "let c = 0; let n = 1; while (c < 5) { let d = 1; c = c + 1; n = n * 2; }; n",
                32,
            ),
            (
                "let a = match true { true => 1, false => 2 }; let b = 0; if a == 1 { b = 10; }; b",
                10,
            ),
            ("let a = 2; { let a = 3; }; a", 2),
            ("let a = 2; { let a = 3; a = 4; }; a", 2),
            ("let a = 2; { let a = 3; a = 4; } a", 2),
            ("let a = 2; { let a = 3; let b = 4; let c = 5; } a", 2),
            ("let a = 2; { a = 3; a = 4; }; a", 4),
        ];

        for (input, expected) in cases {
            let actual = Compiler::run_block(input.to_string()).unwrap();
            assert_eq!(actual, expected, "input: {}", input);
        }
    }

    #[test]
    fn test_compile_program() {
        let cases = vec![
            (
                r#"fun main() {
                let x = 1 + 2 * 4;
                let y = x + 2;

                return x + y;
            }"#,
                20,
            ),
            (
                r#"fun f() {
                return 10;
            }

            fun main() {
                return f();
            }"#,
                10,
            ),
            (
                r#"fun g(x, y, z) {
                return x - y - z;
            }

            fun main() {
                return g(10, 5, 2);
            }"#,
                3,
            ),
            (
                r#"fun g(x, y, z) {
                return x - y - z;
            }

            fun main() {
                let a = 10;
                let p = g(10, 5, 2);
                let b = 20;
                return b - p - a;
            }"#,
                7,
            ),
            (
                r#"
            fun f(a) {
                return match a == 1 {
                    true => 10,
                    false => 20,
                };
            }

            fun main() {
                return f(2);
            }"#,
                20,
            ),
            (
                r#"
            fun main() {
                let a = 2;

                if a == 1 {
                    let b = 10;
                    return b;
                } else if a == 2 {
                    let b = 20;
                    return b;
                } else {
                    let b = 30;
                    return b;
                }
            }"#,
                20,
            ),
        ];

        for (input, expected) in cases {
            let actual = Compiler::run(input.to_string()).unwrap();
            assert_eq!(actual, expected, "input: {}", input);
        }
    }
}
