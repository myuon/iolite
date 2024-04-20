use std::collections::HashMap;

use self::{
    ast::{Declaration, Module, Source, Span, Type},
    byte_code_emitter::{ByteCodeEmitter, ByteCodeEmitterError},
    ir_code_gen::IrCodeGeneratorError,
    lexer::LexerError,
    parser::ParseError,
    runtime::Runtime,
    typechecker::TypecheckerError,
    vm::Instruction,
    vm_code_gen::VmCodeGeneratorError,
};

pub mod ast;
pub mod byte_code_emitter;
pub mod ir;
pub mod ir_code_gen;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod typechecker;
pub mod vm;
pub mod vm_code_gen;

#[derive(Debug)]
pub enum CompilerError {
    LexerError(LexerError),
    ParseError(ParseError),
    TypecheckError(TypecheckerError),
    IrCodeGeneratorError(IrCodeGeneratorError),
    VmCodeGeneratorError(VmCodeGeneratorError),
    ByteCodeEmitterError(ByteCodeEmitterError),
}

pub struct Compiler {}

impl Compiler {
    pub fn find_position(input: &str, position: usize) -> (usize, usize) {
        let mut line = 0;
        let mut col = 0;
        for (i, c) in input.chars().enumerate() {
            if i == position {
                break;
            }

            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    pub fn find_line_and_column(input: &str, line: usize, col: usize) -> usize {
        let mut current_line = 0;
        let mut current_col = 0;
        let mut position = 0;

        for c in input.chars() {
            if current_line == line && current_col == col {
                break;
            }

            if c == '\n' {
                current_line += 1;
                current_col = 0;
            } else {
                current_col += 1;
            }

            position += 1;
        }

        position
    }

    pub fn run_lexer(input: String) -> Result<Vec<lexer::Token>, CompilerError> {
        let mut lexer = lexer::Lexer::new(input);
        Ok(lexer.run().map_err(CompilerError::LexerError)?)
    }

    pub fn parse(input: String) -> Result<Vec<Source<Declaration>>, CompilerError> {
        let mut lexer = lexer::Lexer::new(input.clone());
        let mut parser = parser::Parser::new(lexer.run().map_err(CompilerError::LexerError)?);
        let expr = match parser.decls() {
            Ok(expr) => expr,
            Err(err) => {
                match err.clone() {
                    ParseError::UnexpectedToken { got, .. } => {
                        let (line, col) = Self::find_position(&input, got.position);

                        eprintln!(
                            "Error at line {}, column {}\n\n{}\n{}^",
                            line,
                            col,
                            input.lines().collect::<Vec<_>>().join(" "),
                            " ".repeat(got.position)
                        );
                    }
                    _ => {}
                }

                return Err(CompilerError::ParseError(err));
            }
        };

        Ok(expr)
    }

    pub fn typecheck(
        module: &mut Module,
        input: &str,
    ) -> Result<HashMap<String, Source<Type>>, CompilerError> {
        let mut typechecker = typechecker::Typechecker::new();
        match typechecker.module(module) {
            Ok(_) => {}
            Err(err) => {
                match err.clone() {
                    TypecheckerError::TypeMismatch { span, .. } if span.start.is_some() => {
                        let (line, col) = Self::find_position(input, span.start.unwrap());
                        eprintln!(
                            "Error at line {}, column {} ({})",
                            line,
                            col,
                            span.start.unwrap()
                        );
                        eprintln!(
                            "{}\n{}^",
                            input.lines().collect::<Vec<_>>().join("\n"),
                            " ".repeat(col - 1)
                        );
                    }
                    _ => {}
                }

                return Err(CompilerError::TypecheckError(err));
            }
        }

        Ok(typechecker.types)
    }

    pub fn search_for_definition(
        module: &mut Module,
        position: usize,
    ) -> Result<Option<Span>, CompilerError> {
        let mut typechecker = typechecker::Typechecker::new();

        Ok(typechecker.search_for_definition(module, position))
    }

    pub fn ir_code_gen(
        block: Module,
        types: HashMap<String, Source<Type>>,
    ) -> Result<ir::IrModule, CompilerError> {
        let mut ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        ir_code_gen.set_types(types);

        let ir = ir_code_gen
            .module(block)
            .map_err(CompilerError::IrCodeGeneratorError)?;

        Ok(ir)
    }

    pub fn vm_code_gen(ir: ir::IrModule) -> Result<Vec<Instruction>, CompilerError> {
        let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();
        vm_code_gen
            .program(ir)
            .map_err(CompilerError::VmCodeGeneratorError)?;

        Ok(vm_code_gen.code)
    }

    pub fn byte_code_gen(code: Vec<Instruction>) -> Result<Vec<u8>, CompilerError> {
        let mut emitter = ByteCodeEmitter::new();
        emitter
            .exec(code)
            .map_err(CompilerError::ByteCodeEmitterError)?;

        Ok(emitter.buffer)
    }

    pub fn create_input(input: String) -> String {
        format!("{}\n{}", include_str!("./std.io"), input)
    }

    pub fn create_module(decls: Vec<Source<Declaration>>) -> Module {
        Module {
            name: "main".to_string(),
            declarations: decls,
        }
    }

    pub fn compile(input: String) -> Result<Vec<u8>, CompilerError> {
        let input = Self::create_input(input);

        let decls = Self::parse(input.clone())?;
        let mut module = Self::create_module(decls);
        let types = Self::typecheck(&mut module, &input)?;

        let ir = Self::ir_code_gen(module, types)?;
        let code = Self::vm_code_gen(ir)?;
        let binary = Self::byte_code_gen(code)?;

        Ok(binary)
    }

    pub fn run(input: String, print_stacks: bool) -> Result<i64, CompilerError> {
        let program = Self::compile(input)?;
        Self::run_vm(program, print_stacks)
    }

    pub fn run_vm(program: Vec<u8>, print_stacks: bool) -> Result<i64, CompilerError> {
        let mut runtime = Self::exec_vm(program, print_stacks)?;

        Ok(runtime.pop_i64())
    }

    fn exec_vm(program: Vec<u8>, print_stacks: bool) -> Result<Runtime, CompilerError> {
        let mut runtime = Runtime::new(1024, program);
        runtime.exec(print_stacks).unwrap();

        Ok(runtime)
    }
}

#[cfg(test)]
mod tests {
    use self::ir::Value;

    use super::*;

    #[test]
    fn test_compile_expr_as_int() {
        let cases = vec![
            ("1 + 3 * 4", 13),
            ("1 * 3 - 4", -1),
            ("4 * 3 - 2", 10),
            ("4 * (3 - 2)", 4),
            ("10 - 2 - 5", 3),
            ("match true { true => 2, false => 5 }", 2),
            ("match false { true => 2, false => 5 }", 5),
            (
                "match false { true => 2, false => match true { true => 3, false => 4 } }",
                3,
            ),
            (
                "match false { true => 2, false => match false { true => 3, false => 4 } }",
                4,
            ),
            ("365.2422 as int", 365),
            ("-200", -200),
            ("(-200).abs()", 200),
        ];

        for (input, expected) in cases {
            println!("====== {}", input);
            let actual =
                Compiler::run(format!("fun main() {{ return {}; }}", input), false).unwrap();
            let value = Value::from_u64(actual as u64);
            assert_eq!(value, Value::Int(expected), "input: {}", input);
        }
    }

    #[test]
    fn test_compile_expr_as_float() {
        let cases = vec![
            ("1.5", 1.5),
            ("1.5 + 1.5", 3.0),
            ("1.5 + 1.5 * 2.0", 4.5),
            ("-365.2422", -365.2422),
            ("(-365.2422).abs()", 365.2422),
            ("4 as float", 4.0),
        ];

        for (input, expected) in cases {
            println!("====== {}", input);
            let program = Compiler::compile(format!("fun main() {{ return {}; }}", input)).unwrap();
            let mut runtime = Compiler::exec_vm(program, false).unwrap();
            assert_eq!(
                runtime.pop_value(),
                Value::Float(expected),
                "input: {}",
                input
            );
        }
    }

    #[test]
    fn test_compile_expr_as_bool() {
        let cases = vec![
            ("false && false", false),
            ("true && false", false),
            ("true && true", true),
            ("false || false", false),
            ("true || false", true),
            ("true || true", true),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 <= 2", true),
            ("1 >= 2", false),
            ("1 == 2", false),
            ("2 == 2", true),
            ("1 != 10", true),
        ];

        for (input, expected) in cases {
            println!("====== {}", input);
            let program = Compiler::compile(format!("fun main() {{ return {}; }}", input)).unwrap();
            let mut runtime = Compiler::exec_vm(program, false).unwrap();
            assert_eq!(
                runtime.pop_value(),
                Value::Bool(expected),
                "input: {}",
                input
            );
        }
    }

    #[test]
    fn test_compile_program() {
        let cases = vec![
            (
                "fun main() { let x = 1 + 2 * 4; let y = x + 2; return y; }",
                11,
            ),
            ("fun main() { let x = 1; x = x + 2; return x; }", 3),
            (
                "fun main() { let c = 0; let n = 1; while (c < 5) { c = c + 1; n = n * 2; }; return n; }",
                32,
            ),
            (
                "fun main() { let c = 0; let n = 1; while (c < 5) { let d = 1; c = c + 1; n = n * 2; }; return n; }",
                32,
            ),
            (
                "fun main() { let a = match true { true => 1, false => 2 }; let b = 0; if a == 1 { b = 10; }; return b; }",
                10,
            ),
            (
                "fun main() { let a = match true { true => 1, false => 2 }; let b = 0; if a == 1 { b = 10; }; return b; }",
                10,
            ),
            ("fun main() { let a = 2; { let a = 3; }; return a; }", 2),
            ("fun main() { let a = 2; { let a = 3; a = 4; }; return a; }", 2),
            ("fun main() { let a = 2; { let a = 3; a = 4; } return a; }", 2),
            ("fun main() { let a = 2; { let a = 3; let b = 4; let c = 5; } return a; }", 2),
            ("fun main() { let a = 2; { a = 3; a = 4; }; return a; }", 4),
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
                r#"fun g(x: int, y: int, z: int) {
                return x - y - z;
            }

            fun main() {
                return g(10, 5, 2);
            }"#,
                3,
            ),
            (
                r#"fun g(x: int, y: int, z: int) {
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
            fun f(a: int) {
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
            (
                r#"
            let p = 10;

            fun f() {
                return p + 1;
            }

            fun main() {
                let x = f();
                p = 20;

                return x + p;
            }"#,
                31,
            ),
            (
                r#"
            fun main() {
                let arr = new[ptr[int]](10);
                arr.(0) = 10;
                arr.(1) = 20;
                arr.(2) = 30;

                return arr.(0) + arr.(1) + arr.(2);
            }"#,
                60,
            ),
            (
                r#"
            fun main() {
                let p = {
                    let x = 10;
                    let y = 20;
                    x + y
                };

                return p;
            }"#,
                30,
            ),
            (
                r#"
            let p = 10;

            fun main() {
                let arr = new[ptr[int]](10);
                arr.(0) = 2;
                arr.(1) = 3;
                arr.(2) = 4;

                return p + arr.(0);
            }"#,
                12,
            ),
            (
                r#"
            struct Point {
                x: int,
                y: int,
            }

            fun main() {
                let p = Point {
                    x: 20,
                    y: 7,
                };

                return p.x - p.y;
            }"#,
                13,
            ),
            (
                r#"
            struct Point {
                x: int,
                y: int,
            }

            fun main() {
                let p = Point {
                    x: 20,
                    y: 7,
                };

                p.x = 30;

                return p.x - p.y;
            }"#,
                23,
            ),
            (
                r#"
            let global = 0;
            fun f(x: int, y: int) {
                global = x + y;

                return nil;
            }

            fun main() {
                f(10, 20);

                return global;
            }"#,
                30,
            ),
            (
                r#"
            fun main() {
                let arr = new[array[int]](3);
                arr.(0) = 2;
                arr.(1) = 3;
                arr.(2) = 4;

                let arr2 = new[array[int]](3);
                arr2.(0) = 5;
                arr2.(1) = 6;
                arr2.(2) = 7;

                return arr.(1) + arr2.(1);
            }"#,
                9,
            ),
            (
                r#"
            fun main() {
                let arr = new[array[byte]](10);
                arr.(0) = 65 as byte;
                arr.(1) = 66 as byte;
                arr.(2) = 67 as byte;

                return arr.(2);
            }"#,
                Value::Byte(67).as_u64() as i64,
            ),
            (
                r#"
            fun main() {
                let arr = new[array[int]](10);
                arr.(0) = 2;
                arr.(1) = 3;
                arr.(2) = 4;

                return arr.length * arr.(0) - arr.(1) + arr.(2);
            }"#,
                21,
            ),
        ];

        for (input, expected) in cases {
            println!("====== {}", input);
            let actual = Compiler::run(input.to_string(), false).unwrap();
            assert_eq!(actual, expected, "input: {}", input);
        }
    }
}
