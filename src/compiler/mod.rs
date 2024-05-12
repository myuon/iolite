use std::{
    collections::HashMap,
    io::BufWriter,
    sync::{Arc, Mutex},
};

use anyhow::{anyhow, Result};

use crate::compiler::lexer::Lexeme;

use self::{
    ast::{Declaration, Module, Source, Span, Type},
    byte_code_emitter::{ByteCodeEmitter, ByteCodeEmitterError},
    ir::IrProgram,
    ir_code_gen::IrCodeGeneratorError,
    lexer::{LexerError, Token},
    linker::{Linker, LinkerError},
    parser::ParseError,
    runtime::Runtime,
    typechecker::TypecheckerError,
    vm::{Instruction, VmModule, VmProgram},
    vm_code_gen::VmCodeGeneratorError,
};

pub mod ast;
pub mod byte_code_emitter;
pub mod ir;
pub mod ir_code_gen;
pub mod lexer;
pub mod linker;
pub mod parser;
pub mod runtime;
pub mod typechecker;
pub mod vm;
pub mod vm_code_gen;

#[derive(Debug, thiserror::Error)]
pub enum CompilerError {
    #[error("Lexer error: {0}")]
    LexerError(LexerError),
    #[error("Parse error: {0}")]
    ParseError(ParseError),
    #[error("Typechecker error: {0}")]
    TypecheckError(TypecheckerError),
    #[error("IR code generator error: {0}")]
    IrCodeGeneratorError(IrCodeGeneratorError),
    #[error("VM code generator error: {0}")]
    VmCodeGeneratorError(VmCodeGeneratorError),
    #[error("Linker error: {0}")]
    LinkerError(LinkerError),
    #[error("Byte code emitter error: {0}")]
    ByteCodeEmitterError(ByteCodeEmitterError),
}

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub source: String,
    pub module: Option<Module>,
    pub imports: Vec<String>,
    pub parsed_order: usize,
}

pub struct Compiler {
    pub cwd: String,
    pub modules: HashMap<String, LoadedModule>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            cwd: "".to_string(),
            modules: HashMap::new(),
        }
    }

    pub fn set_cwd(&mut self, cwd: String) {
        self.cwd = cwd;
    }

    pub fn find_position_with_input(input: &str, position: usize) -> (usize, usize) {
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

    pub fn find_position(&self, path: &str, position: usize) -> Result<(usize, usize)> {
        let module = self
            .modules
            .get(path)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", path))?;

        Ok(Self::find_position_with_input(&module.source, position))
    }

    pub fn find_span(&self, span: &Span) -> Result<Option<((usize, usize), (usize, usize))>> {
        let module_name = span.module_name.as_ref().unwrap();
        let module = self
            .modules
            .get(module_name)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", module_name))?;

        match (span.start, span.end) {
            (Some(start), Some(end)) => {
                let start = Self::find_position_with_input(&module.source, start);
                let end = Self::find_position_with_input(&module.source, end);

                Ok(Some((start, end)))
            }
            _ => Ok(None),
        }
    }

    pub fn find_line_and_column_with_input(input: &str, line: usize, col: usize) -> usize {
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

    pub fn find_line_and_column(&self, path: &str, line: usize, col: usize) -> Result<usize> {
        let module = self
            .modules
            .get(path)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", path))?;

        Ok(Self::find_line_and_column_with_input(
            &module.source,
            line,
            col,
        ))
    }

    pub fn parse_module(input: String) -> Result<Module, CompilerError> {
        let decls = Self::parse_decls(input.clone())?;

        Ok(Self::create_module(decls))
    }

    pub fn parse_decls(input: String) -> Result<Vec<Source<Declaration>>, CompilerError> {
        let mut lexer = lexer::Lexer::new("".to_string(), input.clone());
        let mut parser = parser::Parser::new(
            "".to_string(),
            lexer.run().map_err(CompilerError::LexerError)?,
        );
        let expr = match parser.decls() {
            Ok(expr) => expr,
            Err(err) => {
                match err.clone() {
                    ParseError::UnexpectedToken { got, .. } => {
                        let (line, col) = Self::find_position_with_input(&input, got.position);

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

    fn tokens_import_std() -> Vec<Token> {
        vec![
            Token {
                lexeme: Lexeme::Import,
                position: 0,
                span: Span::unknown(),
            },
            Token {
                lexeme: Lexeme::Ident("std".to_string()),
                position: 0,
                span: Span::unknown(),
            },
            Token {
                lexeme: Lexeme::Semicolon,
                position: 0,
                span: Span::unknown(),
            },
        ]
    }

    pub fn parse_with_code(&mut self, path: String, source: String) -> Result<()> {
        self.modules.insert(
            path.clone(),
            LoadedModule {
                source: source.clone(),
                module: None,
                imports: vec![],
                parsed_order: self.modules.len(),
            },
        );

        let mut lexer = lexer::Lexer::new(path.clone(), source);

        let mut tokens = if path != "std" {
            Self::tokens_import_std()
        } else {
            vec![]
        };
        tokens.extend(lexer.run().map_err(CompilerError::LexerError)?);

        let mut parser = parser::Parser::new(path.clone(), tokens);
        let decls = parser.decls().map_err(CompilerError::ParseError)?;
        let module = Module {
            name: path.clone(),
            declarations: decls,
        };

        let m = self.modules.get_mut(&path).unwrap();
        m.module = Some(module.clone());
        m.imports = parser.imports.clone();

        for path in parser.imports {
            if self.modules.contains_key(&path) {
                continue;
            }

            self.parse(path)?;
        }

        Ok(())
    }

    pub fn parse(&mut self, path: String) -> Result<()> {
        let source = if path == "std" {
            include_str!("./std.io").to_string()
        } else {
            let file_path = format!("{}/{}.io", self.cwd, path);
            std::fs::read_to_string(&file_path)
                .map_err(|err| anyhow!("Failed to read file {}.io: {}", file_path, err))?
        };

        self.parse_with_code(path, source)?;

        Ok(())
    }

    pub fn pathes_in_imported_order(&self) -> Vec<String> {
        let mut paths = self.modules.keys().cloned().collect::<Vec<_>>();
        let mut result = vec![];

        while let Some(path) = paths.pop() {
            if result.contains(&path) {
                continue;
            }

            let module = self.modules.get(&path).unwrap();
            let imports = module.imports.clone();

            let mut has_imports = false;
            for import in imports {
                if !result.contains(&import) {
                    has_imports = true;
                    paths.push(path.clone());
                    paths.push(import);
                    break;
                }
            }

            if !has_imports {
                result.push(path);
            }
        }

        result
    }

    pub fn typecheck(&mut self, _path: String) -> Result<()> {
        let paths = self.pathes_in_imported_order();

        let mut typechecker = typechecker::Typechecker::new();
        for path in paths {
            let module = self.modules.get_mut(&path).unwrap();

            Self::typecheck_method(
                &mut typechecker,
                &mut module.module.as_mut().unwrap(),
                &module.source,
            )?;
        }

        Ok(())
    }

    pub fn inlay_hints(&mut self, path: String) -> Result<Vec<(Span, Type)>> {
        let paths = self.pathes_in_imported_order();

        let mut typechecker = typechecker::Typechecker::new();
        for path_target in paths {
            let module = self.modules.get_mut(&path_target).unwrap();

            if path_target == path {
                return Ok(typechecker
                    .inlay_hints(&mut module.module.as_mut().unwrap())
                    .unwrap_or(vec![]));
            } else {
                Self::typecheck_method(
                    &mut typechecker,
                    &mut module.module.as_mut().unwrap(),
                    &module.source,
                )?;
            }
        }

        Ok(vec![])
    }

    pub fn ir_code_gen(&mut self, path: String) -> Result<IrProgram> {
        let paths = self.pathes_in_imported_order();
        let mut modules = vec![];
        let mut init_functions = vec![];
        for path in paths {
            let module = self.modules.get_mut(&path).unwrap();

            let ir = Self::ir_code_gen_module(
                module.module.clone().unwrap(),
                HashMap::new(),
                init_functions.clone(),
            )?;
            if let Some(name) = ir.init_function.clone() {
                init_functions.push(name);
            }

            modules.push(ir);
        }

        let modules_map = modules
            .iter()
            .map(|module| (module.name.clone(), module.clone()))
            .collect::<HashMap<_, _>>();

        let mut irs = vec![];
        for path in self.pathes_in_imported_order() {
            let module = modules_map.get(&path).unwrap().clone();
            irs.push(module);
        }

        Ok(IrProgram { modules: irs })
    }

    fn typecheck_method(
        typechecker: &mut typechecker::Typechecker,
        module: &mut Module,
        input: &str,
    ) -> Result<(), CompilerError> {
        match typechecker.module(module) {
            Ok(_) => {}
            Err(err) => {
                match err.clone() {
                    TypecheckerError::TypeMismatch { span, .. } if span.start.is_some() => {
                        let (line, col) =
                            Self::find_position_with_input(input, span.start.unwrap());
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

        Ok(())
    }

    pub fn typecheck_module(
        module: &mut Module,
        input: &str,
    ) -> Result<HashMap<String, Source<Type>>, CompilerError> {
        let mut typechecker = typechecker::Typechecker::new();
        match typechecker.module(module) {
            Ok(_) => {}
            Err(err) => {
                match err.clone() {
                    TypecheckerError::TypeMismatch { span, .. } if span.start.is_some() => {
                        let (line, col) =
                            Self::find_position_with_input(input, span.start.unwrap());
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

    pub fn search_for_definition(&mut self, path: String, position: usize) -> Result<Option<Span>> {
        let mut typechecker = typechecker::Typechecker::new();

        for path_target in self.pathes_in_imported_order() {
            let module = self.modules.get_mut(&path_target).unwrap();

            if path_target == path {
                return Ok(
                    typechecker.search_for_definition(module.module.as_mut().unwrap(), position)
                );
            } else {
                typechecker.module(module.module.as_mut().unwrap())?;
            }
        }

        Ok(None)
    }

    pub fn infer_type_at(&mut self, path: String, position: usize) -> Result<Option<Type>> {
        let paths = self.pathes_in_imported_order();

        let mut typechecker = typechecker::Typechecker::new();
        for path_target in paths {
            let module = self.modules.get_mut(&path_target).unwrap();

            if path_target == path {
                return Ok(
                    typechecker.infer_type_at(&mut module.module.as_mut().unwrap(), position)
                );
            } else {
                Self::typecheck_method(
                    &mut typechecker,
                    &mut module.module.as_mut().unwrap(),
                    &module.source,
                )?;
            }
        }

        Ok(None)
    }

    pub fn ir_code_gen_module(
        block: Module,
        types: HashMap<String, Source<Type>>,
        init_functions: Vec<String>,
    ) -> Result<ir::IrModule, CompilerError> {
        let mut ir_code_gen = ir_code_gen::IrCodeGenerator::new();
        ir_code_gen.set_types(types);

        let ir = ir_code_gen
            .module(block, init_functions)
            .map_err(CompilerError::IrCodeGeneratorError)?;

        Ok(ir)
    }

    pub fn vm_code_gen(ir: IrProgram) -> Result<VmProgram, CompilerError> {
        let mut modules = vec![];

        for m in ir.modules {
            let module_name = m.name.clone();
            let data_section = m.data_section.clone();
            let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new();
            vm_code_gen
                .program(m)
                .map_err(CompilerError::VmCodeGeneratorError)?;

            modules.push(VmModule {
                name: module_name,
                instructions: vm_code_gen.code,
                data_section,
            })
        }

        Ok(VmProgram { modules })
    }

    pub fn byte_code_gen(code: Vec<Instruction>) -> Result<Vec<u8>, CompilerError> {
        let mut emitter = ByteCodeEmitter::new();
        emitter
            .exec(code)
            .map_err(CompilerError::ByteCodeEmitterError)?;

        Ok(emitter.buffer)
    }

    pub fn link(vm: VmProgram) -> Result<Vec<Instruction>, CompilerError> {
        let mut linker = Linker::new();

        linker.link(vm).map_err(CompilerError::LinkerError)
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

    pub fn compile_with_input(input: String) -> Result<Vec<u8>, CompilerError> {
        let input = Self::create_input(input);

        Self::compile_bundled(input)
    }

    pub fn compile(&mut self, path: String) -> Result<Vec<u8>> {
        let module = self
            .modules
            .get(&path)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", path))?
            .clone();

        self.parse(path.clone())?;
        self.typecheck(path.clone())?;

        let ir = Self::ir_code_gen_module(module.module.clone().unwrap(), HashMap::new(), vec![])?;
        let program = Self::vm_code_gen(IrProgram { modules: vec![ir] })?;
        let linked = Self::link(program)?;
        let binary = Self::byte_code_gen(linked)?;

        Ok(binary)
    }

    pub fn compile_bundled(input: String) -> Result<Vec<u8>, CompilerError> {
        let decls = Self::parse_decls(input.clone())?;
        let mut module = Self::create_module(decls);
        let types = Self::typecheck_module(&mut module, &input)?;

        let ir = Self::ir_code_gen_module(module, types, vec![])?;
        let program = Self::vm_code_gen(IrProgram { modules: vec![ir] })?;
        let linked = Self::link(program)?;
        let binary = Self::byte_code_gen(linked)?;

        Ok(binary)
    }

    pub fn run_input(input: String, print_stacks: bool) -> Result<i64, CompilerError> {
        let program = Self::compile_with_input(input)?;
        Self::run_vm(program, print_stacks)
    }

    pub fn run(&self, path: String, print_stacks: bool) -> Result<i64> {
        let module = self
            .modules
            .get(&path)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", path))?;

        let program = Self::compile_with_input(module.source.clone())?;

        Ok(Self::run_vm(program, print_stacks)?)
    }

    pub fn run_vm(program: Vec<u8>, print_stacks: bool) -> Result<i64, CompilerError> {
        let mut runtime = Self::exec_vm(program, print_stacks)?;

        Ok(runtime.pop_i64())
    }

    pub fn run_vm_with_io_trap(
        program: Vec<u8>,
        print_stacks: bool,
        stdout: Arc<Mutex<BufWriter<Vec<u8>>>>,
    ) -> Result<i64, CompilerError> {
        let mut runtime = Runtime::new(1024, program);
        runtime.trap_stdout = Some(stdout);
        runtime.exec(print_stacks).unwrap();

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
                Compiler::run_input(format!("fun main() {{ return {}; }}", input), false).unwrap();
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
            let program =
                Compiler::compile_with_input(format!("fun main() {{ return {}; }}", input))
                    .unwrap();
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
            let program =
                Compiler::compile_with_input(format!("fun main() {{ return {}; }}", input))
                    .unwrap();
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
            (
                r#"
            fun main() {
                let s = "Hello, World!";

                return s.length;
            }"#,
                Value::Int(13).as_u64() as i64,
            ),
            (
                r#"
            let global1 = 10;
            let global2 = 20;
            
            fun main() {
                let s = "Hello, World!";

                return global1 + global2 - s.length;
            }"#,
                Value::Int(17).as_u64() as i64,
            ),
            (
                r#"
            fun main() {
                let t = "ABC";

                return t.(1);
            }"#,
                Value::Byte(66).as_u64() as i64,
            ),
        ];

        for (input, expected) in cases {
            println!("====== {}", input);
            let actual = Compiler::run_input(input.to_string(), false).unwrap();
            assert_eq!(actual, expected, "input: {}", input);
        }
    }

    #[test]
    fn test_compile_e2e() -> Result<()> {
        let paths = std::fs::read_dir("./tests/compiler")
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect::<Vec<_>>();

        for dir_path in paths {
            let mut compiler = Compiler::new();
            compiler.set_cwd(dir_path.display().to_string());

            let path = dir_path.join("main.io").display().to_string();
            let main = "main".to_string();

            let input = std::fs::read_to_string(path.clone()).unwrap();

            compiler.parse(main.clone())?;
            compiler.typecheck(main.clone())?;

            let ir = compiler.ir_code_gen(main.clone())?;
            let code = Compiler::vm_code_gen(ir)?;
            let linked = Compiler::link(code)?;
            let binary = Compiler::byte_code_gen(linked)?;

            let stdout = Arc::new(Mutex::new(BufWriter::new(vec![])));
            let result = Compiler::run_vm_with_io_trap(binary, false, stdout.clone())?;

            let expected = dir_path.join("result.test").display().to_string();
            if std::path::Path::new(&expected).exists() {
                let expected = std::fs::read_to_string(expected).unwrap();
                let expected = expected.trim().parse::<i64>().unwrap();

                assert_eq!(result, expected, "input: {}", input);
            }

            let expected_stdout = dir_path.join("stdout.test").display().to_string();
            if std::path::Path::new(&expected_stdout).exists() {
                let expected = std::fs::read_to_string(expected_stdout).unwrap();

                let got = String::from_utf8(stdout.lock().unwrap().buffer().to_vec()).unwrap();
                assert_eq!(got, expected, "input: {}", input);
            }
        }

        Ok(())
    }
}
