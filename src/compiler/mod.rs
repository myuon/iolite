use std::{
    collections::HashMap,
    io::BufWriter,
    rc::Rc,
    sync::{Arc, Mutex},
};

use anyhow::{anyhow, Result};
use ast::{AstItemType, TypeMap};

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
pub mod escape_resolver;
pub mod ir;
pub mod ir_code_gen;
pub mod lexer;
pub mod linker;
pub mod parser;
pub mod runtime;
pub mod typechecker;
pub mod vm;
pub mod vm_code_gen;

pub mod reporter {
    use super::ast::Span;

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

    pub fn find_line_and_column_with_input(input: &str, line: usize, col: usize) -> usize {
        let mut current_line = 0;
        let mut position = 0;

        for c in input.chars() {
            if current_line == line {
                break;
            }

            if c == '\n' {
                current_line += 1;
            }

            position += 1;
        }

        position + col
    }

    pub fn report_error_span(source: &str, span: &Span) {
        let (line, col) = find_position_with_input(source, span.start.unwrap_or(0));
        eprintln!(
            "Error at module {}, line {}, column {} ({})",
            span.module_name.clone().unwrap_or("<main>".to_string()),
            line,
            col,
            span.start.unwrap_or(0)
        );
        eprintln!(
            "{}\n{}^",
            source.lines().nth(line).unwrap_or(""),
            if col > 0 {
                " ".repeat(col - 1)
            } else {
                "".to_string()
            }
        );
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompilerError {
    #[error("Lexer error: {0}")]
    LexerError(#[from] LexerError),
    #[error("Parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("Typechecker error: {0}")]
    TypecheckError(#[from] TypecheckerError),
    #[error("IR code generator error: {0}")]
    IrCodeGeneratorError(#[from] IrCodeGeneratorError),
    #[error("VM code generator error: {0}")]
    VmCodeGeneratorError(#[from] VmCodeGeneratorError),
    #[error("Linker error: {0}")]
    LinkerError(#[from] LinkerError),
    #[error("Byte code emitter error: {0}")]
    ByteCodeEmitterError(#[from] ByteCodeEmitterError),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

impl CompilerError {
    pub fn as_span(&self) -> Span {
        match self {
            CompilerError::LexerError(err) => err.as_span(),
            CompilerError::ParseError(err) => err.as_span(),
            CompilerError::TypecheckError(err) => err.as_span(),
            _ => Span::unknown(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub source: String,
    pub module: Option<Module>,
    pub imports: Vec<String>,
    pub parsed_order: usize,
}

#[derive(Default)]
pub struct CompileOptions {
    pub no_emit: bool,
    pub testing_mode: bool,
}

pub struct Compiler {
    pub cwd: String,
    pub modules: HashMap<String, LoadedModule>,
    pub result_typecheck: Option<TypeMap>,
    pub result_ir: Option<IrProgram>,
    pub result_vm: Option<VmProgram>,
    pub result_link: Option<Vec<Instruction>>,
    pub result_codegen: Option<ByteCodeEmitter>,
    pub result_runtime: Option<Runtime>,
    pub result_extcall_table: Option<HashMap<String, usize>>,
    pub verbose: bool,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            cwd: "".to_string(),
            modules: HashMap::new(),
            result_typecheck: None,
            result_ir: None,
            result_vm: None,
            result_link: None,
            result_codegen: None,
            result_runtime: None,
            result_extcall_table: None,
            verbose: false,
        }
    }

    pub fn set_cwd(&mut self, cwd: String) {
        self.cwd = cwd;
    }

    pub fn verbose(&mut self, verbose: bool) {
        self.verbose = verbose;
    }

    pub fn find_position(&self, path: &str, position: usize) -> Result<(usize, usize)> {
        let module = self
            .modules
            .get(path)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", path))?;

        Ok(reporter::find_position_with_input(&module.source, position))
    }

    pub fn find_span(&self, span: &Span) -> Result<Option<((usize, usize), (usize, usize))>> {
        let module_name = span.module_name.as_ref().unwrap();
        let module = self
            .modules
            .get(module_name)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", module_name))?;

        match (span.start, span.end) {
            (Some(start), Some(end)) => {
                let start = reporter::find_position_with_input(&module.source, start);
                let end = reporter::find_position_with_input(&module.source, end);

                Ok(Some((start, end)))
            }
            _ => Ok(None),
        }
    }

    pub fn find_line_and_column(&self, path: &str, line: usize, col: usize) -> Result<usize> {
        let module = self
            .modules
            .get(path)
            .ok_or_else(|| anyhow!("Module {} not found in the compiler", path))?;

        Ok(reporter::find_line_and_column_with_input(
            &module.source,
            line,
            col,
        ))
    }

    fn lexer_run_reported(
        lexer: &mut lexer::Lexer,
        module: &mut LoadedModule,
    ) -> Result<Vec<Token>, CompilerError> {
        Ok(lexer.run().inspect_err(|err| {
            reporter::report_error_span(&module.source, &err.as_span());
        })?)
    }

    fn parse_decls_reported(
        parser: &mut parser::Parser,
        input: &str,
    ) -> Result<Vec<Source<Declaration>>, CompilerError> {
        Ok(parser.decls(None).inspect_err(|err| {
            reporter::report_error_span(&input, &err.as_span());
        })?)
    }

    fn typecheck_module_reported(
        typechecker: &mut typechecker::Typechecker,
        module: &mut LoadedModule,
    ) -> Result<(), CompilerError> {
        Ok(typechecker
            .module(module.module.as_mut().unwrap())
            .inspect_err(|err| {
                reporter::report_error_span(&module.source, &err.as_span());
            })?)
    }

    pub fn parse_module(input: String) -> Result<Module, CompilerError> {
        let decls = Self::parse_decls(input.clone())?;

        Ok(Module {
            name: "main".to_string(),
            declarations: decls,
        })
    }

    pub fn parse_decls(input: String) -> Result<Vec<Source<Declaration>>, CompilerError> {
        let mut lexer = lexer::Lexer::new("".to_string(), input.clone());
        let mut parser = parser::Parser::new("".to_string(), lexer.run()?, false);
        let expr = Self::parse_decls_reported(&mut parser, &input)?;

        Ok(expr)
    }

    pub fn parse_with_code(
        &mut self,
        path: String,
        source: String,
        allow_incomplete: bool,
    ) -> Result<(), CompilerError> {
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
            vec![
                Token {
                    lexeme: Lexeme::Import,
                    span: Span::unknown(),
                },
                Token {
                    lexeme: Lexeme::Ident("std".to_string()),
                    span: Span::unknown(),
                },
                Token {
                    lexeme: Lexeme::Semicolon,
                    span: Span::unknown(),
                },
            ]
        } else {
            vec![]
        };
        tokens.extend(Self::lexer_run_reported(
            &mut lexer,
            self.modules.get_mut(&path).unwrap(),
        )?);

        let mut parser = parser::Parser::new(path.clone(), tokens, allow_incomplete);
        let decls =
            Self::parse_decls_reported(&mut parser, &self.modules.get_mut(&path).unwrap().source)?;
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

            self.parse(path, allow_incomplete)?;
        }

        Ok(())
    }

    pub fn parse(&mut self, path: String, allow_incomplete: bool) -> Result<(), CompilerError> {
        let source = if path == "std" {
            include_str!("./std.io").to_string()
        } else if self.modules.contains_key(&path) {
            self.modules.get(&path).unwrap().source.clone()
        } else {
            let file_path = format!("{}/{}.io", self.cwd, path);
            std::fs::read_to_string(&file_path)?
        };

        self.parse_with_code(path, source, allow_incomplete)?;

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

    pub fn typecheck(&mut self, _path: String) -> Result<(), CompilerError> {
        let paths = self.pathes_in_imported_order();

        let mut typechecker = typechecker::Typechecker::new();
        for path in paths {
            let module = self.modules.get_mut(&path).unwrap();
            Self::typecheck_module_reported(&mut typechecker, module)?;
        }

        self.result_typecheck = Some(typechecker.types);

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
                Self::typecheck_module_reported(&mut typechecker, module)?;
            }
        }

        Ok(vec![])
    }

    pub fn completion(
        &mut self,
        path: String,
        position: usize,
    ) -> Result<Vec<(String, Type, AstItemType)>> {
        let paths = self.pathes_in_imported_order();

        let mut typechecker = typechecker::Typechecker::new();
        for path_target in paths {
            let module = self.modules.get_mut(&path_target).unwrap();

            if path_target == path {
                return Ok(typechecker
                    .completion(&mut module.module.as_mut().unwrap(), position)
                    .unwrap_or(vec![]));
            } else {
                Self::typecheck_module_reported(&mut typechecker, module)?;
            }
        }

        Ok(vec![])
    }

    pub fn ir_code_gen(&mut self, _path: String, testing_mode: bool) -> Result<()> {
        let paths = self.pathes_in_imported_order();
        let mut modules = vec![];
        let mut declared = vec![];

        let typemap = Rc::new(self.result_typecheck.clone().unwrap());

        for path in paths {
            let mut ir_code_gen = ir_code_gen::IrCodeGenerator::new();
            if testing_mode {
                ir_code_gen.enable_testing_mode();
            }
            ir_code_gen.set_types(typemap.clone());
            ir_code_gen.set_declared(declared.clone());

            let module = self.modules.get_mut(&path).unwrap();
            let ir = ir_code_gen.program(module.module.clone().unwrap())?;

            declared = ir_code_gen.declared.clone();

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

        self.result_ir = Some(IrProgram { modules: irs });

        Ok(())
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
                Self::typecheck_module_reported(&mut typechecker, module)?;
            }
        }

        Ok(None)
    }

    pub fn vm_code_gen(&mut self) -> Result<(), CompilerError> {
        let ir = self.result_ir.take().unwrap();
        let mut modules = vec![];
        let mut functions = vec![];
        let mut table = HashMap::new();

        for m in ir.modules {
            let module_name = m.name.clone();
            let data_section = m.data_section.clone();
            let global_section = m.global_section.clone();
            let init_function_name = m.init_function.clone().unwrap();

            let mut vm_code_gen = vm_code_gen::VmCodeGenerator::new(table.clone());
            vm_code_gen.functions = functions;
            vm_code_gen.program(m)?;

            functions = vm_code_gen.functions;

            table = vm_code_gen.extcall_table;

            modules.push(VmModule {
                name: module_name,
                instructions: vm_code_gen.code,
                data_section,
                global_section,
                init_function_name,
            })
        }

        self.result_vm = Some(VmProgram { modules });
        self.result_extcall_table = Some(table);

        Ok(())
    }

    pub fn byte_code_gen(&mut self) -> Result<(), CompilerError> {
        let mut emitter = ByteCodeEmitter::new();

        let code = self.result_link.take().unwrap();
        emitter.exec(code)?;

        self.result_codegen = Some(emitter);

        Ok(())
    }

    pub fn link(&mut self) -> Result<(), CompilerError> {
        let mut linker = Linker::new(self.verbose);

        let vm = self.result_vm.take().unwrap();
        let linked = linker.link(vm)?;

        self.result_link = Some(linked.clone());

        Ok(())
    }

    pub fn compile(
        &mut self,
        path: Option<String>,
        input: String,
        options: CompileOptions,
    ) -> Result<(), CompilerError> {
        let path = path.unwrap_or("main".to_string());

        self.parse_with_code(path.clone(), input.clone(), false)
            .unwrap();
        self.typecheck(path.clone())?;
        if options.no_emit {
            return Ok(());
        }
        self.ir_code_gen(path.clone(), options.testing_mode)
            .unwrap();
        self.vm_code_gen()?;
        self.link()?;
        self.byte_code_gen()?;

        Ok(())
    }

    pub fn execute(
        &mut self,
        print_stacks: bool,
        print_memory_store: bool,
        stdout: Option<Arc<Mutex<BufWriter<Vec<u8>>>>>,
    ) -> Result<(), CompilerError> {
        let program = self.result_codegen.take().unwrap().buffer;
        let extcall_table = self.result_extcall_table.take().unwrap();
        let mut runtime = Runtime::new(1024 * 1024, program, extcall_table);
        if let Some(stdout) = stdout {
            runtime.trap_stdout = Some(stdout);
        }
        runtime.exec(print_stacks, print_memory_store).unwrap();

        self.result_runtime = Some(runtime);

        Ok(())
    }

    pub fn pop_executed_result(&mut self) -> Result<i64, CompilerError> {
        Ok(self.result_runtime.as_mut().unwrap().pop_i64())
    }
}

#[cfg(test)]
mod tests {
    use self::ir::Value;

    use super::*;

    use rayon::prelude::*;

    #[test]
    fn test_compile_expr_as_int() -> Result<()> {
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
            ("10000 * 10000", 100000000),
            ("10000 * -10", -100000),
        ];

        cases
            .into_par_iter()
            .try_for_each(|(input, expected)| -> Result<_> {
                let mut compiler = Compiler::new();

                compiler.compile(
                    None,
                    format!("fun main() {{ return {}; }}", input),
                    CompileOptions {
                        ..Default::default()
                    },
                )?;
                compiler.execute(false, false, None)?;

                let actual = compiler.pop_executed_result()?;
                let value = Value::from_u64(actual as u64);
                assert_eq!(value, Value::Int(expected), "input: {}", input);

                Ok(())
            })?;

        Ok(())
    }

    #[test]
    fn test_compile_expr_as_float() -> Result<()> {
        let cases = vec![
            ("1.5", 1.5),
            ("1.5 + 1.5", 3.0),
            ("1.5 + 1.5 * 2.0", 4.5),
            ("-365.2422", -365.2422),
            ("(-365.2422).abs()", 365.2422),
            ("4 as float", 4.0),
            ("10000.0 * -10.0", -100000.0),
        ];

        cases
            .into_par_iter()
            .try_for_each(|(input, expected)| -> Result<_> {
                let mut compiler = Compiler::new();

                compiler
                    .compile(
                        None,
                        format!("fun main() {{ return {}; }}", input),
                        CompileOptions {
                            ..Default::default()
                        },
                    )
                    .unwrap();
                compiler.execute(false, false, None).unwrap();

                let runtime = compiler.result_runtime.as_mut().unwrap();
                assert_eq!(
                    runtime.pop_value(),
                    Value::Float(expected),
                    "input: {}",
                    input
                );

                Ok(())
            })?;

        Ok(())
    }

    #[test]
    fn test_compile_expr_as_bool() -> Result<()> {
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
            ("!true", false),
            ("!false", true),
        ];

        cases
            .into_par_iter()
            .try_for_each(|(input, expected)| -> Result<_> {
                let mut compiler = Compiler::new();

                compiler.compile(
                    None,
                    format!("fun main() {{ return {}; }}", input),
                    CompileOptions {
                        ..Default::default()
                    },
                )?;
                compiler.execute(false, false, None).unwrap();

                let runtime = compiler.result_runtime.as_mut().unwrap();
                assert_eq!(
                    runtime.pop_value(),
                    Value::Bool(expected),
                    "input: {}",
                    input
                );

                Ok(())
            })?;

        Ok(())
    }

    #[test]
    fn test_compile_program() -> Result<()> {
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

        cases
            .into_par_iter()
            .try_for_each(|(input, expected)| -> Result<_> {
                let mut compiler = Compiler::new();
                compiler.set_cwd(std::env::current_dir().unwrap().display().to_string());

                println!("====== {}", input);
                let path = "main".to_string();
                compiler.parse_with_code(path.clone(), input.to_string(), false)?;
                compiler.typecheck(path.clone())?;
                compiler.ir_code_gen(path.clone(), false)?;
                compiler.vm_code_gen()?;
                compiler.link()?;
                compiler.byte_code_gen()?;
                compiler.execute(false, false, None).unwrap();
                let actual = compiler.result_runtime.as_mut().unwrap().pop_i64();
                assert_eq!(actual, expected, "input: {}", input);

                Ok(())
            })?;

        Ok(())
    }

    #[test]
    fn test_compile_e2e() -> Result<()> {
        let paths = std::fs::read_dir("./tests/compiler")
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect::<Vec<_>>();

        paths
            .into_par_iter()
            .try_for_each(|dir_path| -> Result<_> {
                println!("{}", dir_path.to_str().unwrap());
                let mut compiler = Compiler::new();
                compiler.set_cwd(dir_path.display().to_string());

                let path = dir_path.join("main.io").display().to_string();
                let main = "main".to_string();

                let input = std::fs::read_to_string(path.clone()).unwrap();

                compiler.parse(main.clone(), false)?;
                compiler.typecheck(main.clone())?;

                let stdout = Arc::new(Mutex::new(BufWriter::new(vec![])));

                compiler.ir_code_gen(main.clone(), false)?;
                compiler.vm_code_gen()?;
                compiler.link()?;
                compiler.byte_code_gen()?;
                compiler.execute(false, false, Some(stdout.clone()))?;

                let result = compiler.result_runtime.as_mut().unwrap().pop_i64();

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

                Ok(())
            })?;

        Ok(())
    }
}
