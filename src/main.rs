use std::{
    error::Error,
    io::Read,
    sync::{Arc, Mutex},
};

use base64::prelude::*;
use clap::{Parser, Subcommand};
use compiler::{lexer::Lexeme, runtime::Runtime, CompilerError};
use dap::{
    server::{Dap, DapServer},
    ConfigurationDoneResponse, InitializedEvent, LaunchRequestArguments, LaunchResponse,
    NextResponse, ProtocolMessageEventBuilder, ProtocolMessageEventKind, ProtocolMessageRequest,
    ProtocolMessageResponse, ProtocolMessageResponseBuilder, ReadMemoryResponse, Scope,
    ScopesArguments, ScopesResponse, SetExceptionBreakpointsResponse, SourceResponse, StackFrame,
    StackTraceResponse, StoppedEvent, StoppedEventReason, Thread, ThreadsResponse,
};
use lsp::{
    server::{Lsp, LspServer},
    Diagnostic, DiagnosticOptions, DocumentDiagnosticReportKind, FullDocumentDiagnosticReport,
    InitializeResult, Position, Range, RelatedFullDocumentDiagnosticReport, RpcMessageRequest,
    RpcMessageResponse, SemanticTokenTypes, SemanticTokens, SemanticTokensData,
    SemanticTokensLegend, SemanticTokensOptions, ServerCapabilities, TextDocumentSyncKind,
};
use server::{FutureResult, ServerProcess};

use crate::{
    compiler::{ast::Module, vm::Instruction},
    dap::{
        Capabilities, InitializeResponseBody, OutputEvent, OutputEventKind, Source, Variable,
        VariablesArguments, VariablesResponse,
    },
    lsp::{Location, TextDocumentPositionParams},
};

mod compiler;
mod dap;
mod lsp;
mod net;
mod server;

#[derive(Parser, Debug)]
#[clap(name = "iolite")]
struct Cli {
    #[clap(subcommand)]
    command: CliCommands,
}

#[derive(Debug, Subcommand)]
enum CliCommands {
    Compile {
        input: Option<String>,

        #[clap(long)]
        stdin: bool,
    },
    Run {
        input: Option<String>,

        #[clap(long)]
        stdin: bool,
        #[clap(long = "print-stacks")]
        print_stacks: bool,
    },
    Lsp {},
    Dap {},
}

fn compile(input: String) -> Vec<u8> {
    let decls = compiler::Compiler::parse(compiler::Compiler::create_input(input.clone())).unwrap();
    let mut module = Module {
        name: "main".to_string(),
        declarations: decls,
    };
    let types = compiler::Compiler::typecheck(&mut module, &input).unwrap();

    let ir = compiler::Compiler::ir_code_gen(module, types).unwrap();
    println!("= IR_CODE_GEN");
    println!("{:#?}", ir);

    let code = compiler::Compiler::vm_code_gen(ir).unwrap();
    println!("= VM_CODE_GEN");

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

    let binary = compiler::Compiler::byte_code_gen(code).unwrap();
    println!("= BYTE_CODE_GEN");
    println!("{:x?}", binary);

    binary
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        CliCommands::Compile { input, stdin } => {
            let input = if stdin {
                let mut buf = Vec::new();
                std::io::stdin().read_to_end(&mut buf).unwrap();

                String::from_utf8(buf).unwrap()
            } else {
                input.unwrap()
            };

            compile(input);
        }
        CliCommands::Run {
            input,
            stdin,
            print_stacks,
        } => {
            let input = if stdin {
                let mut buf = Vec::new();
                std::io::stdin().read_to_end(&mut buf).unwrap();

                String::from_utf8(buf).unwrap()
            } else {
                input.unwrap()
            };
            let result = compiler::Compiler::run(input, print_stacks).unwrap();

            println!("result: {}", result);
        }
        CliCommands::Lsp {} => {
            Lsp::new(LspImpl).start((), 3030).await.unwrap();
        }
        CliCommands::Dap {} => {
            Dap::new(DapImpl)
                .start(
                    DapContext(Arc::new(Mutex::new(Runtime::new(1024, vec![])))),
                    3031,
                )
                .await
                .unwrap();
        }
    }
}

#[derive(Clone)]
struct LspImpl;

async fn lsp_handler(
    req: RpcMessageRequest,
) -> Result<Option<RpcMessageResponse>, Box<dyn Error + Sync + Send + 'static>> {
    let token_types = vec![SemanticTokenTypes::Keyword, SemanticTokenTypes::Comment];

    match req.method.as_str() {
        "initialize" => {
            println!(
                "Initialize! {}",
                serde_json::to_string(&req.params).unwrap()
            );

            Ok(Some(RpcMessageResponse {
                jsonrpc: "2.0".to_string(),
                id: req.id,
                result: serde_json::to_value(&InitializeResult {
                    capabilities: ServerCapabilities {
                        text_document_sync: Some(TextDocumentSyncKind::Full),
                        declaration_provider: Some(true),
                        definition_provider: Some(true),
                        semantic_tokens_provider: Some(SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types,
                                token_modifiers: vec![],
                            },
                            range: None,
                            full: Some(true),
                        }),
                        diagnostic_provider: Some(DiagnosticOptions {
                            inter_file_dependencies: true,
                            workspace_diagnostics: true,
                        }),
                    },
                })?,
            }))
        }
        "initialized" => {
            println!("Initialized!");

            Ok(None)
        }
        // it seems not working
        "textDocument/semanticTokens/full" => {
            let params = serde_json::from_value::<lsp::SemanticTokensParams>(req.params.clone())?;
            let filepath = params.text_document.uri.as_filepath().unwrap();
            let content = std::fs::read_to_string(filepath)?;

            let tokens = compiler::Compiler::run_lexer(content.clone()).unwrap();
            let mut token_data = vec![];
            let mut prev = (0, 0);
            for token in tokens {
                match &token.lexeme {
                    Lexeme::Let => match (token.span.start, token.span.end) {
                        (Some(start), Some(end)) => {
                            let (line, col) = compiler::Compiler::find_position(&content, start);

                            token_data.push(SemanticTokensData {
                                line_delta: line - prev.0,
                                char_delta: if line == prev.0 { col - prev.1 } else { col },
                                ty: SemanticTokenTypes::Keyword,
                                length: end - start,
                            });

                            prev = (line, col);
                        }
                        _ => {}
                    },
                    Lexeme::Comment(_) => match (token.span.start, token.span.end) {
                        (Some(start), Some(end)) => {
                            let (line, col) = compiler::Compiler::find_position(&content, start);

                            token_data.push(SemanticTokensData {
                                line_delta: line - prev.0,
                                char_delta: if line == prev.0 { col - prev.1 } else { col },
                                ty: SemanticTokenTypes::Comment,
                                length: end - start,
                            });

                            prev = (line, col);
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }

            Ok(Some(RpcMessageResponse {
                jsonrpc: "2.0".to_string(),
                id: req.id,
                result: serde_json::to_value(&SemanticTokens::encode_data(
                    token_data,
                    token_types,
                ))?,
            }))
        }
        "textDocument/diagnostic" => {
            let params = serde_json::from_value::<lsp::SemanticTokensParams>(req.params.clone())?;

            let input = compiler::Compiler::create_input(
                std::fs::read_to_string(params.text_document.uri.as_filepath().unwrap()).unwrap(),
            );
            let parsed = match compiler::Compiler::parse(input.clone()) {
                Ok(parsed) => parsed,
                Err(CompilerError::ParseError(err)) => {
                    return Ok(Some(RpcMessageResponse {
                        jsonrpc: "2.0".to_string(),
                        id: req.id,
                        result: serde_json::to_value(RelatedFullDocumentDiagnosticReport {
                            related_documents: Some(
                                vec![(
                                    params.text_document.uri,
                                    FullDocumentDiagnosticReport {
                                        kind: DocumentDiagnosticReportKind::Full,
                                        items: vec![Diagnostic {
                                            range: Range {
                                                start: Position {
                                                    line: 0,
                                                    character: 0,
                                                },
                                                end: Position {
                                                    line: 1,
                                                    character: 1,
                                                },
                                            },
                                            message: format!("{:?}", err),
                                        }],
                                    },
                                )]
                                .into_iter()
                                .collect(),
                            ),
                        })?,
                    }));
                }
                _ => todo!(),
            };
            let mut module = compiler::Compiler::create_module(parsed);
            match compiler::Compiler::typecheck(&mut module, &input) {
                Ok(_) => {}
                Err(CompilerError::TypecheckError(err)) => {
                    return Ok(Some(RpcMessageResponse {
                        jsonrpc: "2.0".to_string(),
                        id: req.id,
                        result: serde_json::to_value(RelatedFullDocumentDiagnosticReport {
                            related_documents: Some(
                                vec![(
                                    params.text_document.uri,
                                    FullDocumentDiagnosticReport {
                                        kind: DocumentDiagnosticReportKind::Full,
                                        items: vec![Diagnostic {
                                            range: Range {
                                                start: Position {
                                                    line: 0,
                                                    character: 0,
                                                },
                                                end: Position {
                                                    line: 1,
                                                    character: 1,
                                                },
                                            },
                                            message: format!("{:?}", err),
                                        }],
                                    },
                                )]
                                .into_iter()
                                .collect(),
                            ),
                        })?,
                    }));
                }
                _ => todo!(),
            };

            Ok(Some(RpcMessageResponse {
                jsonrpc: "2.0".to_string(),
                id: req.id,
                result: serde_json::to_value(RelatedFullDocumentDiagnosticReport {
                    related_documents: Some(
                        vec![(
                            params.text_document.uri,
                            FullDocumentDiagnosticReport {
                                kind: DocumentDiagnosticReportKind::Full,
                                items: vec![],
                            },
                        )]
                        .into_iter()
                        .collect(),
                    ),
                })?,
            }))
        }
        "textDocument/declaration" => {
            println!(
                "Declaration! {}",
                serde_json::to_string(&req.params).unwrap()
            );

            Ok(None)
        }
        "textDocument/definition" => {
            let params = serde_json::from_value::<TextDocumentPositionParams>(req.params.clone())?;
            let input =
                std::fs::read_to_string(params.text_document.uri.as_filepath().unwrap()).unwrap();
            let parsed = compiler::Compiler::parse(input.clone()).unwrap();
            let mut module = compiler::Compiler::create_module(parsed);

            let position = compiler::Compiler::find_line_and_column(
                &input,
                params.position.line,
                params.position.character,
            );
            let def_position =
                compiler::Compiler::search_for_definition(&mut module, position).unwrap();

            if let Some(def_position) = def_position {
                let start_position =
                    compiler::Compiler::find_position(&input, def_position.start.unwrap());
                let end_position =
                    compiler::Compiler::find_position(&input, def_position.end.unwrap());

                return Ok(Some(RpcMessageResponse {
                    jsonrpc: "2.0".to_string(),
                    id: req.id,
                    result: serde_json::to_value(Location {
                        uri: params.text_document.uri,
                        range: Range {
                            start: Position {
                                line: start_position.0,
                                character: start_position.1,
                            },
                            end: Position {
                                line: end_position.0,
                                character: end_position.1,
                            },
                        },
                    })?,
                }));
            }

            Ok(None)
        }
        _ => Ok(None),
    }
}

impl LspServer for LspImpl {
    fn handle_request(req: RpcMessageRequest) -> FutureResult<Option<RpcMessageResponse>> {
        Box::pin(lsp_handler(req))
    }
}

#[derive(Clone)]
struct DapImpl;

#[derive(Clone)]
struct DapContext(Arc<Mutex<Runtime>>);

async fn dap_handler(
    ctx: DapContext,
    req: ProtocolMessageRequest,
) -> Result<Vec<ProtocolMessageResponse>, Box<dyn Error + Sync + Send>> {
    const MAIN_THREAD_ID: usize = 1;

    match req.command.as_str() {
        "initialize" => Ok(vec![
            ProtocolMessageResponseBuilder {
                body: serde_json::to_value(InitializeResponseBody(Capabilities {
                    supports_configuration_done_request: Some(true),
                    supports_single_thread_execution_requests: Some(true),
                    supports_function_breakpoints: Some(false),
                    supports_conditional_breakpoints: Some(false),
                    supports_hit_conditional_breakpoints: Some(true),
                    supports_read_memory_request: Some(true),
                    supports_memory_event: Some(false),
                    supports_disassemble_request: Some(true),
                }))?,
            }
            .build(&req),
            ProtocolMessageEventBuilder {
                body: serde_json::to_value(InitializedEvent {})?,
                event: ProtocolMessageEventKind::Initialized,
            }
            .build(),
        ]),
        "launch" => {
            let arg = serde_json::from_value::<LaunchRequestArguments>(req.arguments.clone())?;
            let content = std::fs::read_to_string(arg.source_file.unwrap())?;
            let program = compiler::Compiler::compile(content.clone()).unwrap();

            ctx.0.lock().unwrap().init(1024, program, content);

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(LaunchResponse {})?,
            }
            .build(&req)])
        }
        "setExceptionBreakpoints" => Ok(vec![ProtocolMessageResponseBuilder {
            body: serde_json::to_value(SetExceptionBreakpointsResponse { breakpoints: None })?,
        }
        .build(&req)]),
        "threads" => Ok(vec![ProtocolMessageResponseBuilder {
            body: serde_json::to_value(ThreadsResponse {
                threads: vec![Thread {
                    id: MAIN_THREAD_ID,
                    name: "main".to_string(),
                }],
            })?,
        }
        .build(&req)]),
        "configurationDone" => Ok(vec![
            ProtocolMessageResponseBuilder {
                body: serde_json::to_value(ConfigurationDoneResponse {})?,
            }
            .build(&req),
            ProtocolMessageEventBuilder {
                body: serde_json::to_value(StoppedEvent {
                    reason: StoppedEventReason::Pause,
                    description: None,
                    thread_id: Some(MAIN_THREAD_ID),
                    preserve_focus_hint: None,
                    text: None,
                    all_threads_stopped: None,
                    hit_breakpoint_ids: None,
                })?,
                event: ProtocolMessageEventKind::Stopped,
            }
            .build(),
        ]),
        "source" => {
            let runtime = ctx.0.lock().unwrap();

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(SourceResponse {
                    content: runtime.source_code.clone(),
                    mime_type: None,
                })?,
            }
            .build(&req)])
        }
        "stackTrace" => {
            let runtime = ctx.0.lock().unwrap();
            let frames = runtime.get_stack_frames();

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(StackTraceResponse {
                    total_frames: frames.len(),
                    stack_frames: frames
                        .into_iter()
                        .enumerate()
                        .map(|(i, frame)| StackFrame {
                            id: i,
                            name: format!("<stackframe:#{:x?}>", frame),
                            source: None,
                            line: 0,
                            column: 0,
                            end_line: None,
                            end_column: None,
                            can_restart: None,
                            module_id: None,
                            presentation_hint: None,
                        })
                        .collect(),
                })?,
            }
            .build(&req)])
        }
        "scopes" => {
            let arg = serde_json::from_value::<ScopesArguments>(req.arguments.clone())?;
            let runtime = ctx.0.lock().unwrap();
            let values = runtime.get_stack_values(arg.frame_id);

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(ScopesResponse {
                    scopes: vec![
                        Scope {
                            name: "Runtime Values".to_string(),
                            presentation_hint: None,
                            variables_reference: 1,
                            named_variables: Some(3),
                            indexed_variables: None,
                            expensive: false,
                            source: None,
                            line: None,
                            column: None,
                            end_line: None,
                            end_column: None,
                        },
                        Scope {
                            name: "Locals".to_string(),
                            presentation_hint: None,
                            variables_reference: arg.frame_id + 2,
                            named_variables: Some(values.len()),
                            indexed_variables: None,
                            expensive: false,
                            source: None,
                            line: None,
                            column: None,
                            end_line: None,
                            end_column: None,
                        },
                    ],
                })?,
            }
            .build(&req)])
        }
        "variables" => {
            let arg = serde_json::from_value::<VariablesArguments>(req.arguments.clone())?;
            let runtime = ctx.0.lock().unwrap();

            match arg.variables_reference {
                1 => Ok(vec![ProtocolMessageResponseBuilder {
                    body: serde_json::to_value(VariablesResponse {
                        variables: vec![
                            Variable {
                                name: "pc".to_string(),
                                value: runtime.pc.to_string(),
                                type_: Some("int".to_string()),
                                presentation_hint: None,
                                evaluate_name: None,
                                variables_reference: 0,
                                named_variables: None,
                                indexed_variables: None,
                                memory_reference: None,
                            },
                            Variable {
                                name: "sp".to_string(),
                                value: runtime.sp.to_string(),
                                type_: Some("int".to_string()),
                                presentation_hint: None,
                                evaluate_name: None,
                                variables_reference: 0,
                                named_variables: None,
                                indexed_variables: None,
                                memory_reference: None,
                            },
                            Variable {
                                name: "bp".to_string(),
                                value: runtime.bp.to_string(),
                                type_: Some("int".to_string()),
                                presentation_hint: None,
                                evaluate_name: None,
                                variables_reference: 0,
                                named_variables: None,
                                indexed_variables: None,
                                memory_reference: None,
                            },
                        ],
                    })?,
                }
                .build(&req)]),
                _ => {
                    let values = runtime.get_stack_values(arg.variables_reference - 2);

                    Ok(vec![ProtocolMessageResponseBuilder {
                        body: serde_json::to_value(VariablesResponse {
                            variables: values
                                .into_iter()
                                .enumerate()
                                .map(|(i, value)| Variable {
                                    name: format!("#{}", i),
                                    value: format!("{:?}", value),
                                    type_: Some("int".to_string()),
                                    presentation_hint: None,
                                    evaluate_name: None,
                                    variables_reference: 0,
                                    named_variables: None,
                                    indexed_variables: None,
                                    memory_reference: None,
                                })
                                .collect(),
                        })?,
                    }
                    .build(&req)])
                }
            }
        }
        "next" => {
            let mut runtime = ctx.0.lock().unwrap();
            let next = runtime.step(true).unwrap();
            if !next {
                return Ok(vec![]);
            }

            Ok(vec![
                ProtocolMessageResponseBuilder {
                    body: serde_json::to_value(NextResponse {})?,
                }
                .build(&req),
                ProtocolMessageEventBuilder {
                    body: serde_json::to_value(StoppedEvent {
                        reason: StoppedEventReason::Step,
                        description: Some(format!(
                            "pc: {}, next: {:?}",
                            runtime.pc,
                            runtime.show_next_instruction()
                        )),
                        thread_id: Some(MAIN_THREAD_ID),
                        preserve_focus_hint: None,
                        text: None,
                        all_threads_stopped: None,
                        hit_breakpoint_ids: None,
                    })?,
                    event: ProtocolMessageEventKind::Stopped,
                }
                .build(),
                ProtocolMessageEventBuilder {
                    body: serde_json::to_value(OutputEvent {
                        category: Some(OutputEventKind::Console),
                        output: format!(
                            "pc: {}, next: {:?}\n",
                            runtime.pc,
                            runtime.show_next_instruction()
                        ),
                        group: Some("start".to_string()),
                        variable_reference: None,
                        source: None,
                        line: None,
                        column: None,
                        data: None,
                    })?,
                    event: ProtocolMessageEventKind::Output,
                }
                .build(),
                ProtocolMessageEventBuilder {
                    body: serde_json::to_value(OutputEvent {
                        category: Some(OutputEventKind::Console),
                        output: runtime.show_stacks(),
                        group: None,
                        variable_reference: None,
                        source: None,
                        line: None,
                        column: None,
                        data: None,
                    })?,
                    event: ProtocolMessageEventKind::Output,
                }
                .build(),
                ProtocolMessageEventBuilder {
                    body: serde_json::to_value(OutputEvent {
                        category: Some(OutputEventKind::Console),
                        output: "".to_string(),
                        group: Some("end".to_string()),
                        variable_reference: None,
                        source: None,
                        line: None,
                        column: None,
                        data: None,
                    })?,
                    event: ProtocolMessageEventKind::Output,
                }
                .build(),
            ])
        }
        "readMemory" => {
            let arg = serde_json::from_value::<dap::ReadMemoryArguments>(req.arguments.clone())?;
            let runtime = ctx.0.lock().unwrap();

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(ReadMemoryResponse {
                    address: "0x0".to_string(),
                    data: Some(
                        BASE64_STANDARD
                            .encode(&runtime.memory[0..arg.count.min(runtime.memory.len())]),
                    ),
                    unreadable_bytes: None,
                })?,
            }
            .build(&req)])
        }
        "disassemble" => {
            let arg = serde_json::from_value::<dap::DisassembleArguments>(req.arguments.clone())?;

            println!("disassemble: {:?}", arg);

            Ok(vec![])
        }
        _ => Ok(vec![]),
    }
}

impl DapServer<DapContext> for DapImpl {
    fn handle_request(
        ctx: DapContext,
        req: ProtocolMessageRequest,
    ) -> FutureResult<Vec<ProtocolMessageResponse>> {
        Box::pin(dap_handler(ctx, req))
    }
}
