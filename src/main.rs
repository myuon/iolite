use std::{
    io::Read,
    sync::{Arc, Mutex},
};

use anyhow::Result;
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
use tokio::sync::mpsc::Sender;

use crate::{
    compiler::{ast::Module, runtime::ControlFlow, vm::Instruction},
    dap::{
        BreakpointLocation, Capabilities, ExitedEvent, InitializeResponseBody, OutputEvent,
        OutputEventKind, Source, Variable, VariablesArguments, VariablesResponse,
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

fn compile(input: String) -> Result<Vec<u8>> {
    let decls = compiler::Compiler::parse(compiler::Compiler::create_input(input.clone()))?;
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

    Ok(binary)
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        CliCommands::Compile { input, stdin } => {
            let input = if stdin {
                let mut buf = Vec::new();
                std::io::stdin().read_to_end(&mut buf).unwrap();

                String::from_utf8(buf)?
            } else {
                input.unwrap()
            };

            compile(input)?;
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
            let result = compiler::Compiler::run(input, print_stacks)?;

            println!("result: {}", result);
        }
        CliCommands::Lsp {} => {
            Lsp::new(LspImpl).start((), 3030).await?;
        }
        CliCommands::Dap {} => {
            Dap::new(DapImpl)
                .start(
                    DapContext(Arc::new(Mutex::new(Runtime::new(1024, vec![])))),
                    3031,
                )
                .await?;
        }
    }

    Ok(())
}

#[derive(Clone)]
struct LspImpl;

async fn lsp_handler(req: RpcMessageRequest) -> Result<Option<RpcMessageResponse>> {
    let token_types = vec![SemanticTokenTypes::Keyword, SemanticTokenTypes::Comment];

    match req.method.as_str() {
        "initialize" => {
            println!("Initialize! {}", serde_json::to_string(&req.params)?);

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

            let tokens = compiler::Compiler::run_lexer(content.clone())?;
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

            let input = compiler::Compiler::create_input(std::fs::read_to_string(
                params.text_document.uri.as_filepath().unwrap(),
            )?);
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
            println!("Declaration! {}", serde_json::to_string(&req.params)?);

            Ok(None)
        }
        "textDocument/definition" => {
            let params = serde_json::from_value::<TextDocumentPositionParams>(req.params.clone())?;
            let input = std::fs::read_to_string(params.text_document.uri.as_filepath().unwrap())?;
            let parsed = compiler::Compiler::parse(input.clone())?;
            let mut module = compiler::Compiler::create_module(parsed);

            let position = compiler::Compiler::find_line_and_column(
                &input,
                params.position.line,
                params.position.character,
            );
            let def_position = compiler::Compiler::search_for_definition(&mut module, position)?;

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
    sender: Sender<ProtocolMessageEventBuilder>,
    req: ProtocolMessageRequest,
) -> Result<Vec<ProtocolMessageResponse>> {
    const MAIN_THREAD_ID: usize = 1;

    fn runtime_start(ctx: DapContext, sender: Sender<ProtocolMessageEventBuilder>) {
        tokio::spawn(async move {
            let (flow, pc, next) = {
                let mut runtime = ctx.0.lock().unwrap();

                let mut flow = ControlFlow::Continue;
                while matches!(flow, ControlFlow::Continue) {
                    flow = runtime.step(false).unwrap();
                }

                (flow, runtime.pc, runtime.show_next_instruction())
            };

            println!("Runtime stopped with {:?}", flow);

            match flow {
                ControlFlow::HitBreakpoint => {
                    sender
                        .send(ProtocolMessageEventBuilder {
                            body: serde_json::to_value(StoppedEvent {
                                reason: StoppedEventReason::Breakpoint,
                                description: Some(format!("pc: {}, next: {:?}", pc, next)),
                                thread_id: Some(MAIN_THREAD_ID),
                                preserve_focus_hint: None,
                                text: None,
                                all_threads_stopped: None,
                                hit_breakpoint_ids: None,
                            })?,
                            event: ProtocolMessageEventKind::Stopped,
                        })
                        .await?;
                    println!("sender!!");
                }
                ControlFlow::Finish => {
                    sender
                        .send(ProtocolMessageEventBuilder {
                            body: serde_json::to_value(dap::TerminatedEvent { restart: None })?,
                            event: ProtocolMessageEventKind::Terminated,
                        })
                        .await?;
                    sender
                        .send(ProtocolMessageEventBuilder {
                            body: serde_json::to_value(ExitedEvent { exit_code: 0 })?,
                            event: ProtocolMessageEventKind::Exited,
                        })
                        .await?;
                }
                _ => (),
            };

            Ok::<(), anyhow::Error>(())
        });
    }

    match req.command.as_str() {
        "initialize" => {
            tokio::spawn(async move {
                sender
                    .send(ProtocolMessageEventBuilder {
                        body: serde_json::to_value(InitializedEvent {})?,
                        event: ProtocolMessageEventKind::Initialized,
                    })
                    .await?;

                Ok::<(), anyhow::Error>(())
            });

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(InitializeResponseBody(Capabilities {
                    supports_configuration_done_request: Some(true),
                    supports_single_thread_execution_requests: Some(true),
                    supports_function_breakpoints: Some(false),
                    supports_conditional_breakpoints: Some(false),
                    supports_hit_conditional_breakpoints: Some(false),
                    supports_read_memory_request: Some(true),
                    supports_memory_event: Some(true),
                    supports_disassemble_request: Some(true),
                    supports_breakpoint_locations_request: Some(true),
                }))?,
            }
            .build(&req)])
        }
        "launch" => {
            let arg = serde_json::from_value::<LaunchRequestArguments>(req.arguments.clone())?;
            let content = std::fs::read_to_string(arg.source_file.unwrap())?;
            let program = compiler::Compiler::compile(content.clone())?;

            ctx.0.lock().unwrap().init(1024, program, content);

            sender
                .send(ProtocolMessageEventBuilder {
                    body: serde_json::to_value(StoppedEvent {
                        reason: StoppedEventReason::Entry,
                        description: Some("Entry".to_string()),
                        thread_id: Some(MAIN_THREAD_ID),
                        preserve_focus_hint: None,
                        text: None,
                        all_threads_stopped: None,
                        hit_breakpoint_ids: None,
                    })?,
                    event: ProtocolMessageEventKind::Stopped,
                })
                .await?;

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
        "configurationDone" => Ok(vec![ProtocolMessageResponseBuilder {
            body: serde_json::to_value(ConfigurationDoneResponse {})?,
        }
        .build(&req)]),
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
                            source: Some(Source {
                                name: None,
                                path: None,
                                source_reference: None,
                                presentation_hint: None,
                                origin: None,
                                sources: None,
                                adapter_data: None,
                                checksums: None,
                            }),
                            line: 4,
                            column: 0,
                            end_line: None,
                            end_column: None,
                            can_restart: Some(true),
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
            let control = runtime.step(true)?;

            match control {
                ControlFlow::Finish => Ok(vec![
                    ProtocolMessageResponseBuilder {
                        body: serde_json::to_value(NextResponse {})?,
                    }
                    .build(&req),
                    ProtocolMessageEventBuilder {
                        body: serde_json::to_value(ExitedEvent { exit_code: 0 })?,
                        event: ProtocolMessageEventKind::Exited,
                    }
                    .build(),
                ]),
                ControlFlow::HitBreakpoint => Ok(vec![
                    ProtocolMessageResponseBuilder {
                        body: serde_json::to_value(NextResponse {})?,
                    }
                    .build(&req),
                    ProtocolMessageEventBuilder {
                        body: serde_json::to_value(StoppedEvent {
                            reason: StoppedEventReason::Breakpoint,
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
                ]),
                ControlFlow::Continue => Ok(vec![
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
                ]),
            }
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
        "setFunctionBreakpoints" => Ok(vec![ProtocolMessageResponseBuilder {
            body: serde_json::to_value(dap::SetFunctionBreakpointsResponse {
                breakpoints: vec![],
            })?,
        }
        .build(&req)]),
        "breakpointLocations" => {
            let arg =
                serde_json::from_value::<dap::BreakpointLocationsArguments>(req.arguments.clone())?;

            let mut runtime = ctx.0.lock().unwrap();
            let position = compiler::Compiler::find_line_and_column(
                &runtime.source_code,
                arg.line,
                arg.column.unwrap_or(0),
            );

            runtime.set_breakpoints(vec![position]);

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(dap::BreakpointLocationsResponse {
                    breakpoints: vec![BreakpointLocation {
                        line: arg.line,
                        column: arg.column,
                        end_line: arg.end_line,
                        end_column: arg.end_column,
                    }],
                })?,
            }
            .build(&req)])
        }
        "setBreakpoints" => {
            let arg =
                serde_json::from_value::<dap::SetBreakpointsArguments>(req.arguments.clone())?;

            let std_content = compiler::Compiler::create_input(String::new());
            let std_content_len = std_content.len();

            let mut runtime = ctx.0.lock().unwrap();

            let mut breakpoints = vec![];
            if let Some(bps) = &arg.breakpoints {
                for bp in bps {
                    let position = compiler::Compiler::find_line_and_column(
                        &runtime.source_code,
                        bp.line,
                        bp.column.unwrap_or(0),
                    );

                    breakpoints.push(position + std_content_len);
                }
            }

            runtime.set_breakpoints(breakpoints);

            Ok(vec![ProtocolMessageResponseBuilder {
                body: serde_json::to_value(dap::SetBreakpointsResponse {
                    breakpoints: arg
                        .breakpoints
                        .unwrap_or(vec![])
                        .into_iter()
                        .enumerate()
                        .map(|(i, bp)| dap::Breakpoint {
                            id: Some(i),
                            verified: true,
                            message: None,
                            source: None,
                            line: Some(bp.line),
                            column: bp.column,
                            end_line: None,
                            end_column: None,
                            instruction_reference: None,
                            offset: None,
                            reason: None,
                        })
                        .collect(),
                })?,
            }
            .build(&req)])
        }
        "continue" => {
            let resp = ProtocolMessageResponseBuilder {
                body: serde_json::to_value(dap::ContinueResponse {
                    all_threads_continued: None,
                })?,
            }
            .build(&req);

            runtime_start(ctx, sender);

            Ok(vec![resp])
        }
        _ => Ok(vec![]),
    }
}

impl DapServer<DapContext> for DapImpl {
    fn handle_request(
        ctx: DapContext,
        sender: Sender<ProtocolMessageEventBuilder>,
        req: ProtocolMessageRequest,
    ) -> FutureResult<Vec<ProtocolMessageResponse>> {
        Box::pin(dap_handler(ctx, sender, req))
    }
}
