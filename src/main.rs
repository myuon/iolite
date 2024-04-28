use std::{
    io::Read,
    path::Path,
    sync::{Arc, Mutex},
};

use ::dap::{
    base_message::Sendable,
    events::{Event, ExitedEventBody, OutputEventBody, StoppedEventBody, TerminatedEventBody},
    requests::Command,
    responses::{
        ContinueResponse, ReadMemoryResponse, ResponseBody, ScopesResponse, SetBreakpointsResponse,
        SetExceptionBreakpointsResponse, SourceResponse, StackTraceResponse, ThreadsResponse,
        VariablesResponse,
    },
    types::{
        Breakpoint, Capabilities, OutputEventCategory, OutputEventGroup, Scope, Source, StackFrame,
        StoppedEventReason, Thread, Variable,
    },
};
use anyhow::{anyhow, bail, Result};
use base64::prelude::*;
use clap::{Parser, Subcommand};
use compiler::{
    ast::{
        AstWalker, AstWalkerMode, Span, AST_WALKER_FIELD, AST_WALKER_FUNCTION, AST_WALKER_METHOD,
        AST_WALKER_TYPE,
    },
    parser::ParseError,
    runtime::Runtime,
    typechecker::TypecheckerError,
    CompilerError,
};
use utils::{
    dap::server::{Dap, DapServer},
    lsp::{
        server::{Lsp, LspServer},
        NotificationMessage, RpcMessageRequest, RpcMessageResponse,
    },
};

use lsp_types::{
    notification::{DidSaveTextDocument, Initialized, Notification, PublishDiagnostics},
    request::{
        DocumentDiagnosticRequest, GotoDefinition, HoverRequest, Initialize, InlayHintRequest,
        Request, SemanticTokensFullRequest,
    },
    DeclarationCapability, Diagnostic, DiagnosticOptions, DiagnosticServerCapabilities,
    DiagnosticSeverity, DidSaveTextDocumentParams, FullDocumentDiagnosticReport, Hover,
    HoverContents, HoverParams, HoverProviderCapability, InitializeResult, InlayHint,
    InlayHintLabel, InlayHintParams, Location, MarkedString, OneOf, Position,
    PublishDiagnosticsParams, Range, RelatedFullDocumentDiagnosticReport, SemanticToken,
    SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensServerCapabilities,
    ServerCapabilities, TextDocumentIdentifier, TextDocumentPositionParams,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use utils::sender::SimpleSender;
use utils::server_process::{FutureResult, ServerProcess};

use crate::compiler::{ast::Module, runtime::ControlFlow, vm::Instruction};

mod compiler;
mod utils;

#[derive(Parser, Debug)]
#[clap(name = "iolite")]
struct Cli {
    #[clap(subcommand)]
    command: CliCommands,
}

#[derive(Debug, Subcommand)]
enum CliCommands {
    Compile {
        file: Option<String>,

        #[clap(long)]
        stdin: bool,
    },
    Run {
        file: Option<String>,

        #[clap(long)]
        stdin: bool,
        #[clap(long = "print-stacks")]
        print_stacks: bool,
    },
    Lsp {},
    Dap {},
}

fn compile(input: String) -> Result<Vec<u8>> {
    let decls = compiler::Compiler::parse_decls(compiler::Compiler::create_input(input.clone()))?;
    let mut module = Module {
        name: "main".to_string(),
        declarations: decls,
    };
    let types = compiler::Compiler::typecheck_module(&mut module, &input).unwrap();

    let ir = compiler::Compiler::ir_code_gen_module(module, types).unwrap();
    let code = compiler::Compiler::vm_code_gen(ir).unwrap();
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

    Ok(binary)
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut compiler = compiler::Compiler::new();

    match cli.command {
        CliCommands::Compile { file, stdin } => {
            let input = if stdin {
                let mut buf = Vec::new();
                std::io::stdin().read_to_end(&mut buf).unwrap();

                String::from_utf8(buf)?
            } else {
                file.unwrap()
            };

            compile(input)?;
        }
        CliCommands::Run {
            file,
            stdin,
            print_stacks: _,
        } => {
            let cwd = match file.clone() {
                Some(file) => std::path::Path::new(&file)
                    .parent()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                None => std::env::current_dir()
                    .map_err(|err| anyhow!("Failed to get current directory: {}", err))?
                    .to_str()
                    .unwrap()
                    .to_string(),
            };
            compiler.set_cwd(cwd);

            let source_code = match file {
                Some(file) => std::fs::read_to_string(&file)
                    .map_err(|err| anyhow!("Failed to read file {}: {}", file, err))?,
                None => {
                    if stdin {
                        let mut buf = Vec::new();
                        std::io::stdin().read_to_end(&mut buf).unwrap();

                        String::from_utf8(buf).unwrap()
                    } else {
                        bail!("No input file or stdin provided");
                    }
                }
            };

            let main = "main".to_string();

            compiler.parse_with_code(main.clone(), source_code)?;
            compiler.typecheck(main.clone())?;

            let ir = compiler.ir_code_gen(main.clone())?;
            let code = compiler::Compiler::vm_code_gen(ir)?;
            let binary = compiler::Compiler::byte_code_gen(code)?;

            let result = compiler::Compiler::run_vm(binary, false)?;

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

async fn lsp_handler(
    req: RpcMessageRequest,
    sender: SimpleSender<String, NotificationMessage>,
) -> Result<Option<RpcMessageResponse>> {
    let token_types = vec![
        SemanticTokenType::FUNCTION,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::METHOD,
        SemanticTokenType::TYPE,
    ];

    match req.method.as_str() {
        Initialize::METHOD => {
            println!("Initialize! {}", serde_json::to_string(&req.params)?);

            Ok(Some(RpcMessageResponse::new(
                req.id,
                InitializeResult {
                    capabilities: ServerCapabilities {
                        text_document_sync: Some(TextDocumentSyncCapability::Kind(
                            TextDocumentSyncKind::FULL,
                        )),
                        declaration_provider: Some(DeclarationCapability::Simple(true)),
                        definition_provider: Some(OneOf::Left(true)),
                        semantic_tokens_provider: Some(
                            SemanticTokensServerCapabilities::SemanticTokensOptions(
                                SemanticTokensOptions {
                                    legend: SemanticTokensLegend {
                                        token_types,
                                        token_modifiers: vec![],
                                    },
                                    range: None,
                                    full: Some(SemanticTokensFullOptions::Bool(true)),
                                    work_done_progress_options: WorkDoneProgressOptions {
                                        work_done_progress: None,
                                    },
                                },
                            ),
                        ),
                        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                            DiagnosticOptions {
                                identifier: None,
                                inter_file_dependencies: false,
                                workspace_diagnostics: false,
                                work_done_progress_options: WorkDoneProgressOptions {
                                    work_done_progress: None,
                                },
                            },
                        )),
                        position_encoding: None,
                        selection_range_provider: None,
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        completion_provider: None,
                        signature_help_provider: None,
                        type_definition_provider: None,
                        implementation_provider: None,
                        references_provider: None,
                        document_highlight_provider: None,
                        document_symbol_provider: None,
                        workspace_symbol_provider: None,
                        code_action_provider: None,
                        code_lens_provider: None,
                        document_formatting_provider: None,
                        document_range_formatting_provider: None,
                        document_on_type_formatting_provider: None,
                        rename_provider: None,
                        document_link_provider: None,
                        color_provider: None,
                        folding_range_provider: None,
                        execute_command_provider: None,
                        workspace: None,
                        call_hierarchy_provider: None,
                        moniker_provider: None,
                        linked_editing_range_provider: None,
                        inline_value_provider: None,
                        inlay_hint_provider: Some(OneOf::Left(true)),
                        experimental: None,
                    },
                    server_info: None,
                },
            )?))
        }
        Initialized::METHOD => {
            println!("Initialized!");

            Ok(None)
        }
        SemanticTokensFullRequest::METHOD => {
            let params = serde_json::from_value::<SemanticTokensParams>(req.params.clone())?;
            let filepath = params.text_document.uri.path();
            let content = std::fs::read_to_string(filepath)?;

            let Ok(module) = compiler::Compiler::parse_module(content.clone()) else {
                return Ok(None);
            };
            let mut walker = AstWalker::new(AstWalkerMode::SemanticTokens);
            walker.module(&module);

            let mut token_data = vec![];
            let mut prev = (0, 0);

            let mut tokens = walker.tokens;
            tokens.sort_by(|a, b| a.1.start.unwrap().cmp(&b.1.start.unwrap()));

            for (ty, span) in tokens {
                match (span.start, span.end) {
                    (Some(start), Some(end)) => {
                        let (line, col) =
                            compiler::Compiler::find_position_with_input(&content, start);

                        token_data.push(SemanticToken {
                            delta_line: (line - prev.0) as u32,
                            delta_start: if line == prev.0 {
                                (col - prev.1) as u32
                            } else {
                                col as u32
                            },
                            token_type: token_types
                                .iter()
                                .position(|t| {
                                    t == &match ty.as_str() {
                                        AST_WALKER_FUNCTION => SemanticTokenType::FUNCTION,
                                        AST_WALKER_FIELD => SemanticTokenType::PROPERTY,
                                        AST_WALKER_METHOD => SemanticTokenType::METHOD,
                                        AST_WALKER_TYPE => SemanticTokenType::TYPE,
                                        _ => todo!(),
                                    }
                                })
                                .unwrap() as u32,
                            length: (end - start) as u32,
                            token_modifiers_bitset: 0,
                        });

                        prev = (line, col);
                    }
                    _ => {}
                }
            }

            Ok(Some(RpcMessageResponse::new(
                req.id,
                SemanticTokens {
                    result_id: None,
                    data: token_data,
                },
            )?))
        }
        DocumentDiagnosticRequest::METHOD | DidSaveTextDocument::METHOD => {
            let params = serde_json::from_value::<SemanticTokensParams>(req.params.clone())?;

            let path = params.text_document.uri.path();
            let mut compiler = compiler::Compiler::new();
            compiler.set_cwd(
                Path::new(&path)
                    .parent()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
            );

            let path = Path::new(&path)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
                .replace(".io", "");

            match compiler
                .parse(path.to_string())
                .and_then(|_| compiler.typecheck(path.to_string()))
            {
                Ok(_) => {
                    sender
                        .send(NotificationMessage::new::<PublishDiagnostics>(
                            PublishDiagnosticsParams {
                                uri: params.text_document.uri,
                                diagnostics: vec![],
                                version: None,
                            },
                        )?)
                        .await?;
                }
                Err(err) => {
                    let message = format!("{:?}", err);
                    let span = match err.downcast::<CompilerError>() {
                        Ok(CompilerError::LexerError(_)) => Span::unknown(),
                        Ok(CompilerError::ParseError(err)) => match err {
                            ParseError::UnexpectedEos => Span::unknown(),
                            ParseError::UnexpectedToken { got, .. } => got.span,
                        },
                        Ok(CompilerError::TypecheckError(err)) => match err {
                            TypecheckerError::IdentNotFound(ident) => ident.span,
                            TypecheckerError::TypeMismatch { span, .. } => span,
                            TypecheckerError::NumericTypeExpected(_) => Span::unknown(),
                            TypecheckerError::ArgumentCountMismatch(_, _) => Span::unknown(),
                            TypecheckerError::FunctionTypeExpected(_) => Span::unknown(),
                            TypecheckerError::IndexNotSupported(_) => Span::unknown(),
                            TypecheckerError::ConversionNotSupported(_, ty) => ty.span,
                        },
                        err => todo!("{:?}", err),
                    };
                    let start = {
                        match span.start {
                            Some(s) => compiler.find_position(&path, s)?,
                            None => (0, 0),
                        }
                    };
                    let end = {
                        match span.end {
                            Some(e) => compiler.find_position(&path, e)?,
                            None => (0, 1),
                        }
                    };

                    sender
                        .send(NotificationMessage::new::<PublishDiagnostics>(
                            PublishDiagnosticsParams {
                                uri: params.text_document.uri,
                                diagnostics: vec![Diagnostic {
                                    range: Range {
                                        start: Position {
                                            line: start.0 as u32,
                                            character: start.1 as u32,
                                        },
                                        end: Position {
                                            line: end.0 as u32,
                                            character: end.1 as u32,
                                        },
                                    },
                                    message,
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    code: None,
                                    code_description: None,
                                    source: None,
                                    related_information: None,
                                    tags: None,
                                    data: None,
                                }],
                                version: None,
                            },
                        )?)
                        .await?;

                    return Ok(None);
                }
            };

            Ok(Some(RpcMessageResponse::new(
                req.id,
                RelatedFullDocumentDiagnosticReport {
                    related_documents: None,
                    full_document_diagnostic_report: FullDocumentDiagnosticReport {
                        items: vec![],
                        result_id: None,
                    },
                },
            )?))
        }
        GotoDefinition::METHOD => {
            let params = serde_json::from_value::<TextDocumentPositionParams>(req.params.clone())?;

            let path = params.text_document.uri.path();
            let mut compiler = compiler::Compiler::new();

            compiler.parse(path.to_string())?;

            let position = compiler.find_line_and_column(
                path,
                params.position.line as usize,
                params.position.character as usize,
            )?;
            let def_position = compiler.search_for_definition(path.to_string(), position)?;

            if let Some(def_position) = def_position {
                let start_position = compiler.find_position(path, def_position.start.unwrap())?;
                let end_position = compiler.find_position(path, def_position.end.unwrap())?;

                return Ok(Some(RpcMessageResponse::new(
                    req.id,
                    Location {
                        uri: params.text_document.uri,
                        range: Range {
                            start: Position {
                                line: start_position.0 as u32,
                                character: start_position.1 as u32,
                            },
                            end: Position {
                                line: end_position.0 as u32,
                                character: end_position.1 as u32,
                            },
                        },
                    },
                )?));
            }

            Ok(None)
        }
        HoverRequest::METHOD => {
            let params = serde_json::from_value::<HoverParams>(req.params.clone())?;
            let path = Path::new(
                params
                    .text_document_position_params
                    .text_document
                    .uri
                    .path(),
            );
            let (line, col) = (
                params.text_document_position_params.position.line as usize,
                params.text_document_position_params.position.character as usize,
            );
            let module_name = Path::new(path.to_str().unwrap())
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .replace(".io", "");

            let mut compiler = compiler::Compiler::new();
            compiler.set_cwd(path.parent().unwrap().to_str().unwrap().to_string());
            compiler.parse(module_name.clone())?;

            let position = compiler.find_line_and_column(&module_name, line, col)?;
            let ty = compiler.infer_type_at(module_name.clone(), position)?;
            if let Some(ty) = ty {
                Ok(Some(RpcMessageResponse::new(
                    req.id,
                    Hover {
                        contents: HoverContents::Scalar(MarkedString::String(format!(
                            "{}",
                            ty.to_string()
                        ))),
                        range: None,
                    },
                )?))
            } else {
                Ok(None)
            }
        }
        InlayHintRequest::METHOD => {
            let params = serde_json::from_value::<InlayHintParams>(req.params.clone())?;
            let path = Path::new(params.text_document.uri.path());
            let module_name = Path::new(path.to_str().unwrap())
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .replace(".io", "");

            let mut compiler = compiler::Compiler::new();
            compiler.set_cwd(path.parent().unwrap().to_str().unwrap().to_string());
            compiler.parse(module_name.clone())?;
            let types = compiler.inlay_hints(module_name.clone())?;

            let mut hints = vec![];
            for (span, ty) in types {
                let (line, col) = compiler.find_position(&module_name, span.end.unwrap())?;
                hints.push(InlayHint {
                    position: Position {
                        line: line as u32,
                        character: col as u32,
                    },
                    label: InlayHintLabel::String(format!(": {}", ty.to_string())),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                })
            }

            Ok(Some(RpcMessageResponse::new(req.id, hints)?))
        }
        _ => Ok(None),
    }
}

impl LspServer for LspImpl {
    fn handle_request(
        req: RpcMessageRequest,
        sender: SimpleSender<String, NotificationMessage>,
    ) -> FutureResult<Option<RpcMessageResponse>> {
        Box::pin(lsp_handler(req, sender))
    }
}

#[tokio::test]
async fn test_lsp_handler_diagnostics() -> Result<()> {
    use pretty_assertions::assert_eq;

    let req = RpcMessageRequest {
        id: None,
        method: DidSaveTextDocument::METHOD.to_string(),
        params: serde_json::to_value(&DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier::new(Url::parse(&format!(
                "file:///{}",
                std::env::current_dir()?
                    .join("tests/lsp/diagnostics/test1.io")
                    .to_str()
                    .unwrap()
            ))?),
            text: None,
        })?,
        jsonrpc: "".to_string(),
    };
    let (sender, mut receiver) = tokio::sync::mpsc::channel::<String>(100);
    let sender = SimpleSender::new(sender, Arc::new(|m| serde_json::to_string(&m).unwrap()));
    let res = lsp_handler(req, sender).await?;
    assert!(res.is_none());

    let r = receiver.recv().await.unwrap();
    let message = serde_json::from_str::<'_, NotificationMessage>(&r)?;
    let params = serde_json::from_value::<PublishDiagnosticsParams>(message.params.clone())?;
    assert_eq!(
        params
            .diagnostics
            .into_iter()
            .map(|d| d.range)
            .collect::<Vec<_>>(),
        vec![Range {
            start: Position {
                line: 3,
                character: 11,
            },
            end: Position {
                line: 3,
                character: 12,
            },
        }],
    );

    Ok(())
}

#[derive(Clone)]
struct DapImpl;

#[derive(Clone)]
struct DapContext(Arc<Mutex<Runtime>>);

async fn dap_handler(
    ctx: DapContext,
    sender: SimpleSender<Sendable, Event>,
    req: Command,
) -> Result<ResponseBody> {
    const MAIN_THREAD_ID: i64 = 1;

    fn runtime_start(ctx: DapContext, sender: SimpleSender<Sendable, Event>) {
        tokio::spawn(async move {
            let (flow, pc, next) = {
                let mut runtime = ctx.0.lock().unwrap();

                let mut flow = ControlFlow::Continue;
                while matches!(flow, ControlFlow::Continue) {
                    flow = runtime.step(false).unwrap();
                }

                (flow, runtime.pc, runtime.show_next_instruction())
            };

            match flow {
                ControlFlow::HitBreakpoint => {
                    sender
                        .send(Event::Stopped(StoppedEventBody {
                            reason: StoppedEventReason::Breakpoint,
                            description: Some(format!("pc: {}, next: {:?}", pc, next)),
                            thread_id: Some(MAIN_THREAD_ID),
                            preserve_focus_hint: None,
                            text: None,
                            all_threads_stopped: None,
                            hit_breakpoint_ids: None,
                        }))
                        .await?;
                }
                ControlFlow::Finish => {
                    sender
                        .send(Event::Terminated(Some(TerminatedEventBody {
                            restart: None,
                        })))
                        .await?;
                    sender
                        .send(Event::Exited(ExitedEventBody { exit_code: 0 }))
                        .await?;
                }
                _ => (),
            };

            Ok::<(), anyhow::Error>(())
        });
    }

    match req {
        Command::Initialize(_) => {
            tokio::spawn(async move {
                sender.send(Event::Initialized).await?;

                Ok::<(), anyhow::Error>(())
            });

            Ok(ResponseBody::Initialize(Capabilities {
                supports_configuration_done_request: None,
                supports_function_breakpoints: None,
                supports_conditional_breakpoints: None,
                supports_hit_conditional_breakpoints: None,
                supports_evaluate_for_hovers: None,
                exception_breakpoint_filters: None,
                supports_step_back: None,
                supports_set_variable: None,
                supports_restart_frame: None,
                supports_goto_targets_request: None,
                supports_step_in_targets_request: None,
                supports_completions_request: None,
                completion_trigger_characters: None,
                supports_modules_request: None,
                additional_module_columns: None,
                supported_checksum_algorithms: None,
                supports_restart_request: None,
                supports_exception_options: None,
                supports_value_formatting_options: None,
                supports_exception_info_request: None,
                support_terminate_debuggee: None,
                support_suspend_debuggee: None,
                supports_delayed_stack_trace_loading: None,
                supports_loaded_sources_request: None,
                supports_log_points: None,
                supports_terminate_threads_request: None,
                supports_set_expression: None,
                supports_terminate_request: None,
                supports_data_breakpoints: None,
                supports_read_memory_request: None,
                supports_write_memory_request: None,
                supports_disassemble_request: None,
                supports_cancel_request: None,
                supports_breakpoint_locations_request: None,
                supports_clipboard_context: None,
                supports_stepping_granularity: None,
                supports_instruction_breakpoints: None,
                supports_exception_filter_options: None,
                supports_single_thread_execution_requests: None,
            }))
        }
        Command::ConfigurationDone => todo!(),
        Command::Launch(arg) => {
            #[derive(serde::Deserialize, Debug)]
            #[serde(rename_all = "camelCase")]
            struct LaunchAdditionalData {
                source_file: Option<String>,
                cwd: Option<String>,
            }

            let data =
                serde_json::from_value::<LaunchAdditionalData>(arg.additional_data.unwrap())?;

            let source_file = Path::new(&data.cwd.clone().unwrap())
                .join(Path::new(&data.source_file.unwrap()))
                .to_str()
                .unwrap()
                .to_string();
            let mut compiler = compiler::Compiler::new();
            compiler.set_cwd(data.cwd.unwrap());

            let main = "main".to_string();
            let source_code = std::fs::read_to_string(&source_file)?;

            compiler.parse_with_code(main.clone(), source_code.clone())?;
            compiler.typecheck(main.clone())?;

            let ir = compiler.ir_code_gen(main.clone())?;
            let code = compiler::Compiler::vm_code_gen(ir)?;
            let program = compiler::Compiler::byte_code_gen(code)?;

            ctx.0.lock().unwrap().init(
                1024,
                program,
                std::path::Path::new(&source_file)
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                source_code,
            );

            sender
                .send(Event::Stopped(StoppedEventBody {
                    reason: StoppedEventReason::Entry,
                    description: Some("Entry".to_string()),
                    thread_id: Some(MAIN_THREAD_ID),
                    preserve_focus_hint: None,
                    text: None,
                    all_threads_stopped: None,
                    hit_breakpoint_ids: None,
                }))
                .await?;

            Ok(ResponseBody::Launch)
        }
        Command::Attach(_) => todo!(),
        Command::BreakpointLocations(_) => todo!(),
        Command::Completions(_) => todo!(),
        Command::Continue(_) => {
            runtime_start(ctx, sender);

            Ok(ResponseBody::Continue(ContinueResponse {
                all_threads_continued: None,
            }))
        }
        Command::DataBreakpointInfo(_) => todo!(),
        Command::Disassemble(_) => todo!(),
        Command::Disconnect(_) => Ok(ResponseBody::Disconnect),
        Command::Evaluate(_) => todo!(),
        Command::ExceptionInfo(_) => todo!(),
        Command::Goto(_) => todo!(),
        Command::GotoTargets(_) => todo!(),
        Command::LoadedSources => todo!(),
        Command::Modules(_) => todo!(),
        Command::Next(_) => {
            let control = {
                let mut runtime = ctx.0.lock().unwrap();
                let control = runtime.step(true)?;

                control
            };

            match control {
                ControlFlow::Finish => {
                    sender
                        .send(Event::Exited(ExitedEventBody { exit_code: 0 }))
                        .await?;

                    Ok(ResponseBody::Next)
                }
                ControlFlow::HitBreakpoint => {
                    sender
                        .send(Event::Stopped(StoppedEventBody {
                            reason: StoppedEventReason::Breakpoint,
                            description: Some("Breakpoint".to_string()),
                            thread_id: Some(MAIN_THREAD_ID),
                            preserve_focus_hint: None,
                            text: None,
                            all_threads_stopped: None,
                            hit_breakpoint_ids: None,
                        }))
                        .await?;

                    Ok(ResponseBody::Next)
                }
                ControlFlow::Continue => {
                    let (pc, next, stacks) = {
                        let runtime = ctx.0.lock().unwrap();

                        (
                            runtime.pc,
                            runtime.show_next_instruction(),
                            runtime.show_stacks(),
                        )
                    };

                    sender
                        .send(Event::Stopped(StoppedEventBody {
                            reason: StoppedEventReason::Step,
                            description: Some(format!("pc: {}, next: {:?}", pc, next)),
                            thread_id: Some(MAIN_THREAD_ID),
                            preserve_focus_hint: None,
                            text: None,
                            all_threads_stopped: None,
                            hit_breakpoint_ids: None,
                        }))
                        .await?;
                    sender
                        .send(Event::Output(OutputEventBody {
                            category: Some(OutputEventCategory::Console),
                            output: format!("pc: {}, next: {:?}\n", pc, next),
                            group: Some(OutputEventGroup::Start),
                            variables_reference: None,
                            source: None,
                            line: None,
                            column: None,
                            data: None,
                        }))
                        .await?;
                    sender
                        .send(Event::Output(OutputEventBody {
                            category: Some(OutputEventCategory::Console),
                            output: stacks,
                            group: None,
                            variables_reference: None,
                            source: None,
                            line: None,
                            column: None,
                            data: None,
                        }))
                        .await?;
                    sender
                        .send(Event::Output(OutputEventBody {
                            category: Some(OutputEventCategory::Console),
                            output: "".to_string(),
                            group: Some(OutputEventGroup::End),
                            variables_reference: None,
                            source: None,
                            line: None,
                            column: None,
                            data: None,
                        }))
                        .await?;

                    Ok(ResponseBody::Next)
                }
            }
        }
        Command::Pause(_) => todo!(),
        Command::ReadMemory(arg) => {
            let runtime = ctx.0.lock().unwrap();

            Ok(ResponseBody::ReadMemory(ReadMemoryResponse {
                address: "0x0".to_string(),
                data: Some(
                    BASE64_STANDARD
                        .encode(&runtime.memory[0..(arg.count as usize).min(runtime.memory.len())]),
                ),
                unreadable_bytes: None,
            }))
        }
        Command::Restart(_) => todo!(),
        Command::RestartFrame(_) => todo!(),
        Command::ReverseContinue(_) => todo!(),
        Command::Scopes(arg) => {
            let runtime = ctx.0.lock().unwrap();
            let values = runtime.get_stack_values(arg.frame_id as usize);

            Ok(ResponseBody::Scopes(ScopesResponse {
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
                        named_variables: Some(values.len() as i64),
                        indexed_variables: None,
                        expensive: false,
                        source: None,
                        line: None,
                        column: None,
                        end_line: None,
                        end_column: None,
                    },
                ],
            }))
        }
        Command::SetBreakpoints(arg) => {
            let std_content = compiler::Compiler::create_input(String::new());
            let std_content_len = std_content.len();

            let mut runtime = ctx.0.lock().unwrap();

            let mut breakpoints = vec![];
            if let Some(bps) = &arg.breakpoints {
                for bp in bps {
                    let position = compiler::Compiler::find_line_and_column_with_input(
                        &runtime.source_code,
                        bp.line as usize,
                        bp.column.unwrap_or(0) as usize,
                    );

                    breakpoints.push(position + std_content_len);
                }
            }

            runtime.set_breakpoints(breakpoints.clone());

            Ok(ResponseBody::SetBreakpoints(SetBreakpointsResponse {
                breakpoints: breakpoints
                    .into_iter()
                    .enumerate()
                    .map(|(i, pos)| Breakpoint {
                        id: Some(i as i64),
                        verified: true,
                        message: Some(format!("{}", pos)),
                        source: None,
                        line: None,
                        column: None,
                        end_line: None,
                        end_column: None,
                        instruction_reference: None,
                        offset: None,
                    })
                    .collect(),
            }))
        }
        Command::SetDataBreakpoints(_) => todo!(),
        Command::SetExceptionBreakpoints(_) => Ok(ResponseBody::SetExceptionBreakpoints(
            SetExceptionBreakpointsResponse { breakpoints: None },
        )),
        Command::SetExpression(_) => todo!(),
        Command::SetFunctionBreakpoints(_) => todo!(),
        Command::SetInstructionBreakpoints(_) => todo!(),
        Command::SetVariable(_) => todo!(),
        Command::Source(_) => {
            let runtime = ctx.0.lock().unwrap();

            Ok(ResponseBody::Source(SourceResponse {
                content: runtime.source_code.clone(),
                mime_type: None,
            }))
        }
        Command::StackTrace(_) => {
            let runtime = ctx.0.lock().unwrap();
            let frames = runtime.get_stack_frames();

            Ok(ResponseBody::StackTrace(StackTraceResponse {
                total_frames: Some(frames.len() as i64),
                stack_frames: frames
                    .into_iter()
                    .enumerate()
                    .map(|(i, frame)| StackFrame {
                        id: i as i64,
                        name: format!("<stackframe:#{:x?}>", frame),
                        source: Some(Source {
                            name: Some(runtime.source_file.clone()),
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
                        instruction_pointer_reference: None,
                    })
                    .collect(),
            }))
        }
        Command::StepBack(_) => todo!(),
        Command::StepIn(_) => todo!(),
        Command::StepInTargets(_) => todo!(),
        Command::StepOut(_) => todo!(),
        Command::Terminate(_) => todo!(),
        Command::TerminateThreads(_) => todo!(),
        Command::Threads => Ok(ResponseBody::Threads(ThreadsResponse {
            threads: vec![Thread {
                id: MAIN_THREAD_ID,
                name: "main".to_string(),
            }],
        })),
        Command::Variables(arg) => {
            let runtime = ctx.0.lock().unwrap();

            match arg.variables_reference {
                1 => Ok(ResponseBody::Variables(VariablesResponse {
                    variables: vec![
                        Variable {
                            name: "pc".to_string(),
                            value: runtime.pc.to_string(),
                            type_field: Some("int".to_string()),
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
                            type_field: Some("int".to_string()),
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
                            type_field: Some("int".to_string()),
                            presentation_hint: None,
                            evaluate_name: None,
                            variables_reference: 0,
                            named_variables: None,
                            indexed_variables: None,
                            memory_reference: None,
                        },
                    ],
                })),
                _ => {
                    let values = runtime.get_stack_values(arg.variables_reference as usize - 2);

                    Ok(ResponseBody::Variables(VariablesResponse {
                        variables: values
                            .into_iter()
                            .enumerate()
                            .map(|(i, value)| Variable {
                                name: format!("#{}", i),
                                value: format!("{:?}", value),
                                type_field: Some("int".to_string()),
                                presentation_hint: None,
                                evaluate_name: None,
                                variables_reference: 0,
                                named_variables: None,
                                indexed_variables: None,
                                memory_reference: None,
                            })
                            .collect(),
                    }))
                }
            }
        }
        Command::WriteMemory(_) => todo!(),
        Command::Cancel(_) => todo!(),
    }
}

impl DapServer<DapContext> for DapImpl {
    fn handle_request(
        ctx: DapContext,
        sender: SimpleSender<Sendable, Event>,
        req: Command,
    ) -> FutureResult<ResponseBody> {
        Box::pin(dap_handler(ctx, sender, req))
    }
}
