use std::path::Path;

use anyhow::Result;
use lsp_types::{
    notification::{DidSaveTextDocument, Initialized, Notification, PublishDiagnostics},
    request::{
        DocumentDiagnosticRequest, GotoDefinition, HoverRequest, Initialize, InlayHintRequest,
        Request, SemanticTokensFullRequest,
    },
    DeclarationCapability, Diagnostic, DiagnosticOptions, DiagnosticServerCapabilities,
    DiagnosticSeverity, FullDocumentDiagnosticReport, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeResult, InlayHint, InlayHintLabel, InlayHintParams,
    Location, MarkedString, OneOf, Position, PublishDiagnosticsParams, Range,
    RelatedFullDocumentDiagnosticReport, SemanticToken, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensServerCapabilities, ServerCapabilities, TextDocumentPositionParams,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};

use crate::{
    compiler::{
        self,
        ast::{
            AstWalker, AstWalkerMode, Span, AST_WALKER_FIELD, AST_WALKER_FUNCTION,
            AST_WALKER_KEYWORD, AST_WALKER_METHOD, AST_WALKER_NAMESPACE, AST_WALKER_TYPE,
        },
        lexer::LexerError,
        parser::ParseError,
        typechecker::TypecheckerError,
        CompilerError,
    },
    utils::{
        lsp::{server::LspServer, NotificationMessage, RpcMessageRequest, RpcMessageResponse},
        sender::SimpleSender,
        server_process::FutureResult,
    },
};

#[derive(Clone)]
pub struct LspImpl;

impl LspServer for LspImpl {
    fn handle_request(
        req: RpcMessageRequest,
        sender: SimpleSender<String, NotificationMessage>,
    ) -> FutureResult<Option<RpcMessageResponse>> {
        Box::pin(lsp_handler(req, sender))
    }
}

async fn lsp_handler(
    req: RpcMessageRequest,
    sender: SimpleSender<String, NotificationMessage>,
) -> Result<Option<RpcMessageResponse>> {
    let token_types = vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::METHOD,
        SemanticTokenType::TYPE,
        SemanticTokenType::KEYWORD,
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
                                        AST_WALKER_NAMESPACE => SemanticTokenType::NAMESPACE,
                                        AST_WALKER_KEYWORD => SemanticTokenType::KEYWORD,
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
            let cwd = Path::new(&path).parent().unwrap().to_str().unwrap();
            compiler.set_cwd(cwd.to_string());

            let path = Path::new(&path)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
                .replace(".io", "");

            match compiler
                .parse(path.to_string())
                .and_then(|_| Ok(compiler.typecheck(path.to_string())?))
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
                    let message = format!("{}", err);
                    let span = err.as_span();
                    let range = compiler.find_span(&span)?;
                    let (start, end) = range.unwrap();

                    sender
                        .send(NotificationMessage::new::<PublishDiagnostics>(
                            PublishDiagnosticsParams {
                                uri: Url::parse(&format!(
                                    "file://{}/{}.io",
                                    cwd,
                                    span.module_name.unwrap()
                                ))?,
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
            let cwd = Path::new(&path).parent().unwrap().to_str().unwrap();

            let mut compiler = compiler::Compiler::new();
            compiler.set_cwd(cwd.to_string());

            let module_name = Path::new(&path)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
                .replace(".io", "");

            match compiler.parse(module_name.clone()) {
                Ok(_) => {}
                Err(err) => {
                    eprintln!("{:?}", err);
                    return Ok(None);
                }
            };

            let position = match compiler.find_line_and_column(
                &module_name,
                params.position.line as usize,
                params.position.character as usize,
            ) {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!("{:?}", err);
                    return Ok(None);
                }
            };
            let def_position = match compiler.search_for_definition(module_name.clone(), position) {
                Ok(pos) => pos,
                Err(err) => {
                    eprintln!("{:?}", err);
                    return Ok(None);
                }
            };

            if let Some(def_position) = def_position {
                let (start, end) = compiler.find_span(&def_position)?.unwrap();

                return Ok(Some(RpcMessageResponse::new(
                    req.id,
                    Location {
                        uri: Url::parse(&format!(
                            "file://{}/{}.io",
                            cwd,
                            def_position.module_name.unwrap()
                        ))?,
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
                    },
                )?));
            } else {
                eprintln!("Definition not found, {}:{}", module_name, position);
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
            if let Err(err) = compiler.parse(module_name.clone()) {
                eprintln!("{:?}", err);
                return Ok(None);
            }

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
            if let Err(err) = compiler.parse(module_name.clone()) {
                eprintln!("{:?}", err);
                return Ok(None);
            }
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lsp_types::{
        DidSaveTextDocumentParams, GotoDefinitionParams, PartialResultParams,
        TextDocumentIdentifier, Url, WorkDoneProgressParams,
    };
    use pretty_assertions::assert_eq;

    use super::*;

    #[tokio::test]
    async fn test_lsp_handler_diagnostics() -> Result<()> {
        let cases = vec![
            (
                "test1.io",
                vec![(
                    "test1",
                    "Typechecker error: Type mismatch: expected Int, but got Array(Byte)",
                    Range {
                        start: Position {
                            line: 3,
                            character: 11,
                        },
                        end: Position {
                            line: 3,
                            character: 12,
                        },
                    },
                )],
            ),
            (
                "test2.io",
                vec![(
                    "test2",
                    "Parse error: unexpected token",
                    Range {
                        start: Position {
                            line: 1,
                            character: 8,
                        },
                        end: Position {
                            line: 1,
                            character: 11,
                        },
                    },
                )],
            ),
            (
                "test3.io",
                vec![(
                    "test3",
                    "Lexer error: invalid character",
                    Range {
                        start: Position {
                            line: 1,
                            character: 8,
                        },
                        end: Position {
                            line: 1,
                            character: 9,
                        },
                    },
                )],
            ),
            (
                "test4/main.io",
                vec![(
                    "test4/lib",
                    "Type mismatch: expected Int, but got Array(Byte)",
                    Range {
                        start: Position {
                            line: 1,
                            character: 11,
                        },
                        end: Position {
                            line: 1,
                            character: 12,
                        },
                    },
                )],
            ),
        ];

        for (file, checks) in cases {
            let req = RpcMessageRequest {
                id: None,
                method: DidSaveTextDocument::METHOD.to_string(),
                params: serde_json::to_value(&DidSaveTextDocumentParams {
                    text_document: TextDocumentIdentifier::new(Url::parse(&format!(
                        "file://{}",
                        std::env::current_dir()?
                            .join(format!("tests/lsp/diagnostics/{}", file))
                            .to_str()
                            .unwrap()
                    ))?),
                    text: None,
                })?,
                jsonrpc: "".to_string(),
            };
            let (sender, mut receiver) = tokio::sync::mpsc::channel::<String>(100);
            let sender =
                SimpleSender::new(sender, Arc::new(|m| serde_json::to_string(&m).unwrap()));
            let res = lsp_handler(req, sender).await?;
            assert!(res.is_none());

            let r = receiver.recv().await.unwrap();
            let message = serde_json::from_str::<'_, NotificationMessage>(&r)?;
            let params =
                serde_json::from_value::<PublishDiagnosticsParams>(message.params.clone())?;

            for (i, (module_name, err, range)) in checks.iter().enumerate() {
                assert_eq!(
                    params.uri.to_file_path().unwrap().to_str().unwrap(),
                    std::env::current_dir()?
                        .join(format!("tests/lsp/diagnostics/{}.io", module_name))
                        .to_str()
                        .unwrap(),
                    "{}",
                    serde_json::to_string(&params)?
                );
                assert!(
                    params.diagnostics[i].message.contains(*err),
                    "Failed: {}.contains({})",
                    params.diagnostics[i].message,
                    *err,
                );
                assert_eq!(params.diagnostics[i].range, *range, "file: {}", file);
            }
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_lsp_handler_go_to_definition() -> Result<()> {
        let cases = vec![
            (
                "test1.io",
                Position {
                    line: 9,
                    character: 10,
                },
                (
                    "test1",
                    Range {
                        start: Position {
                            line: 4,
                            character: 4,
                        },
                        end: Position {
                            line: 4,
                            character: 5,
                        },
                    },
                ),
            ),
            (
                "test1.io",
                Position {
                    line: 5,
                    character: 9,
                },
                (
                    "test1",
                    Range {
                        start: Position {
                            line: 0,
                            character: 4,
                        },
                        end: Position {
                            line: 0,
                            character: 5,
                        },
                    },
                ),
            ),
            (
                "test2/main.io",
                Position {
                    line: 3,
                    character: 12,
                },
                (
                    "test2/lib",
                    Range {
                        start: Position {
                            line: 0,
                            character: 4,
                        },
                        end: Position {
                            line: 0,
                            character: 8,
                        },
                    },
                ),
            ),
        ];

        for (file, position, result) in cases {
            let req = RpcMessageRequest {
                id: None,
                method: GotoDefinition::METHOD.to_string(),
                params: serde_json::to_value(&GotoDefinitionParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier::new(Url::parse(&format!(
                            "file://{}",
                            std::env::current_dir()?
                                .join(format!("tests/lsp/gotodefinition/{}", file))
                                .to_str()
                                .unwrap()
                        ))?),
                        position,
                    },
                    work_done_progress_params: WorkDoneProgressParams {
                        work_done_token: None,
                    },
                    partial_result_params: PartialResultParams {
                        partial_result_token: None,
                    },
                })?,
                jsonrpc: "".to_string(),
            };
            let (sender, mut receiver) = tokio::sync::mpsc::channel::<String>(100);
            let sender =
                SimpleSender::new(sender, Arc::new(|m| serde_json::to_string(&m).unwrap()));
            let res = lsp_handler(req, sender).await?;

            assert_eq!(
                serde_json::from_value::<Location>(res.unwrap().result)?,
                Location {
                    uri: Url::parse(&format!(
                        "file://{}",
                        std::env::current_dir()?
                            .join(format!("tests/lsp/gotodefinition/{}.io", result.0))
                            .to_str()
                            .unwrap()
                    ))?,
                    range: result.1,
                },
            );

            let r = receiver.try_recv();
            assert!(r.is_err());
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_lsp_handler_inlay_hints() -> Result<()> {
        let cases = vec![(
            "test1.io",
            Range {
                start: Position {
                    line: 9,
                    character: 10,
                },
                end: Position {
                    line: 9,
                    character: 10,
                },
            },
            vec![
                InlayHint {
                    position: Position {
                        line: 1,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": int".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 2,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": bool".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 3,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": float".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 4,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": array[byte]".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 5,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": array[int]".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 6,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": nil".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 7,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": ptr[int]".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 8,
                        character: 7,
                    },
                    label: InlayHintLabel::String(": byte".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
                InlayHint {
                    position: Position {
                        line: 0,
                        character: 10,
                    },
                    label: InlayHintLabel::String(": nil".to_string()),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                },
            ],
        )];

        for (file, range, result) in cases {
            let req = RpcMessageRequest {
                id: None,
                method: InlayHintRequest::METHOD.to_string(),
                params: serde_json::to_value(&InlayHintParams {
                    work_done_progress_params: WorkDoneProgressParams {
                        work_done_token: None,
                    },
                    text_document: TextDocumentIdentifier::new(Url::parse(&format!(
                        "file://{}",
                        std::env::current_dir()?
                            .join(format!("tests/lsp/inlayhints/{}", file))
                            .to_str()
                            .unwrap()
                    ))?),
                    range,
                })?,
                jsonrpc: "".to_string(),
            };
            let (sender, mut receiver) = tokio::sync::mpsc::channel::<String>(100);
            let sender =
                SimpleSender::new(sender, Arc::new(|m| serde_json::to_string(&m).unwrap()));
            let res = lsp_handler(req, sender).await?;

            assert_eq!(
                res.unwrap().result,
                serde_json::to_value::<Vec<InlayHint>>(result)?,
            );

            let r = receiver.try_recv();
            assert!(r.is_err());
        }

        Ok(())
    }
}
