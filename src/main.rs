use std::{error::Error, io::Read};

use clap::{Parser, Subcommand};
use compiler::{lexer::Lexeme, CompilerError};
use lsp::{
    Diagnostic, DiagnosticOptions, DocumentDiagnosticReportKind, FullDocumentDiagnosticReport,
    InitializeResult, Position, Range, RelatedFullDocumentDiagnosticReport, RpcMessageRequest,
    RpcMessageResponse, SemanticTokenTypes, SemanticTokens, SemanticTokensData,
    SemanticTokensLegend, SemanticTokensOptions, ServerCapabilities, TextDocumentSyncKind,
};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpListener,
};

use crate::{
    compiler::{ast::Module, vm::Instruction},
    lsp::{Location, TextDocumentPositionParams},
};

mod compiler;
mod lsp;

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
            run_server().await.unwrap();
        }
    }
}

async fn run_server() -> Result<(), Box<dyn Error + Sync + Send>> {
    let listener = TcpListener::bind("127.0.0.1:3030").await?;
    println!("Listening on http://127.0.0.1:3030");

    loop {
        let (stream, _) = listener.accept().await?;
        let (mut reader, mut writer) = tokio::io::split(stream);

        tokio::spawn(async move {
            println!("tokio:spawn");

            loop {
                if let Err(err) = process_stream(&mut reader, &mut writer).await {
                    eprintln!("failed to process stream; err = {:?}", err);
                    break;
                }
            }

            println!("tokio:spawn end");
        });
    }
}

async fn read_headers(
    reader: &mut (impl AsyncReadExt + Unpin),
) -> Result<Vec<(String, String)>, Box<dyn Error + Sync + Send>> {
    let mut data = vec![];
    while !data.ends_with("\r\n\r\n".as_bytes()) {
        let mut buf = vec![0; 1];
        let n = reader.read(&mut buf).await?;
        if n == 0 {
            return Err("UnexpectedEof".into());
        }

        data.push(buf[0]);
    }

    Ok(parse_headers(&String::from_utf8(data)?))
}

async fn process_stream(
    reader: &mut (impl AsyncReadExt + Unpin),
    writer: &mut (impl AsyncWriteExt + Unpin),
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let headers = read_headers(reader).await?;
    let length = headers
        .into_iter()
        .find(|(key, _)| key == "Content-Length")
        .ok_or("Content-Length is not found in headers".to_string())?
        .1
        .parse::<usize>()
        .map_err(|err| format!("Cannot parse content-length: {}", err))?;

    let mut content = vec![0; length];
    reader.read(&mut content).await?;

    let content_part = String::from_utf8(content)?;
    println!(
        "!content={}",
        if content_part.len() > 100 {
            format!("{}..", content_part[..100].to_string())
        } else {
            format!("{}", content_part)
        },
    );

    let req = serde_json::from_str::<RpcMessageRequest>(&content_part)?;

    let resp = handle_request(req).await.unwrap();
    if let Some(resp) = resp {
        let resp_body = serde_json::to_string(&resp)?;
        let rcp_resp = format!("Content-Length: {}\r\n\r\n{}", resp_body.len(), resp_body);
        writer.write(rcp_resp.as_bytes()).await?;

        println!("ok; resp={}", resp_body);
    }

    Ok::<_, Box<dyn Error + Sync + Send>>(())
}

fn parse_headers(headers: &str) -> Vec<(String, String)> {
    headers
        .split("\r\n")
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut parts = line.split(": ");
            let key = parts.next().unwrap().to_string();
            let value = parts.next().unwrap().to_string();

            (key, value)
        })
        .collect()
}

#[test]
fn test_parse_headers() {
    let cases = vec![
        (
            "Content-Length: 10\r\nContent-Type: application/json",
            vec![
                ("Content-Length".to_string(), "10".to_string()),
                ("Content-Type".to_string(), "application/json".to_string()),
            ],
        ),
        (
            "Content-Length: 10\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n",
            vec![
                ("Content-Length".to_string(), "10".to_string()),
                (
                    "Content-Type".to_string(),
                    "application/vscode-jsonrpc; charset=utf-8".to_string(),
                ),
            ],
        ),
    ];

    for (input, expected) in cases {
        assert_eq!(parse_headers(input), expected);
    }
}

async fn handle_request(
    req: RpcMessageRequest,
) -> Result<Option<RpcMessageResponse>, Box<dyn Error>> {
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
