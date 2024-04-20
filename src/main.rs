use std::{error::Error, io::Read};

use clap::{Parser, Subcommand};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpListener,
};

use crate::compiler::{ast::Module, vm::Instruction};

mod compiler;

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

async fn process_stream(
    reader: &mut (impl AsyncReadExt + Unpin),
    writer: &mut (impl AsyncWriteExt + Unpin),
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut header_part_str = vec![];
    let mut content_part_str = vec![];
    let mut content_length = -1;

    loop {
        let mut buf = vec![0; 1024];
        match reader.read(&mut buf).await {
            Ok(0) => {
                println!("connection closed");
                break;
            }
            Ok(n) => {
                let chunk = buf[..n].to_vec();
                if let Some(pos) = find_position(&chunk, &"\r\n\r\n".as_bytes().to_vec()) {
                    header_part_str.extend_from_slice(&chunk[..pos]);

                    let header_part =
                        parse_headers(&String::from_utf8(header_part_str.clone()).unwrap());
                    for (key, value) in header_part {
                        if key == "Content-Length" {
                            content_length = value.parse().unwrap();
                        }
                    }
                    if content_length == -1 {
                        eprintln!("Content-Length is not found");
                        break;
                    }

                    let c = &chunk[pos + 4..];
                    content_part_str.extend_from_slice(&c);
                    content_length -= c.len() as i32;
                } else {
                    if content_length == -1 {
                        header_part_str.extend_from_slice(&chunk);
                    } else {
                        content_part_str.extend_from_slice(&chunk);
                        content_length -= n as i32;
                    }
                }

                if content_length == 0 {
                    break;
                }

                continue;
            }
            Err(e) => {
                eprintln!("failed to read from socket; err = {:?}", e);
                break;
            }
        }
    }

    let content_part = String::from_utf8(content_part_str)?;
    println!(
        "rpc header={}, content_part={}",
        String::from_utf8(header_part_str)?,
        if content_part.len() > 100 {
            format!("{}..", content_part[..100].to_string())
        } else {
            format!("{}", content_part)
        },
    );

    let req = serde_json::from_str::<RpcMessageRequest>(&content_part)?;

    let resp = handle_request(req).await.unwrap();
    let resp_body = serde_json::to_string(&resp)?;

    let rcp_resp = format!("Content-Length: {}\r\n\r\n{}", resp_body.len(), resp_body);

    writer.write(rcp_resp.as_bytes()).await?;

    println!("ok; resp={}", resp_body);

    Ok::<_, Box<dyn Error + Sync + Send>>(())
}

fn find_position<T: PartialEq>(content: &Vec<T>, haystack: &Vec<T>) -> Option<usize> {
    for i in 0..content.len() {
        if content[i..].starts_with(&haystack) {
            return Some(i);
        }
    }

    None
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

#[derive(Deserialize)]
struct RpcMessageRequest {
    jsonrpc: String,
    id: Option<Value>,
    method: String,
}

#[derive(Serialize)]
struct RpcMessageResponse {
    jsonrpc: String,
    id: Option<Value>,
    result: Value,
}

#[derive(Serialize)]
struct InitializeResult {
    capabilities: ServerCapabilities,
}

#[derive(Serialize)]
struct ServerCapabilities {}

async fn handle_request(req: RpcMessageRequest) -> Result<RpcMessageResponse, Box<dyn Error>> {
    Ok(RpcMessageResponse {
        jsonrpc: "2.0".to_string(),
        id: req.id,
        result: serde_json::to_value(&InitializeResult {
            capabilities: ServerCapabilities {},
        })?,
    })
}
