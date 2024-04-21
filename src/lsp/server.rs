use std::error::Error;

use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};

use crate::{
    lsp::RpcMessageRequest,
    net::read_headers,
    server::{FutureResult, ServerProcess},
};

use super::RpcMessageResponse;

#[derive(Clone)]
pub struct Lsp<I>(I);

impl<I> Lsp<I> {
    pub fn new(i: I) -> Self {
        Lsp(i)
    }
}

pub trait LspServer {
    fn handle_request(req: RpcMessageRequest) -> FutureResult<Option<RpcMessageResponse>>;
}

impl<C: Clone, I: LspServer + Sync + Send + Clone + 'static> ServerProcess<C> for Lsp<I> {
    fn handle(self, ctx: C, stream: TcpStream) -> FutureResult<()> {
        let (mut reader, mut writer) = tokio::io::split(stream);

        tokio::spawn(async move {
            println!("tokio:spawn");

            loop {
                if let Err(err) = {
                    let headers = read_headers(&mut reader).await.unwrap();
                    let length = headers
                        .into_iter()
                        .find(|(key, _)| key == "Content-Length")
                        .ok_or("Content-Length is not found in headers".to_string())
                        .unwrap()
                        .1
                        .parse::<usize>()
                        .map_err(|err| format!("Cannot parse content-length: {}", err))
                        .unwrap();

                    let mut content = vec![0; length];
                    reader.read(&mut content).await.unwrap();

                    let content_part = String::from_utf8(content).unwrap();
                    println!(
                        "!content={}",
                        if content_part.len() > 100 {
                            format!("{}..", content_part[..100].to_string())
                        } else {
                            format!("{}", content_part)
                        },
                    );

                    let req = serde_json::from_str::<RpcMessageRequest>(&content_part).unwrap();

                    let resp = I::handle_request(req).await.unwrap();
                    if let Some(resp) = resp {
                        let resp_body = serde_json::to_string(&resp).unwrap();
                        let rcp_resp =
                            format!("Content-Length: {}\r\n\r\n{}", resp_body.len(), resp_body);
                        writer.write(rcp_resp.as_bytes()).await.unwrap();

                        println!("ok; resp={}", resp_body);
                    }

                    Ok::<_, Box<dyn Error + Sync + Send>>(())
                } {
                    eprintln!("failed to process stream; err = {:?}", err);
                    break;
                }
            }

            println!("tokio:spawn end");
        });

        Box::pin(async { Ok(()) })
    }
}
