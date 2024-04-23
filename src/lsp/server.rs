use std::{error::Error, sync::Arc};

use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
    sync::mpsc::Sender,
};

use crate::{
    lsp::RpcMessageRequest,
    net::read_headers,
    sender::SimpleSender,
    server::{FutureResult, ServerProcess},
};

use super::{NotificationMessage, RpcMessageResponse};

#[derive(Clone)]
pub struct Lsp<I>(I);

impl<I> Lsp<I> {
    pub fn new(i: I) -> Self {
        Lsp(i)
    }
}

pub trait LspServer {
    fn handle_request(
        req: RpcMessageRequest,
        sender: SimpleSender<String, NotificationMessage>,
    ) -> FutureResult<Option<RpcMessageResponse>>;
}

impl<C: Clone, I: LspServer + Sync + Send + Clone + 'static> ServerProcess<C> for Lsp<I> {
    fn handle(self, _ctx: C, stream: TcpStream) -> FutureResult<()> {
        let (mut reader, mut writer) = tokio::io::split(stream);

        tokio::spawn(async move {
            let (sender, mut receiver) = tokio::sync::mpsc::channel::<String>(100);
            tokio::spawn(async move {
                while let Some(body) = receiver.recv().await {
                    let rcp_event = format!("Content-Length: {}\r\n\r\n{}", body.len(), body);

                    writer.write(rcp_event.as_bytes()).await?;
                }

                Ok::<_, anyhow::Error>(())
            });

            let simple_sender = SimpleSender::new(
                sender.clone(),
                Arc::new(|notification| {
                    let str = serde_json::to_string(&notification).unwrap();

                    println!("+ notify: {}..", str.chars().take(80).collect::<String>());

                    str
                }),
            );

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

                    let req = serde_json::from_str::<RpcMessageRequest>(&content_part).unwrap();
                    println!(
                        "> req: method={}, params={}..",
                        req.method,
                        req.params.to_string().chars().take(80).collect::<String>()
                    );

                    let resp = I::handle_request(req, simple_sender.clone()).await.unwrap();
                    if let Some(resp) = resp {
                        let resp_body = serde_json::to_string(&resp).unwrap();
                        println!(
                            "< resp: {}..",
                            resp.result.to_string().chars().take(80).collect::<String>()
                        );

                        sender.send(resp_body).await?;
                    }

                    Ok::<_, Box<dyn Error + Sync + Send>>(())
                } {
                    eprintln!("failed to process stream; err = {:?}", err);
                    break;
                }
            }

            Ok::<_, anyhow::Error>(())
        });

        Box::pin(async { Ok(()) })
    }
}
