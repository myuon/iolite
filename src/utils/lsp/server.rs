use std::sync::Arc;

use anyhow::anyhow;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};

use crate::{
    utils::net::read_headers,
    utils::sender::SimpleSender,
    utils::server_process::{FutureResult, ServerProcess},
};

use super::{NotificationMessage, RpcMessageRequest, RpcMessageResponse};

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
                Arc::new(|notification: NotificationMessage| {
                    let str = serde_json::to_string(&notification).unwrap();

                    println!(
                        "+ notify: method={} params={}..",
                        notification.method,
                        serde_json::to_string(&notification.params)
                            .unwrap()
                            .chars()
                            .take(80)
                            .collect::<String>()
                    );

                    str
                }),
            );

            loop {
                if let Err(err) = {
                    let headers = read_headers(&mut reader).await.unwrap();
                    let length = headers
                        .into_iter()
                        .find(|(key, _)| key == "Content-Length")
                        .ok_or(anyhow!("Content-Length is not found in headers"))?
                        .1
                        .parse::<usize>()
                        .map_err(|err| anyhow!("Cannot parse content-length: {}", err))?;

                    let mut content = vec![0; length];
                    reader.read(&mut content).await?;

                    let content_part = String::from_utf8(content)?;

                    let req = serde_json::from_str::<RpcMessageRequest>(&content_part)?;
                    println!(
                        "> req: method={}, params={}..",
                        req.method,
                        req.params.to_string().chars().take(80).collect::<String>()
                    );

                    let resp = I::handle_request(req, simple_sender.clone()).await?;
                    if let Some(resp) = resp {
                        let resp_body = serde_json::to_string(&resp)?;
                        println!(
                            "< resp: {}..",
                            resp.result.to_string().chars().take(80).collect::<String>()
                        );

                        sender.send(resp_body).await?;
                    }

                    Ok::<_, anyhow::Error>(())
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
