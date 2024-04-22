use std::{error::Error, sync::Arc};

use anyhow::anyhow;
use dap::{
    base_message::Sendable,
    events::Event,
    requests::{Command, Request},
    responses::ResponseBody,
};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
    sync::mpsc::Sender,
};

use crate::{
    net::read_headers,
    server::{FutureResult, ServerProcess},
};

pub struct SimpleSender<T, U> {
    sender: Sender<T>,
    mapper: SimpleSenderFn<T, U>,
}

impl<T, U> Clone for SimpleSender<T, U> {
    fn clone(&self) -> Self {
        SimpleSender {
            sender: self.sender.clone(),
            mapper: self.mapper.clone(),
        }
    }
}

type SimpleSenderFn<T, U> = Arc<dyn Fn(U) -> T + Sync + Send + 'static>;

impl<T, U> SimpleSender<T, U> {
    pub fn new(sender: Sender<T>, mapper: SimpleSenderFn<T, U>) -> Self {
        SimpleSender { sender, mapper }
    }

    pub async fn send(&self, u: U) -> Result<(), tokio::sync::mpsc::error::SendError<T>> {
        self.sender.send((self.mapper)(u)).await
    }
}

#[derive(Clone)]
pub struct Dap<I>(I);

impl<I> Dap<I> {
    pub fn new(i: I) -> Self {
        Dap(i)
    }
}

pub trait DapServer<C> {
    fn handle_request(
        ctx: C,
        sender: SimpleSender<Sendable, Event>,
        req: Command,
    ) -> FutureResult<ResponseBody>;
}

impl<C: Sync + Send + Clone + 'static, I: DapServer<C> + Sync + Send + Clone + 'static>
    ServerProcess<C> for Dap<I>
{
    fn handle(self, ctx: C, stream: TcpStream) -> FutureResult<()> {
        let (mut reader, mut writer) = tokio::io::split(stream);

        tokio::spawn(async move {
            let (sender, mut receiver) = tokio::sync::mpsc::channel::<Sendable>(100);
            tokio::spawn(async move {
                while let Some(sendable) = receiver.recv().await {
                    let body = serde_json::to_string(&sendable)?;
                    let rcp_event = format!("Content-Length: {}\r\n\r\n{}", body.len(), body);

                    writer.write(rcp_event.as_bytes()).await?;
                }

                Ok::<_, anyhow::Error>(())
            });

            let simple_sender = SimpleSender::new(
                sender.clone(),
                Arc::new(|event| {
                    println!(
                        "+ event: {}..",
                        serde_json::to_string(&event)
                            .unwrap()
                            .chars()
                            .take(80)
                            .collect::<String>()
                    );

                    Sendable::Event(event)
                }),
            );

            loop {
                if let Err(err) = {
                    let headers = read_headers(&mut reader).await?;
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

                    let req = serde_json::from_str::<Request>(&content_part)?;
                    println!(
                        "> received: {}..",
                        content_part.chars().take(80).collect::<String>()
                    );

                    let resp =
                        I::handle_request(ctx.clone(), simple_sender.clone(), req.command.clone())
                            .await?;

                    println!(
                        "< respond: {}..",
                        serde_json::to_string(&resp)?
                            .chars()
                            .take(80)
                            .collect::<String>()
                    );

                    sender.send(Sendable::Response(req.success(resp))).await?;

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
