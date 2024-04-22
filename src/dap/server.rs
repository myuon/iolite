use std::error::Error;

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

#[derive(Clone)]
pub struct Dap<I>(I);

impl<I> Dap<I> {
    pub fn new(i: I) -> Self {
        Dap(i)
    }
}

pub trait DapServer<C> {
    fn handle_request(ctx: C, sender: Sender<Event>, req: Command) -> FutureResult<ResponseBody>;
}

impl<C: Sync + Send + Clone + 'static, I: DapServer<C> + Sync + Send + Clone + 'static>
    ServerProcess<C> for Dap<I>
{
    fn handle(self, ctx: C, stream: TcpStream) -> FutureResult<()> {
        let (mut reader, mut writer) = tokio::io::split(stream);

        tokio::spawn(async move {
            println!("tokio:spawn");

            let (sender, mut receiver) = tokio::sync::mpsc::channel::<Event>(100);

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

                    let req = serde_json::from_str::<Request>(&content_part).unwrap();
                    println!(
                        "> received: {}..",
                        content_part.chars().take(80).collect::<String>()
                    );

                    let resp = I::handle_request(ctx.clone(), sender.clone(), req.command.clone())
                        .await
                        .unwrap();

                    println!(
                        "< respond: {}..",
                        serde_json::to_string(&resp)
                            .unwrap()
                            .chars()
                            .take(80)
                            .collect::<String>()
                    );

                    let resp_body =
                        serde_json::to_string(&Sendable::Response(req.success(resp))).unwrap();

                    let rcp_resp =
                        format!("Content-Length: {}\r\n\r\n{}", resp_body.len(), resp_body);
                    writer.write(rcp_resp.as_bytes()).await.unwrap();

                    Ok::<_, Box<dyn Error + Sync + Send>>(())
                } {
                    eprintln!("failed to process stream; err = {:?}", err);
                    break;
                }

                while let Ok(event) = receiver.try_recv() {
                    let event_body = serde_json::to_string(&Sendable::Event(event)).unwrap();

                    println!(
                        "+ send: {}..",
                        event_body.chars().take(80).collect::<String>()
                    );

                    let rcp_event =
                        format!("Content-Length: {}\r\n\r\n{}", event_body.len(), event_body);
                    writer.write(rcp_event.as_bytes()).await.unwrap();
                }
            }

            println!("tokio:spawn end");
        });

        Box::pin(async { Ok(()) })
    }
}
