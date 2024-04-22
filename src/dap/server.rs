use std::error::Error;

use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
    sync::mpsc::Sender,
};

use crate::{
    net::read_headers,
    server::{FutureResult, ServerProcess},
};

use super::{ProtocolMessageEventBuilder, ProtocolMessageRequest, ProtocolMessageResponse};

#[derive(Clone)]
pub struct Dap<I>(I);

impl<I> Dap<I> {
    pub fn new(i: I) -> Self {
        Dap(i)
    }
}

pub trait DapServer<C> {
    fn handle_request(
        context: C,
        sender: Sender<ProtocolMessageEventBuilder>,
        req: ProtocolMessageRequest,
    ) -> FutureResult<Vec<ProtocolMessageResponse>>;
}

impl<C: Sync + Send + Clone + 'static, I: DapServer<C> + Sync + Send + Clone + 'static>
    ServerProcess<C> for Dap<I>
{
    fn handle(self, ctx: C, stream: TcpStream) -> FutureResult<()> {
        let (mut reader, mut writer) = tokio::io::split(stream);

        tokio::spawn(async move {
            println!("tokio:spawn");

            let (sender, mut receiver) =
                tokio::sync::mpsc::channel::<ProtocolMessageEventBuilder>(100);

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

                    let req =
                        serde_json::from_str::<ProtocolMessageRequest>(&content_part).unwrap();
                    println!("> received: {}", req.command);

                    let resps = I::handle_request(ctx.clone(), sender.clone(), req)
                        .await
                        .unwrap();
                    for resp in resps {
                        let resp_body = serde_json::to_string(&resp).unwrap();
                        let rcp_resp =
                            format!("Content-Length: {}\r\n\r\n{}", resp_body.len(), resp_body);
                        writer.write(rcp_resp.as_bytes()).await.unwrap();

                        println!("< respond: {}", resp.command.unwrap());
                    }

                    Ok::<_, Box<dyn Error + Sync + Send>>(())
                } {
                    eprintln!("failed to process stream; err = {:?}", err);
                    break;
                }

                while let Ok(event) = receiver.try_recv() {
                    let event_body = serde_json::to_string(&event).unwrap();
                    let rcp_event =
                        format!("Content-Length: {}\r\n\r\n{}", event_body.len(), event_body);
                    writer.write(rcp_event.as_bytes()).await.unwrap();

                    println!("+ send: {}", event.event.to_string());
                }
            }

            println!("tokio:spawn end");
        });

        Box::pin(async { Ok(()) })
    }
}
