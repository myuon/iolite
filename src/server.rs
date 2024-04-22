use std::{future::Future, pin::Pin};

use anyhow::Result;
use tokio::net::{TcpListener, TcpStream};

pub type FutureResult<T> = Pin<Box<dyn Future<Output = Result<T>> + Send>>;

pub trait ServerProcess<C: Clone>: Sized + Clone {
    fn handle(self, ctx: C, stream: TcpStream) -> FutureResult<()>;

    async fn start(self, ctx: C, port: usize) -> Result<()> {
        let listener = TcpListener::bind(format!("127.0.0.1:{}", port)).await?;
        println!("Listening on http://127.0.0.1:{}", port);

        loop {
            let (stream, _) = listener.accept().await?;
            self.clone().handle(ctx.clone(), stream).await?;
        }
    }
}
