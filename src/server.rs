use std::{error::Error, future::Future, pin::Pin};

use tokio::net::{TcpListener, TcpStream};

pub type FutureResult<T> =
    Pin<Box<dyn Future<Output = Result<T, Box<dyn Error + Sync + Send + 'static>>> + Send>>;

pub trait ServerProcess: Sized + Clone {
    fn handle(self, stream: TcpStream) -> FutureResult<()>;

    async fn start(self, port: usize) -> Result<(), Box<dyn Error + Sync + Send + 'static>> {
        let listener = TcpListener::bind(format!("127.0.0.1:{}", port)).await?;
        println!("Listening on http://127.0.0.1:{}", port);

        loop {
            let (stream, _) = listener.accept().await?;
            self.clone().handle(stream).await?;
        }
    }
}
