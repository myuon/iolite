use std::sync::Arc;

use tokio::sync::mpsc::{error::SendError, Sender};

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

    pub async fn send(&self, u: U) -> Result<(), SendError<T>> {
        self.sender.send((self.mapper)(u)).await
    }
}
