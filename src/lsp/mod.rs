use anyhow::Result;
use lsp_types::notification::Notification;
use serde::{Deserialize, Serialize};
use serde_json::Value;

pub mod server;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RpcMessageRequest {
    pub jsonrpc: String,
    pub id: Option<Value>,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct RpcMessageResponse {
    pub jsonrpc: String,
    pub id: Option<Value>,
    pub result: Value,
}

impl RpcMessageResponse {
    pub fn new<T: Serialize>(id: Option<Value>, result: T) -> Result<Self> {
        Ok(Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: serde_json::to_value(result)?,
        })
    }
}

#[derive(Serialize, Debug, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NotificationMessage {
    pub jsonrpc: String,
    pub method: String,
    pub params: Value,
}

impl NotificationMessage {
    pub fn new<T: Notification>(params: <T as Notification>::Params) -> Result<Self> {
        Ok(Self {
            jsonrpc: "2.0".to_string(),
            method: T::METHOD.to_string(),
            params: serde_json::to_value(params)?,
        })
    }
}
