use anyhow::Result;
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

#[derive(Serialize)]
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
