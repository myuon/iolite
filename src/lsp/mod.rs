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
