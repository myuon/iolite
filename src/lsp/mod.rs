use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Deserialize)]
pub struct RpcMessageRequest {
    pub jsonrpc: String,
    pub id: Option<Value>,
    pub method: String,
}

#[derive(Serialize)]
pub struct RpcMessageResponse {
    pub jsonrpc: String,
    pub id: Option<Value>,
    pub result: Value,
}

#[derive(Serialize)]
pub struct InitializeResult {
    pub capabilities: ServerCapabilities,
}

#[derive(Serialize)]
pub struct ServerCapabilities {}
