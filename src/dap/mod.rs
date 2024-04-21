use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Serialize, Deserialize)]
pub enum ProtocolMessageTypePredefined {
    Request,
    Response,
    Event,
}

#[derive(Serialize, Deserialize)]
pub struct ProtocolMessageRequest {
    #[serde(rename = "type")]
    ty: String, // "request"
    seq: usize,
    command: String,
    arguments: Value,
}

#[derive(Serialize, Deserialize)]
pub struct ProtocolMessageResponse {
    #[serde(rename = "type")]
    ty: String, // "response"
    request_seq: usize,
    success: bool,
    command: String,
    message: String,
    body: Value,
}
