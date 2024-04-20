use serde::{Deserialize, Serialize};
use serde_json::Value;

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

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResult {
    pub capabilities: ServerCapabilities,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerCapabilities {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_tokens_provider: Option<SemanticTokensOptions>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokensOptions {
    pub legend: SemanticTokensLegend,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub full: Option<bool>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokensLegend {
    pub token_types: Vec<SemanticTokenTypes>,
    pub token_modifiers: Vec<SemanticTokenModifiers>,
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum SemanticTokenTypes {
    Namespace,
    Type,
    Class,
    Enum,
    Interface,
    Struct,
    TypeParameter,
    Parameter,
    Variable,
    Property,
    EnumMember,
    Event,
    Function,
    Method,
    Macro,
    Keyword,
    Modifier,
    Comment,
    String,
    Number,
    Regexp,
    Operator,
    Decorator,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub enum SemanticTokenModifiers {
    Declaration,
    Definition,
    Readonly,
    Static,
    Deprecated,
    Abstract,
    Async,
    Modification,
    Documentation,
    DefaultLibrary,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokens {
    pub data: Vec<usize>,
}

pub struct SemanticTokensData {
    pub line_delta: usize,
    pub char_delta: usize,
    pub ty: SemanticTokenTypes,
    pub length: usize,
}

impl SemanticTokens {
    pub fn encode_data(data: Vec<SemanticTokensData>, tys: Vec<SemanticTokenTypes>) -> Self {
        let mut result = Vec::new();

        for SemanticTokensData {
            line_delta,
            char_delta,
            ty,
            length,
        } in data
        {
            result.push(line_delta - 1);
            result.push(char_delta - 1);
            result.push(tys.iter().position(|x| x == &ty).unwrap());
            result.push(length);
            result.push(0); // modifiers
        }

        SemanticTokens { data: result }
    }
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DocumentUri(pub String);

impl DocumentUri {
    pub fn as_filepath(&self) -> Option<&str> {
        self.0.strip_prefix("file://")
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextDocumentIdentifier {
    pub uri: DocumentUri,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SemanticTokensParams {
    pub text_document: TextDocumentIdentifier,
}
