use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::compiler::parser;

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
    pub text_document_sync: Option<TextDocumentSyncKind>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_tokens_provider: Option<SemanticTokensOptions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostic_provider: Option<DiagnosticOptions>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub enum TextDocumentSyncKind {
    None = 0,
    Full = 1,
    Incremental = 2,
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
pub struct DiagnosticOptions {
    pub inter_file_dependencies: bool,
    pub workspace_diagnostics: bool,
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

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Hash)]
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

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PublishDiagnosticsParams {
    pub uri: DocumentUri,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Diagnostic {
    pub range: Range,
    pub message: String,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub enum DocumentDiagnosticReportKind {
    Full,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FullDocumentDiagnosticReport {
    pub kind: DocumentDiagnosticReportKind,
    pub items: Vec<Diagnostic>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RelatedFullDocumentDiagnosticReport {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub related_documents: Option<HashMap<DocumentUri, FullDocumentDiagnosticReport>>,
}
