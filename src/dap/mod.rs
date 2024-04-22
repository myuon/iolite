use serde::{Deserialize, Serialize};
use serde_json::Value;

pub mod server;

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProtocolMessageType {
    Request,
    Response,
    Event,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct ProtocolMessageRequest {
    #[serde(rename = "type")]
    pub ty: ProtocolMessageType, // "request"
    pub seq: usize,
    pub command: String,
    #[serde(default)]
    pub arguments: Value,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct ProtocolMessageResponse {
    #[serde(rename = "type")]
    pub ty: ProtocolMessageType, // "response"
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_seq: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub success: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub event: Option<ProtocolMessageEventKind>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    pub body: Value,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct ProtocolMessageResponseBuilder {
    pub body: Value,
}

impl ProtocolMessageResponseBuilder {
    pub fn build(self, req: &ProtocolMessageRequest) -> ProtocolMessageResponse {
        ProtocolMessageResponse {
            ty: ProtocolMessageType::Response,
            request_seq: Some(req.seq),
            success: Some(true),
            command: Some(req.command.clone()),
            event: None,
            message: None,
            body: self.body,
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(rename_all = "snake_case")]
pub struct ProtocolMessageEventBuilder {
    pub event: ProtocolMessageEventKind,
    pub body: Value,
}

impl ProtocolMessageEventBuilder {
    pub fn build(self) -> ProtocolMessageResponse {
        ProtocolMessageResponse {
            ty: ProtocolMessageType::Event,
            request_seq: None,
            success: None,
            command: None,
            message: None,
            event: Some(self.event.clone()),
            body: self.body,
        }
    }
}

#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "snake_case")]
pub enum ProtocolMessageEventKind {
    Initialized,
    Stopped,
    Output,
    Exited,
    Terminated,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeResponseBody(pub Capabilities);

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Capabilities {
    pub supports_configuration_done_request: Option<bool>,
    pub supports_function_breakpoints: Option<bool>,
    pub supports_conditional_breakpoints: Option<bool>,
    pub supports_hit_conditional_breakpoints: Option<bool>,
    pub supports_single_thread_execution_requests: Option<bool>,
    pub supports_read_memory_request: Option<bool>,
    pub supports_memory_event: Option<bool>,
    pub supports_disassemble_request: Option<bool>,
    pub supports_breakpoint_locations_request: Option<bool>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LaunchRequestArguments {
    pub no_debug: Option<bool>,
    pub source_file: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LaunchResponse {}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializedEvent {}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetExceptionBreakpointsArguments {
    pub filters: Vec<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SetExceptionBreakpointsResponse {
    pub breakpoints: Option<Vec<Breakpoint>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Breakpoint {
    pub id: Option<usize>,
    pub verified: bool,
    pub message: Option<String>,
    pub source: Option<Source>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub end_line: Option<usize>,
    pub end_column: Option<usize>,
    pub instruction_reference: Option<String>,
    pub offset: Option<usize>,
    pub reason: Option<String>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Source {
    pub name: Option<String>,
    pub path: Option<String>,
    pub source_reference: Option<usize>,
    pub presentation_hint: Option<String>,
    pub origin: Option<String>,
    pub sources: Option<Vec<Source>>,
    pub adapter_data: Option<Value>,
    pub checksums: Option<Vec<Value>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadsResponse {
    pub threads: Vec<Thread>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Thread {
    pub id: usize,
    pub name: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct StoppedEvent {
    pub reason: StoppedEventReason,
    pub description: Option<String>,
    pub thread_id: Option<usize>,
    pub preserve_focus_hint: Option<bool>,
    pub text: Option<String>,
    pub all_threads_stopped: Option<bool>,
    pub hit_breakpoint_ids: Option<Vec<usize>>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub enum StoppedEventReason {
    Step,
    Breakpoint,
    Exception,
    Pause,
    Entry,
    Goto,
    #[serde(rename = "function breakpoint")]
    FunctionBreakpoint,
    #[serde(rename = "data breakpoint")]
    DataBreakpoint,
    #[serde(rename = "instruction breakpoint")]
    InstructionBreakpoint,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ConfigurationDoneResponse {}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceResponse {
    pub content: String,
    pub mime_type: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StackTraceArguments {
    pub thread_id: usize,
    pub start_frame: Option<usize>,
    pub levels: Option<usize>,
    pub format: Option<Value>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct StackTraceResponse {
    pub stack_frames: Vec<StackFrame>,
    pub total_frames: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct StackFrame {
    pub id: usize,
    pub name: String,
    pub source: Option<Source>,
    pub line: usize,
    pub column: usize,
    pub end_line: Option<usize>,
    pub end_column: Option<usize>,
    pub can_restart: Option<bool>,
    pub module_id: Option<usize>,
    pub presentation_hint: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ScopesArguments {
    pub frame_id: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ScopesResponse {
    pub scopes: Vec<Scope>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Scope {
    pub name: String,
    pub presentation_hint: Option<String>,
    pub variables_reference: usize,
    pub named_variables: Option<usize>,
    pub indexed_variables: Option<usize>,
    pub expensive: bool,
    pub source: Option<Source>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub end_line: Option<usize>,
    pub end_column: Option<usize>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NextArguments {
    pub thread_id: usize,
    pub single_thread: Option<bool>,
    pub granularity: Option<Value>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct NextResponse {}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadMemoryArguments {
    pub memory_reference: String,
    pub offset: Option<usize>,
    pub count: usize,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadMemoryResponse {
    pub address: String,
    pub unreadable_bytes: Option<usize>,
    pub data: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct OutputEvent {
    pub category: Option<OutputEventKind>,
    pub output: String,
    pub group: Option<String>,
    pub variable_reference: Option<usize>,
    pub source: Option<Source>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub data: Option<Value>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub enum OutputEventKind {
    Console,
    Important,
    Stdout,
    Stderr,
    Telemetry,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DisassembleArguments {
    pub memory_reference: String,
    pub offset: Option<usize>,
    pub instruction_count: usize,
    pub instruction_offset: Option<usize>,
    pub resolve_symbols: Option<bool>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DisassembleResponse {
    pub instructions: Vec<DisassembledInstruction>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DisassembledInstruction {
    address: String,
    instruction_bytes: Option<String>,
    instruction: String,
    symbol: Option<String>,
    location: Option<Source>,
    line: Option<usize>,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
    presentation_hint: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablesArguments {
    pub variables_reference: usize,
    pub filter: Option<String>,
    pub start: Option<usize>,
    pub count: Option<usize>,
    pub format: Option<Value>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablesResponse {
    pub variables: Vec<Variable>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Variable {
    pub name: String,
    pub value: String,
    #[serde(rename = "type")]
    pub type_: Option<String>,
    pub presentation_hint: Option<Value>,
    pub evaluate_name: Option<String>,
    pub variables_reference: usize,
    pub named_variables: Option<usize>,
    pub indexed_variables: Option<usize>,
    pub memory_reference: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetFunctionBreakpointArguments {
    pub breakpoints: Vec<Value>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SetFunctionBreakpointsResponse {
    pub breakpoints: Vec<Value>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BreakpointLocationsArguments {
    pub source: Source,
    pub line: usize,
    pub column: Option<usize>,
    pub end_line: Option<usize>,
    pub end_column: Option<usize>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BreakpointLocationsResponse {
    pub breakpoints: Vec<BreakpointLocation>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BreakpointLocation {
    pub line: usize,
    pub column: Option<usize>,
    pub end_line: Option<usize>,
    pub end_column: Option<usize>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetBreakpointsArguments {
    pub source: Source,
    pub breakpoints: Option<Vec<SourceBreakpoint>>,
    pub lines: Option<Vec<usize>>,
    pub source_modified: Option<bool>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct SourceBreakpoint {
    pub line: usize,
    pub column: Option<usize>,
    pub condition: Option<String>,
    pub hit_condition: Option<String>,
    pub log_message: Option<String>,
    pub mode: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SetBreakpointsResponse {
    pub breakpoints: Vec<Breakpoint>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExitedEvent {
    pub exit_code: usize,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ContinueArguments {
    pub thread_id: usize,
    pub single_thread: Option<bool>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ContinueResponse {
    pub all_threads_continued: Option<bool>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TerminatedEvent {
    pub restart: Option<Value>,
}
