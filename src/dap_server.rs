use std::{
    path::Path,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use base64::prelude::*;
use dap::{
    base_message::Sendable,
    events::{Event, ExitedEventBody, OutputEventBody, StoppedEventBody, TerminatedEventBody},
    requests::Command,
    responses::{
        ContinueResponse, ReadMemoryResponse, ResponseBody, ScopesResponse, SetBreakpointsResponse,
        SetExceptionBreakpointsResponse, SourceResponse, StackTraceResponse, ThreadsResponse,
        VariablesResponse,
    },
    types::{
        Breakpoint, Capabilities, OutputEventCategory, OutputEventGroup, Scope, Source, StackFrame,
        StoppedEventReason, Thread, Variable,
    },
};

use crate::{
    compiler::{
        self,
        runtime::{ControlFlow, Runtime},
    },
    utils::{dap::server::DapServer, sender::SimpleSender, server_process::FutureResult},
};

#[derive(Clone)]
pub struct DapImpl;

#[derive(Clone)]
pub struct DapContext(Arc<Mutex<Runtime>>);

impl DapContext {
    pub fn new(runtime: Runtime) -> Self {
        DapContext(Arc::new(Mutex::new(runtime)))
    }
}

async fn dap_handler(
    ctx: DapContext,
    sender: SimpleSender<Sendable, Event>,
    req: Command,
) -> Result<ResponseBody> {
    const MAIN_THREAD_ID: i64 = 1;

    fn runtime_start(ctx: DapContext, sender: SimpleSender<Sendable, Event>) {
        tokio::spawn(async move {
            let (flow, pc, next) = {
                let mut runtime = ctx.0.lock().unwrap();

                let mut flow = ControlFlow::Continue;
                while matches!(flow, ControlFlow::Continue) {
                    flow = runtime.step(false).unwrap();
                }

                (flow, runtime.pc, runtime.show_next_instruction())
            };

            match flow {
                ControlFlow::HitBreakpoint => {
                    sender
                        .send(Event::Stopped(StoppedEventBody {
                            reason: StoppedEventReason::Breakpoint,
                            description: Some(format!("pc: {}, next: {:?}", pc, next)),
                            thread_id: Some(MAIN_THREAD_ID),
                            preserve_focus_hint: None,
                            text: None,
                            all_threads_stopped: None,
                            hit_breakpoint_ids: None,
                        }))
                        .await?;
                }
                ControlFlow::Finish => {
                    sender
                        .send(Event::Terminated(Some(TerminatedEventBody {
                            restart: None,
                        })))
                        .await?;
                    sender
                        .send(Event::Exited(ExitedEventBody { exit_code: 0 }))
                        .await?;
                }
                _ => (),
            };

            Ok::<(), anyhow::Error>(())
        });
    }

    match req {
        Command::Initialize(_) => {
            tokio::spawn(async move {
                sender.send(Event::Initialized).await?;

                Ok::<(), anyhow::Error>(())
            });

            Ok(ResponseBody::Initialize(Capabilities {
                supports_configuration_done_request: None,
                supports_function_breakpoints: None,
                supports_conditional_breakpoints: None,
                supports_hit_conditional_breakpoints: None,
                supports_evaluate_for_hovers: None,
                exception_breakpoint_filters: None,
                supports_step_back: None,
                supports_set_variable: None,
                supports_restart_frame: None,
                supports_goto_targets_request: None,
                supports_step_in_targets_request: None,
                supports_completions_request: None,
                completion_trigger_characters: None,
                supports_modules_request: None,
                additional_module_columns: None,
                supported_checksum_algorithms: None,
                supports_restart_request: None,
                supports_exception_options: None,
                supports_value_formatting_options: None,
                supports_exception_info_request: None,
                support_terminate_debuggee: None,
                support_suspend_debuggee: None,
                supports_delayed_stack_trace_loading: None,
                supports_loaded_sources_request: None,
                supports_log_points: None,
                supports_terminate_threads_request: None,
                supports_set_expression: None,
                supports_terminate_request: None,
                supports_data_breakpoints: None,
                supports_read_memory_request: None,
                supports_write_memory_request: None,
                supports_disassemble_request: None,
                supports_cancel_request: None,
                supports_breakpoint_locations_request: None,
                supports_clipboard_context: None,
                supports_stepping_granularity: None,
                supports_instruction_breakpoints: None,
                supports_exception_filter_options: None,
                supports_single_thread_execution_requests: None,
            }))
        }
        Command::ConfigurationDone => todo!(),
        Command::Launch(arg) => {
            #[derive(serde::Deserialize, Debug)]
            #[serde(rename_all = "camelCase")]
            struct LaunchAdditionalData {
                source_file: Option<String>,
                cwd: Option<String>,
            }

            let data =
                serde_json::from_value::<LaunchAdditionalData>(arg.additional_data.unwrap())?;

            let source_file = Path::new(&data.cwd.clone().unwrap())
                .join(Path::new(&data.source_file.unwrap()))
                .to_str()
                .unwrap()
                .to_string();
            let mut compiler = compiler::Compiler::new();
            compiler.set_cwd(data.cwd.unwrap());

            let main = "main".to_string();
            let source_code = std::fs::read_to_string(&source_file)?;

            compiler.parse_with_code(main.clone(), source_code.clone())?;
            compiler.typecheck(main.clone())?;

            let ir = compiler.ir_code_gen(main.clone())?;
            let code = compiler::Compiler::vm_code_gen(ir)?;
            let program = compiler::Compiler::byte_code_gen(code)?;

            ctx.0.lock().unwrap().init(
                1024,
                program,
                std::path::Path::new(&source_file)
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                source_code,
            );

            sender
                .send(Event::Stopped(StoppedEventBody {
                    reason: StoppedEventReason::Entry,
                    description: Some("Entry".to_string()),
                    thread_id: Some(MAIN_THREAD_ID),
                    preserve_focus_hint: None,
                    text: None,
                    all_threads_stopped: None,
                    hit_breakpoint_ids: None,
                }))
                .await?;

            Ok(ResponseBody::Launch)
        }
        Command::Attach(_) => todo!(),
        Command::BreakpointLocations(_) => todo!(),
        Command::Completions(_) => todo!(),
        Command::Continue(_) => {
            runtime_start(ctx, sender);

            Ok(ResponseBody::Continue(ContinueResponse {
                all_threads_continued: None,
            }))
        }
        Command::DataBreakpointInfo(_) => todo!(),
        Command::Disassemble(_) => todo!(),
        Command::Disconnect(_) => Ok(ResponseBody::Disconnect),
        Command::Evaluate(_) => todo!(),
        Command::ExceptionInfo(_) => todo!(),
        Command::Goto(_) => todo!(),
        Command::GotoTargets(_) => todo!(),
        Command::LoadedSources => todo!(),
        Command::Modules(_) => todo!(),
        Command::Next(_) => {
            let control = {
                let mut runtime = ctx.0.lock().unwrap();
                let control = runtime.step(true)?;

                control
            };

            match control {
                ControlFlow::Finish => {
                    sender
                        .send(Event::Exited(ExitedEventBody { exit_code: 0 }))
                        .await?;

                    Ok(ResponseBody::Next)
                }
                ControlFlow::HitBreakpoint => {
                    sender
                        .send(Event::Stopped(StoppedEventBody {
                            reason: StoppedEventReason::Breakpoint,
                            description: Some("Breakpoint".to_string()),
                            thread_id: Some(MAIN_THREAD_ID),
                            preserve_focus_hint: None,
                            text: None,
                            all_threads_stopped: None,
                            hit_breakpoint_ids: None,
                        }))
                        .await?;

                    Ok(ResponseBody::Next)
                }
                ControlFlow::Continue => {
                    let (pc, next, stacks) = {
                        let runtime = ctx.0.lock().unwrap();

                        (
                            runtime.pc,
                            runtime.show_next_instruction(),
                            runtime.show_stacks(),
                        )
                    };

                    sender
                        .send(Event::Stopped(StoppedEventBody {
                            reason: StoppedEventReason::Step,
                            description: Some(format!("pc: {}, next: {:?}", pc, next)),
                            thread_id: Some(MAIN_THREAD_ID),
                            preserve_focus_hint: None,
                            text: None,
                            all_threads_stopped: None,
                            hit_breakpoint_ids: None,
                        }))
                        .await?;
                    sender
                        .send(Event::Output(OutputEventBody {
                            category: Some(OutputEventCategory::Console),
                            output: format!("pc: {}, next: {:?}\n", pc, next),
                            group: Some(OutputEventGroup::Start),
                            variables_reference: None,
                            source: None,
                            line: None,
                            column: None,
                            data: None,
                        }))
                        .await?;
                    sender
                        .send(Event::Output(OutputEventBody {
                            category: Some(OutputEventCategory::Console),
                            output: stacks,
                            group: None,
                            variables_reference: None,
                            source: None,
                            line: None,
                            column: None,
                            data: None,
                        }))
                        .await?;
                    sender
                        .send(Event::Output(OutputEventBody {
                            category: Some(OutputEventCategory::Console),
                            output: "".to_string(),
                            group: Some(OutputEventGroup::End),
                            variables_reference: None,
                            source: None,
                            line: None,
                            column: None,
                            data: None,
                        }))
                        .await?;

                    Ok(ResponseBody::Next)
                }
            }
        }
        Command::Pause(_) => todo!(),
        Command::ReadMemory(arg) => {
            let runtime = ctx.0.lock().unwrap();

            Ok(ResponseBody::ReadMemory(ReadMemoryResponse {
                address: "0x0".to_string(),
                data: Some(
                    BASE64_STANDARD
                        .encode(&runtime.memory[0..(arg.count as usize).min(runtime.memory.len())]),
                ),
                unreadable_bytes: None,
            }))
        }
        Command::Restart(_) => todo!(),
        Command::RestartFrame(_) => todo!(),
        Command::ReverseContinue(_) => todo!(),
        Command::Scopes(arg) => {
            let runtime = ctx.0.lock().unwrap();
            let values = runtime.get_stack_values(arg.frame_id as usize);

            Ok(ResponseBody::Scopes(ScopesResponse {
                scopes: vec![
                    Scope {
                        name: "Runtime Values".to_string(),
                        presentation_hint: None,
                        variables_reference: 1,
                        named_variables: Some(3),
                        indexed_variables: None,
                        expensive: false,
                        source: None,
                        line: None,
                        column: None,
                        end_line: None,
                        end_column: None,
                    },
                    Scope {
                        name: "Locals".to_string(),
                        presentation_hint: None,
                        variables_reference: arg.frame_id + 2,
                        named_variables: Some(values.len() as i64),
                        indexed_variables: None,
                        expensive: false,
                        source: None,
                        line: None,
                        column: None,
                        end_line: None,
                        end_column: None,
                    },
                ],
            }))
        }
        Command::SetBreakpoints(arg) => {
            let std_content = compiler::Compiler::create_input(String::new());
            let std_content_len = std_content.len();

            let mut runtime = ctx.0.lock().unwrap();

            let mut breakpoints = vec![];
            if let Some(bps) = &arg.breakpoints {
                for bp in bps {
                    let position = compiler::Compiler::find_line_and_column_with_input(
                        &runtime.source_code,
                        bp.line as usize,
                        bp.column.unwrap_or(0) as usize,
                    );

                    breakpoints.push(position + std_content_len);
                }
            }

            runtime.set_breakpoints(breakpoints.clone());

            Ok(ResponseBody::SetBreakpoints(SetBreakpointsResponse {
                breakpoints: breakpoints
                    .into_iter()
                    .enumerate()
                    .map(|(i, pos)| Breakpoint {
                        id: Some(i as i64),
                        verified: true,
                        message: Some(format!("{}", pos)),
                        source: None,
                        line: None,
                        column: None,
                        end_line: None,
                        end_column: None,
                        instruction_reference: None,
                        offset: None,
                    })
                    .collect(),
            }))
        }
        Command::SetDataBreakpoints(_) => todo!(),
        Command::SetExceptionBreakpoints(_) => Ok(ResponseBody::SetExceptionBreakpoints(
            SetExceptionBreakpointsResponse { breakpoints: None },
        )),
        Command::SetExpression(_) => todo!(),
        Command::SetFunctionBreakpoints(_) => todo!(),
        Command::SetInstructionBreakpoints(_) => todo!(),
        Command::SetVariable(_) => todo!(),
        Command::Source(_) => {
            let runtime = ctx.0.lock().unwrap();

            Ok(ResponseBody::Source(SourceResponse {
                content: runtime.source_code.clone(),
                mime_type: None,
            }))
        }
        Command::StackTrace(_) => {
            let runtime = ctx.0.lock().unwrap();
            let frames = runtime.get_stack_frames();

            Ok(ResponseBody::StackTrace(StackTraceResponse {
                total_frames: Some(frames.len() as i64),
                stack_frames: frames
                    .into_iter()
                    .enumerate()
                    .map(|(i, frame)| StackFrame {
                        id: i as i64,
                        name: format!("<stackframe:#{:x?}>", frame),
                        source: Some(Source {
                            name: Some(runtime.source_file.clone()),
                            path: None,
                            source_reference: None,
                            presentation_hint: None,
                            origin: None,
                            sources: None,
                            adapter_data: None,
                            checksums: None,
                        }),
                        line: 4,
                        column: 0,
                        end_line: None,
                        end_column: None,
                        can_restart: Some(true),
                        module_id: None,
                        presentation_hint: None,
                        instruction_pointer_reference: None,
                    })
                    .collect(),
            }))
        }
        Command::StepBack(_) => todo!(),
        Command::StepIn(_) => todo!(),
        Command::StepInTargets(_) => todo!(),
        Command::StepOut(_) => todo!(),
        Command::Terminate(_) => todo!(),
        Command::TerminateThreads(_) => todo!(),
        Command::Threads => Ok(ResponseBody::Threads(ThreadsResponse {
            threads: vec![Thread {
                id: MAIN_THREAD_ID,
                name: "main".to_string(),
            }],
        })),
        Command::Variables(arg) => {
            let runtime = ctx.0.lock().unwrap();

            match arg.variables_reference {
                1 => Ok(ResponseBody::Variables(VariablesResponse {
                    variables: vec![
                        Variable {
                            name: "pc".to_string(),
                            value: runtime.pc.to_string(),
                            type_field: Some("int".to_string()),
                            presentation_hint: None,
                            evaluate_name: None,
                            variables_reference: 0,
                            named_variables: None,
                            indexed_variables: None,
                            memory_reference: None,
                        },
                        Variable {
                            name: "sp".to_string(),
                            value: runtime.sp.to_string(),
                            type_field: Some("int".to_string()),
                            presentation_hint: None,
                            evaluate_name: None,
                            variables_reference: 0,
                            named_variables: None,
                            indexed_variables: None,
                            memory_reference: None,
                        },
                        Variable {
                            name: "bp".to_string(),
                            value: runtime.bp.to_string(),
                            type_field: Some("int".to_string()),
                            presentation_hint: None,
                            evaluate_name: None,
                            variables_reference: 0,
                            named_variables: None,
                            indexed_variables: None,
                            memory_reference: None,
                        },
                    ],
                })),
                _ => {
                    let values = runtime.get_stack_values(arg.variables_reference as usize - 2);

                    Ok(ResponseBody::Variables(VariablesResponse {
                        variables: values
                            .into_iter()
                            .enumerate()
                            .map(|(i, value)| Variable {
                                name: format!("#{}", i),
                                value: format!("{:?}", value),
                                type_field: Some("int".to_string()),
                                presentation_hint: None,
                                evaluate_name: None,
                                variables_reference: 0,
                                named_variables: None,
                                indexed_variables: None,
                                memory_reference: None,
                            })
                            .collect(),
                    }))
                }
            }
        }
        Command::WriteMemory(_) => todo!(),
        Command::Cancel(_) => todo!(),
    }
}

impl DapServer<DapContext> for DapImpl {
    fn handle_request(
        ctx: DapContext,
        sender: SimpleSender<Sendable, Event>,
        req: Command,
    ) -> FutureResult<ResponseBody> {
        Box::pin(dap_handler(ctx, sender, req))
    }
}
