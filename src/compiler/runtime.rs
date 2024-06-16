use std::{
    cell::RefCell,
    collections::HashMap,
    io::{stdout, BufWriter, Write},
    sync::{Arc, Mutex},
};

#[cfg(feature = "gui")]
use fltk::{app, button::Button, draw, frame::Frame, group::Flex, prelude::*, window::Window};

use nanoid::nanoid;
use thiserror::Error;

use crate::compiler::vm_code_gen::VmCodeGenerator;

use super::{ast::Span, ir::Value, vm::Instruction};

#[derive(Debug, Clone, Error)]
pub enum RuntimeError {
    #[error("Unknown instruction: {0}")]
    UnknownInstruction(u8),
    #[error("Unknown register: {0}")]
    UnknownRegister(u8),
}

#[derive(Debug, Clone)]
pub enum ControlFlow {
    Continue,
    Finish,
    HitBreakpoint,
}

#[cfg(feature = "gui")]
thread_local! {
    static APP: RefCell<Option<app::App >> = RefCell::new(None);
    static WIDGETS: RefCell<Vec<Box<dyn std::any::Any>>> = RefCell::new(vec![]);

    static GUI_DATA: RefCell<Vec<Box<dyn std::any::Any>>> = RefCell::new(vec![]);
}

#[cfg(feature = "gui")]
fn register_gui_data(data: impl std::any::Any) -> usize {
    let mut id = 0;
    GUI_DATA.with(|gui_data_ref| {
        let mut gui_data = gui_data_ref.borrow_mut();

        id = gui_data.len();

        gui_data.push(Box::new(data));
    });

    id
}

pub struct Runtime {
    pub(crate) memory: Vec<u8>,
    pub(crate) sp: usize,
    pub(crate) bp: usize,
    pub(crate) pc: usize,
    pub(crate) program: Vec<u8>,
    pub(crate) source_file: String,
    pub(crate) source_code: String,
    pub(crate) breakpoints: Vec<usize>,
    pub(crate) trap_stdout: Option<Arc<Mutex<BufWriter<Vec<u8>>>>>,
    prev_source_map: (usize, usize),
    protected_section: usize,
    closure_tasks: HashMap<String, (u64, u64)>,
    channel: (
        std::sync::mpsc::Sender<(String, Vec<Value>)>,
        std::sync::mpsc::Receiver<(String, Vec<Value>)>,
    ),
    interrupted: Vec<(String, usize, usize)>,
}

impl Runtime {
    pub fn new(size: usize, program: Vec<u8>) -> Self {
        let memory = vec![0; size];
        let bp = memory.len();
        let sp = bp;

        Self {
            memory,
            sp,
            bp,
            pc: 0,
            program,
            source_file: String::new(),
            source_code: String::new(),
            breakpoints: vec![],
            prev_source_map: (0, 0),
            trap_stdout: None,
            protected_section: 0,
            closure_tasks: HashMap::new(),
            channel: std::sync::mpsc::channel(),
            interrupted: vec![],
        }
    }

    pub fn init(
        &mut self,
        size: usize,
        program: Vec<u8>,
        source_file: String,
        source_code: String,
    ) {
        self.memory = vec![0; size];
        self.bp = self.memory.len();
        self.sp = self.bp;
        self.pc = 0;
        self.program = program;
        self.source_file = source_file;
        self.source_code = source_code;
        self.breakpoints = vec![];
        self.prev_source_map = (0, 0);
    }

    pub fn set_breakpoints(&mut self, breakpoints: Vec<usize>) {
        self.breakpoints = breakpoints;
    }

    pub fn get_stack_frames(&self) -> Vec<usize> {
        let mut frames = vec![];
        let mut bp = self.bp;
        frames.push(bp);
        while bp > 0 && bp < self.memory.len() {
            bp = self.load_i64(bp as u64) as usize;
            frames.push(bp);
        }

        frames
    }

    pub fn get_stack_values(&self, frame_id: usize) -> Vec<Value> {
        let frames = self.get_stack_frames();
        let frame_top = frames[frame_id];

        let mut values = vec![];

        for i in (frame_top
            ..frames
                .get(frame_id + 1)
                .cloned()
                .unwrap_or(self.memory.len()))
            .step_by(8)
        {
            let val = self.load_i64(i as u64);

            values.push(Value::from_u64(val as u64));
        }

        values
    }

    pub fn memory_view_64(&self) -> Vec<u64> {
        let mut view = vec![];
        for i in (self.sp..self.memory.len()).step_by(8) {
            view.push(u64::from_le_bytes([
                self.memory[i],
                self.memory[i + 1],
                self.memory[i + 2],
                self.memory[i + 3],
                self.memory[i + 4],
                self.memory[i + 5],
                self.memory[i + 6],
                self.memory[i + 7],
            ]));
        }

        view.reverse();

        view
    }

    fn load_i64(&self, address: u64) -> i64 {
        i64::from_le_bytes([
            self.memory[address as usize],
            self.memory[address as usize + 1],
            self.memory[address as usize + 2],
            self.memory[address as usize + 3],
            self.memory[address as usize + 4],
            self.memory[address as usize + 5],
            self.memory[address as usize + 6],
            self.memory[address as usize + 7],
        ])
    }

    fn load_f32(&self, address: u32) -> f32 {
        f32::from_le_bytes([
            self.memory[address as usize + 0],
            self.memory[address as usize + 1],
            self.memory[address as usize + 2],
            self.memory[address as usize + 3],
        ])
    }

    fn store_i64(&mut self, address: u64, value: i64) {
        assert!(
            self.protected_section <= address as usize,
            "Section protected"
        );

        self.memory[address as usize..(address as usize + 8)].copy_from_slice(&value.to_le_bytes());
    }

    fn store_u32(&mut self, address: u32, value: u32) {
        assert!(
            self.protected_section <= address as usize,
            "Section protected"
        );

        self.memory[address as usize..(address as usize + 4)].copy_from_slice(&value.to_le_bytes());
    }

    fn store_u8(&mut self, address: u32, value: u8) {
        assert!(
            self.protected_section <= address as usize,
            "Section protected"
        );

        self.memory[address as usize] = value;
    }

    pub fn pop_i64(&mut self) -> i64 {
        let val = self.load_i64(self.sp as u64);
        self.sp += 8;
        val
    }

    pub fn pop_f32(&mut self) -> f32 {
        let val = self.load_i64(self.sp as u64);
        self.sp += 8;
        f32::from_bits(val as i32 as u32)
    }

    fn push(&mut self, val: i64) {
        self.sp -= 8;
        self.store_i64(self.sp as u64, val);
    }

    fn push_f32(&mut self, val: f32) {
        self.sp -= 8;
        self.memory[self.sp..(self.sp + 4)].copy_from_slice(&val.to_le_bytes());
    }

    fn pop_address(&mut self) -> u64 {
        self.pop_i64() as u64
    }

    pub fn pop_value(&mut self) -> Value {
        let value = self.pop_i64();

        Value::from_u64(value as u64)
    }

    pub fn show_next_instruction(&self) -> Instruction {
        if self.pc >= self.program.len() {
            return Instruction::Nop;
        }

        let inst = Instruction::from_byte(&self.program[self.pc..]).unwrap_or(Instruction::Nop);

        match inst {
            Instruction::Push(_) => Instruction::Push(
                Value::from_u64(u64::from_le_bytes([
                    self.program[self.pc + 1],
                    self.program[self.pc + 2],
                    self.program[self.pc + 3],
                    self.program[self.pc + 4],
                    self.program[self.pc + 5],
                    self.program[self.pc + 6],
                    self.program[self.pc + 7],
                    self.program[self.pc + 8],
                ]))
                .as_u64(),
            ),
            Instruction::ExtCall(_) => Instruction::ExtCall(u64::from_le_bytes([
                self.program[self.pc + 1],
                self.program[self.pc + 2],
                self.program[self.pc + 3],
                self.program[self.pc + 4],
                self.program[self.pc + 5],
                self.program[self.pc + 6],
                self.program[self.pc + 7],
                self.program[self.pc + 8],
            ]) as usize),
            Instruction::SourceMap(_) => Instruction::SourceMap(Span::span(
                "_".to_string(),
                u64::from_le_bytes([
                    self.program[self.pc + 1],
                    self.program[self.pc + 2],
                    self.program[self.pc + 3],
                    self.program[self.pc + 4],
                    self.program[self.pc + 5],
                    self.program[self.pc + 6],
                    self.program[self.pc + 7],
                    self.program[self.pc + 8],
                ]) as usize,
                u64::from_le_bytes([
                    self.program[self.pc + 9],
                    self.program[self.pc + 10],
                    self.program[self.pc + 11],
                    self.program[self.pc + 12],
                    self.program[self.pc + 13],
                    self.program[self.pc + 14],
                    self.program[self.pc + 15],
                    self.program[self.pc + 16],
                ]) as usize,
            )),
            Instruction::Data { .. } => {
                let offset = u64::from_le_bytes([
                    self.program[self.pc + 1],
                    self.program[self.pc + 2],
                    self.program[self.pc + 3],
                    self.program[self.pc + 4],
                    self.program[self.pc + 5],
                    self.program[self.pc + 6],
                    self.program[self.pc + 7],
                    self.program[self.pc + 8],
                ]) as usize;
                let length = u64::from_le_bytes([
                    self.program[self.pc + 9],
                    self.program[self.pc + 10],
                    self.program[self.pc + 11],
                    self.program[self.pc + 12],
                    self.program[self.pc + 13],
                    self.program[self.pc + 14],
                    self.program[self.pc + 15],
                    self.program[self.pc + 16],
                ]) as usize;

                Instruction::Data {
                    offset: offset as u64,
                    length: length as u64,
                    data: self.program[self.pc + 17..self.pc + 17 + length].to_vec(),
                }
            }
            _ => inst,
        }
    }

    pub fn show_stacks(&self) -> String {
        let mut result = String::new();
        let mut p = self.memory.len() - 8;
        result.push_str("|");

        while p >= self.sp {
            let val = Value::from_u64(self.load_i64(p as u64) as u64);

            if p == self.sp {
                result.push_str(&format!(" {:?} S", val));
            } else if p == self.bp {
                result.push_str(&format!(" {:?} B", val));
            } else {
                result.push_str(&format!(" {:?} |", val));
            }
            p -= 8;
        }

        result
    }

    fn print_stack(&self) {
        let mut p = self.memory.len() - 8;
        print!("[{}:{:x}] |", self.pc, self.pc);
        while p >= self.sp {
            let val = Value::from_u64(self.load_i64(p as u64) as u64);

            if p == self.sp {
                print!(" {:?} S", val);
            } else if p == self.bp {
                print!(" {:?} B", val);
            } else {
                print!(" {:?} |", val);
            }
            p -= 8;
        }

        println!(" >> next: {:?}", self.show_next_instruction());
    }

    fn consume(&mut self) -> u8 {
        let code = self.program[self.pc];
        self.pc += 1;

        code
    }

    fn consume_u64(&mut self) -> u64 {
        u64::from_le_bytes([
            self.consume(),
            self.consume(),
            self.consume(),
            self.consume(),
            self.consume(),
            self.consume(),
            self.consume(),
            self.consume(),
        ])
    }

    pub fn step(
        &mut self,
        print_stacks: bool,
        print_memory_store: bool,
    ) -> Result<ControlFlow, RuntimeError> {
        if print_stacks {
            self.print_stack();
        }

        for bp in &self.breakpoints {
            if &self.pc == bp {
                println!("breakpoint at {}", bp);
            }
        }

        if let Ok((task_id, args)) = self.channel.1.try_recv() {
            let (callback_ptr, callback_env) = self.closure_tasks.get(&task_id).unwrap().clone();
            let args_len = args.len() + 1;

            // allocate for the return value
            self.push(0);

            // push closure env (as the last argument)
            self.push(callback_env as i64);
            // push args
            for arg in args.into_iter().rev() {
                self.push(arg.as_u64() as i64);
            }

            // call
            let pc = callback_ptr & 0xffffffff; // remove type tag
            self.push(self.pc as i64);
            self.pc = pc as usize;

            self.interrupted.push((task_id.clone(), self.sp, args_len));

            if print_stacks {
                println!("Task {} started: pc={}, args={}", task_id, pc, args_len);
            }

            return Ok(ControlFlow::Continue);
        }

        let inst = self.consume();
        match inst {
            // push
            0x01 => {
                let imm = self.consume_u64() as i64;
                self.push(imm);
            }
            // call
            0x02 => {
                let pc = self.pop_i64();
                self.push(self.pc as i64);
                self.pc = pc as usize;
            }
            // extcall
            0x03 => {
                let label = self.consume_u64();
                match label {
                    1 => {
                        let fd = self.pop_i64();
                        let address = self.pop_address();
                        let size = self.pop_i64();

                        match fd {
                            1 => {
                                let data = &self.memory
                                    [address as usize..(address as usize + size as usize)];
                                let result = match self.trap_stdout.clone() {
                                    Some(stdout) => stdout.lock().unwrap().write(data),
                                    None => stdout().write(data),
                                };

                                self.push(match result {
                                    Ok(_) => 0,
                                    Err(_) => 1,
                                });
                            }
                            _ => todo!(),
                        };
                    }
                    #[cfg(feature = "gui")]
                    index => {
                        let table = VmCodeGenerator::extcall_table();

                        if index as usize == table["extcall_sdl_init"] {
                            let context = sdl2::init().unwrap();
                            let id = register_gui_data(context);
                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_sdl_context_video"] {
                            let context_id = self.pop_i64() as i32;

                            let video = GUI_DATA.with(|data_ref| {
                                let data = data_ref.borrow();
                                let context = data[context_id as usize]
                                    .downcast_ref::<sdl2::Sdl>()
                                    .unwrap();
                                let video = context.video().unwrap();

                                video
                            });
                            let id = register_gui_data(video);

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_sdl_context_event_pump"] {
                            let context_id = self.pop_i64() as i32;

                            let event_pump = GUI_DATA.with(|data_ref| {
                                let data = data_ref.borrow();
                                let context = data[context_id as usize]
                                    .downcast_ref::<sdl2::Sdl>()
                                    .unwrap();
                                let event_pump = context.event_pump().unwrap();

                                event_pump
                            });
                            let id = register_gui_data(event_pump);

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_event_pump_poll"] {
                            let pump_id = self.pop_i64() as i32;

                            let event = GUI_DATA.with(|data_ref| {
                                let mut data = data_ref.borrow_mut();
                                let event_pump = data[pump_id as usize]
                                    .downcast_mut::<sdl2::EventPump>()
                                    .unwrap();

                                event_pump.poll_event()
                            });
                            let id = register_gui_data(event);

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_event_is_quit"] {
                            let event_id = self.pop_i64() as i32;

                            let is_quit = GUI_DATA.with(|data_ref| {
                                let data = data_ref.borrow();
                                let event = data[event_id as usize]
                                    .downcast_ref::<Option<sdl2::event::Event>>()
                                    .unwrap();

                                matches!(event, Some(sdl2::event::Event::Quit { .. }))
                            });

                            self.push(Value::Bool(is_quit).as_u64() as i64);
                        } else if index as usize == table["extcall_video_window"] {
                            let video_id = self.pop_i64() as i32;
                            let title_ptr = self.pop_i64() as u64;
                            let title_len = self.pop_i64() as usize;
                            let title = String::from_utf8(
                                self.memory[title_ptr as usize..(title_ptr as usize + title_len)]
                                    .to_vec(),
                            )
                            .unwrap();
                            let width = self.pop_i64() as i32;
                            let height = self.pop_i64() as i32;

                            let window = GUI_DATA.with(|data_ref| {
                                let data = data_ref.borrow();
                                let video = data[video_id as usize]
                                    .downcast_ref::<sdl2::VideoSubsystem>()
                                    .unwrap();
                                let window = video
                                    .window(title.as_str(), width as u32, height as u32)
                                    .build()
                                    .unwrap();

                                window
                            });
                            let id = register_gui_data(window);

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_window_get_canvas"] {
                            let window_id = self.pop_i64() as i32;

                            let canvas = GUI_DATA.with(|data_ref| {
                                let data = data_ref.borrow();
                                let window = data[window_id as usize]
                                    .downcast_ref::<sdl2::video::Window>()
                                    .unwrap();
                                let canvas = window.clone().into_canvas().build().unwrap();

                                canvas
                            });
                            let id = register_gui_data(canvas);

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_canvas_set_draw_color"] {
                            let canvas_id = self.pop_i64() as i32;
                            let r = self.pop_i64() as u8;
                            let g = self.pop_i64() as u8;
                            let b = self.pop_i64() as u8;

                            GUI_DATA.with(|data_ref| {
                                let mut data = data_ref.borrow_mut();
                                let canvas = data[canvas_id as usize]
                                    .downcast_mut::<sdl2::render::Canvas<sdl2::video::Window>>()
                                    .unwrap();

                                canvas.set_draw_color(sdl2::pixels::Color::RGB(r, g, b));
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_canvas_clear"] {
                            let canvas_id = self.pop_i64() as i32;

                            GUI_DATA.with(|data_ref| {
                                let mut data = data_ref.borrow_mut();
                                let canvas = data[canvas_id as usize]
                                    .downcast_mut::<sdl2::render::Canvas<sdl2::video::Window>>()
                                    .unwrap();

                                canvas.clear();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_canvas_present"] {
                            let canvas_id = self.pop_i64() as i32;

                            GUI_DATA.with(|data_ref| {
                                let mut data = data_ref.borrow_mut();
                                let canvas = data[canvas_id as usize]
                                    .downcast_mut::<sdl2::render::Canvas<sdl2::video::Window>>()
                                    .unwrap();

                                canvas.present();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_canvas_fill_rect"] {
                            let canvas_id = self.pop_i64() as i32;
                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let width = self.pop_i64() as i32;
                            let height = self.pop_i64() as i32;

                            GUI_DATA.with(|data_ref| {
                                let mut data = data_ref.borrow_mut();
                                let canvas = data[canvas_id as usize]
                                    .downcast_mut::<sdl2::render::Canvas<sdl2::video::Window>>()
                                    .unwrap();

                                canvas
                                    .fill_rect(sdl2::rect::Rect::new(
                                        x,
                                        y,
                                        width as u32,
                                        height as u32,
                                    ))
                                    .unwrap();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_sleep"] {
                            let sec = self.pop_f32();

                            std::thread::sleep(std::time::Duration::from_secs_f32(sec));

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_app_default"] {
                            let app = app::App::default();

                            APP.with(|app_ref| {
                                *app_ref.borrow_mut() = Some(app);
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_app_run"] {
                            let _ = self.pop_i64();

                            APP.with(|app_ref| {
                                let app = app_ref.borrow().unwrap();
                                app.run().unwrap();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_app_wait"] {
                            let _ = self.pop_i64();

                            let mut result = false;
                            APP.with(|app_ref| {
                                let app = app_ref.borrow().unwrap();

                                result = app.wait();
                            });

                            self.push(Value::Bool(result).as_u64() as i64);
                        } else if index as usize == table["extcall_app_redraw"] {
                            let _ = self.pop_i64();

                            APP.with(|app_ref| {
                                let app = app_ref.borrow().unwrap();

                                app.redraw();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_app_event_key"] {
                            let _ = self.pop_i64();
                            let result = app::event_key().bits();

                            self.push(Value::Int(result as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_app_event_coords"] {
                            let _ = self.pop_i64();
                            let (x, y) = app::event_coords();
                            let value = x + (y << 16);

                            self.push(Value::Int(value).as_u64() as i64);
                        } else if index as usize == table["extcall_app_quit"] {
                            let _ = self.pop_i64();

                            APP.with(|app_ref| {
                                let app = app_ref.borrow().unwrap();

                                app.quit();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_app_sleep"] {
                            let sec = self.pop_f32();

                            app::sleep(sec as f64);

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_app_add_idle"] {
                            let _ = self.pop_i64();
                            let callback_ptr = self.pop_i64() as u64;
                            let callback_env = self.pop_i64() as u64;

                            let task_id = nanoid!();
                            self.closure_tasks
                                .insert(task_id.clone(), (callback_ptr, callback_env));

                            let sender = self.channel.0.clone();
                            app::add_idle3(move |_| {
                                sender.send((task_id.clone(), vec![])).unwrap();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_new"] {
                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let width = self.pop_i64() as i32;
                            let height = self.pop_i64() as i32;
                            let title_ptr = self.pop_i64() as u64;
                            let title_len = self.pop_i64() as usize;
                            let title = String::from_utf8(
                                self.memory[title_ptr as usize..(title_ptr as usize + title_len)]
                                    .to_vec(),
                            )
                            .unwrap();

                            let mut id = 0;
                            let window = Window::new(x, y, width, height, title.as_str());

                            WIDGETS.with(|widgets_ref| {
                                id = widgets_ref.borrow().len();

                                let mut widgets = widgets_ref.borrow_mut();
                                widgets.push(Box::new(window));
                            });

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_window_end"] {
                            let window_id = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_ref::<Window>()
                                    .unwrap();

                                window.end();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_show"] {
                            let window_id = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_mut::<Window>()
                                    .unwrap();

                                window.show();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_set_handler"] {
                            let window_id = self.pop_i64() as i32;
                            let callback_ptr = self.pop_i64() as u64;
                            let callback_env = self.pop_i64() as u64;

                            let task_id = nanoid!();
                            self.closure_tasks
                                .insert(task_id.clone(), (callback_ptr, callback_env));

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_mut::<Window>()
                                    .unwrap();

                                let sender = self.channel.0.clone();
                                window.handle(move |_, event| {
                                    sender
                                        .send((task_id.clone(), vec![Value::Int(event.bits())]))
                                        .unwrap();

                                    true
                                });
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_draw"] {
                            let window_id = self.pop_i64() as i32;
                            let callback_ptr = self.pop_i64() as u64;
                            let callback_env = self.pop_i64() as u64;

                            let task_id = nanoid!();
                            self.closure_tasks
                                .insert(task_id.clone(), (callback_ptr, callback_env));

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_mut::<Window>()
                                    .unwrap();

                                let sender = self.channel.0.clone();
                                window.draw(move |_| {
                                    sender.send((task_id.clone(), vec![])).unwrap();
                                });
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_redraw"] {
                            let window_id = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_mut::<Window>()
                                    .unwrap();

                                window.redraw();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_set_color"] {
                            let window_id = self.pop_i64() as i32;
                            let r = self.pop_i64() as u8;
                            let g = self.pop_i64() as u8;
                            let b = self.pop_i64() as u8;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_mut::<Window>()
                                    .unwrap();

                                window.set_color(fltk::enums::Color::from_rgb(r, g, b));
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_window_make_current"] {
                            let window_id = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let window = widgets[window_id as usize]
                                    .downcast_mut::<Window>()
                                    .unwrap();

                                window.make_current();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_button_default"] {
                            let title_ptr = self.pop_i64() as u64;
                            let title_len = self.pop_i64() as usize;
                            let title = String::from_utf8(
                                self.memory[title_ptr as usize..(title_ptr as usize + title_len)]
                                    .to_vec(),
                            )
                            .unwrap();

                            let button = Button::default().with_label(title.as_str());

                            let mut id = 0;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                id = widgets.len();
                                widgets.push(Box::new(button));
                            });

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_frame_new"] {
                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let width = self.pop_i64() as i32;
                            let height = self.pop_i64() as i32;
                            let frame = Frame::new(x, y, width, height, None);

                            let mut id = 0;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                id = widgets.len();
                                widgets.push(Box::new(frame));
                            });

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_frame_default"] {
                            let frame = Frame::default();

                            let mut id = 0;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                id = widgets.len();
                                widgets.push(Box::new(frame));
                            });

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_frame_set_rectangle"] {
                            let frame_id = self.pop_i64() as i32;

                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let width = self.pop_i64() as i32;
                            let height = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let frame =
                                    widgets[frame_id as usize].downcast_mut::<Frame>().unwrap();

                                frame.set_pos(x, y);
                                frame.set_size(width, height);
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_frame_set_label"] {
                            let frame_id = self.pop_i64() as i32;

                            let title_ptr = self.pop_i64() as u64;
                            let title_len = self.pop_i64() as usize;
                            let title = String::from_utf8(
                                self.memory[title_ptr as usize..(title_ptr as usize + title_len)]
                                    .to_vec(),
                            )
                            .unwrap();

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                let frame =
                                    widgets[frame_id as usize].downcast_mut::<Frame>().unwrap();
                                frame.set_label(title.as_str());
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_frame_resize"] {
                            let frame_id = self.pop_i64() as i32;
                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let width = self.pop_i64() as i32;
                            let height = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                let frame =
                                    widgets[frame_id as usize].downcast_mut::<Frame>().unwrap();
                                frame.resize(x, y, width, height);
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_frame_set_color"] {
                            let frame_id = self.pop_i64() as i32;
                            let r = self.pop_i64() as u8;
                            let g = self.pop_i64() as u8;
                            let b = self.pop_i64() as u8;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                let frame =
                                    widgets[frame_id as usize].downcast_mut::<Frame>().unwrap();
                                frame.set_color(fltk::enums::Color::from_rgb(r, g, b));
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_frame_set_frame"] {
                            let frame_id = self.pop_i64() as i32;
                            let frame_type = self.pop_i64() as usize;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                let frame =
                                    widgets[frame_id as usize].downcast_mut::<Frame>().unwrap();
                                frame.set_frame(fltk::enums::FrameType::by_index(frame_type));
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_frame_draw"] {
                            let flex_id = self.pop_i64() as i32;
                            let callback_ptr = self.pop_i64() as u64;
                            let callback_env = self.pop_i64() as u64;

                            let task_id = nanoid!();
                            self.closure_tasks
                                .insert(task_id.clone(), (callback_ptr, callback_env));

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let frame =
                                    widgets[flex_id as usize].downcast_mut::<Frame>().unwrap();

                                let sender = self.channel.0.clone();
                                frame.draw(move |_| {
                                    sender.send((task_id.clone(), vec![])).unwrap();
                                });
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_button_set_callback"] {
                            let button_id = self.pop_i64() as i32;

                            let callback_ptr = self.pop_i64() as u64;
                            let callback_env = self.pop_i64() as u64;
                            let task_id = nanoid!();
                            self.closure_tasks
                                .insert(task_id.clone(), (callback_ptr, callback_env));

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let button = widgets[button_id as usize]
                                    .downcast_mut::<Button>()
                                    .unwrap();

                                let sender = self.channel.0.clone();
                                button.set_callback(move |_| {
                                    sender.send((task_id.clone(), vec![])).unwrap();
                                });
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_flex_default_fill"] {
                            let flex = Flex::default_fill();

                            let mut id = 0;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();

                                id = widgets.len();
                                widgets.push(Box::new(flex));
                            });

                            self.push(Value::Int(id as i32).as_u64() as i64);
                        } else if index as usize == table["extcall_flex_column"] {
                            let flex_id = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let flex =
                                    widgets[flex_id as usize].downcast_ref::<Flex>().unwrap();

                                widgets[flex_id as usize] = Box::new(flex.clone().column());
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_flex_set_margins"] {
                            let flex_id = self.pop_i64() as i32;
                            let left = self.pop_i64() as i32;
                            let top = self.pop_i64() as i32;
                            let right = self.pop_i64() as i32;
                            let bottom = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let flex =
                                    widgets[flex_id as usize].downcast_mut::<Flex>().unwrap();

                                flex.set_margins(left, top, right, bottom);
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_flex_set_pad"] {
                            let flex_id = self.pop_i64() as i32;
                            let pad = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let flex =
                                    widgets[flex_id as usize].downcast_mut::<Flex>().unwrap();

                                flex.set_pad(pad);
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_flex_end"] {
                            let flex_id = self.pop_i64() as i32;

                            WIDGETS.with(|widgets_ref| {
                                let mut widgets = widgets_ref.borrow_mut();
                                let flex =
                                    widgets[flex_id as usize].downcast_mut::<Flex>().unwrap();

                                flex.end();
                            });

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_draw_set_draw_color"] {
                            let r = self.pop_i64() as u8;
                            let g = self.pop_i64() as u8;
                            let b = self.pop_i64() as u8;
                            draw::set_draw_color(fltk::enums::Color::from_rgb(r, g, b));

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_draw_draw_rect"] {
                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let w = self.pop_i64() as i32;
                            let h = self.pop_i64() as i32;
                            draw::draw_rectf(x, y, w, h);

                            self.push(Value::Nil.as_u64() as i64);
                        } else if index as usize == table["extcall_draw_draw_box"] {
                            let box_type = self.pop_i64() as i32;
                            let x = self.pop_i64() as i32;
                            let y = self.pop_i64() as i32;
                            let w = self.pop_i64() as i32;
                            let h = self.pop_i64() as i32;
                            let r = self.pop_i64() as u8;
                            let g = self.pop_i64() as u8;
                            let b = self.pop_i64() as u8;
                            draw::draw_box(
                                fltk::enums::FrameType::by_index(box_type as usize),
                                x,
                                y,
                                w,
                                h,
                                fltk::enums::Color::from_rgb(r, g, b),
                            );

                            self.push(Value::Nil.as_u64() as i64);
                        } else {
                            todo!()
                        }
                    }
                    #[cfg(not(feature = "gui"))]
                    _ => todo!(),
                }
            }
            // return
            0x04 => {
                let mut returned = false;

                if let Some((task_id, sp, args)) = self.interrupted.iter().last().cloned() {
                    if sp == self.sp {
                        if print_stacks {
                            println!(
                                "Task {} finished, {}, current_sp: {}, args: {}",
                                task_id, sp, self.sp, args
                            );
                        }

                        let value = self.pop_value();
                        assert!(
                            matches!(value, Value::Int(_)),
                            "Return value must be an integer, but got {:?}",
                            value
                        );
                        self.pc = value.as_u64() as usize;

                        // pops arguments
                        for _ in 0..args {
                            self.pop_i64();
                        }
                        // pops the return value
                        self.pop_i64();

                        self.interrupted.pop();

                        returned = true;
                    }
                }

                if !returned {
                    let value = self.pop_value();
                    assert!(
                        matches!(value, Value::Int(_)),
                        "Return value must be an integer, but got {:?}",
                        value
                    );
                    self.pc = value.as_u64() as usize;
                }
            }
            // jump
            0x05 => {
                self.pc = self.pop_i64() as usize;
            }
            // jump_if
            0x06 => {
                let address = self.pop_address();
                let val = self.pop_i64();
                if val != 0 {
                    self.pc = address as usize;
                }
            }
            // nop
            0x07 => {}
            // data
            0x08 => {
                let offset = self.consume_u64();
                let length = self.consume_u64();
                self.protected_section = offset as usize + length as usize;

                for i in 0..length {
                    let b = self.consume();
                    self.memory[offset as usize + i as usize] = b;
                }
            }
            // abort
            0x09 => {
                self.print_stack();
                for (index, bp) in self.get_stack_frames().into_iter().enumerate() {
                    println!("#{} [{}]", index, bp);
                    println!("  {:?}", self.get_stack_values(index));
                }
                panic!("Aborted");
            }

            // add
            0x10 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(b + a);
            }
            // sub
            0x11 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                // NOTE: prevent sign bit to be used
                self.push((b as i32 - a as i32) as u32 as u64 as i64);
            }
            // mul
            0x12 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push((b as i32 * a as i32) as u32 as u64 as i64);
            }
            // div
            0x13 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push((b as i32 / a as i32) as u32 as u64 as i64);
            }
            // addFloat
            0x14 => {
                let a = self.pop_f32();
                let b = self.pop_f32();
                self.push_f32(b + a);
            }
            // subFloat
            0x15 => {
                let a = self.pop_f32();
                let b = self.pop_f32();
                self.push_f32(b - a);
            }
            // mulFloat
            0x16 => {
                let a = self.pop_f32();
                let b = self.pop_f32();
                self.push_f32(b * a);
            }
            // divFloat
            0x17 => {
                let a = self.pop_f32();
                let b = self.pop_f32();
                self.push_f32(b / a);
            }
            // modInt
            0x18 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(b % a);
            }

            // xor
            0x20 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(b ^ a);
            }
            // and
            0x21 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(b & a);
            }
            // or
            0x22 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(b | a);
            }
            // not
            0x23 => {
                let a = self.pop_i64();
                self.push(if a == 0 { 1 } else { 0 });
            }

            // eq
            0x30 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(if b == a { 1 } else { 0 });
            }
            // not_eq
            0x31 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(if b != a { 1 } else { 0 });
            }
            // lt
            0x32 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(if (b as i32) < (a as i32) { 1 } else { 0 });
            }
            // gt
            0x33 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(if (b as i32) > (a as i32) { 1 } else { 0 });
            }
            // le
            0x34 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(if (b as i32) <= (a as i32) { 1 } else { 0 });
            }
            // ge
            0x35 => {
                let a = self.pop_i64();
                let b = self.pop_i64();
                self.push(if (b as i32) >= (a as i32) { 1 } else { 0 });
            }

            // load
            0x40 => {
                let address = self.pop_address();
                self.push(self.load_i64(address));
            }
            // load from register
            0x41 => {
                let register = self.consume();
                match register {
                    0x01 => self.push(self.bp as i64),
                    0x02 => self.push(self.sp as i64),
                    _ => return Err(RuntimeError::UnknownRegister(register)),
                }
            }
            // store
            0x42 => {
                let value = self.pop_i64();
                let address = self.pop_address();
                if print_stacks || print_memory_store {
                    println!("store 0x{:x} {:?}", address, Value::from_u64(value as u64));
                }
                self.store_i64(address as u32 as u64, value);

                assert!(
                    address != self.bp as u64,
                    "Cannot store into <prev_bp>\n{}",
                    self.show_stacks()
                );
                assert!(
                    address != self.bp as u64 + 8,
                    "Cannot store into <prev_pc>\n{}",
                    self.show_stacks()
                );
            }
            // store into register
            0x43 => {
                let register = self.consume();
                let value = self.pop_i64();
                assert!(
                    matches!(Value::from_u64(value as u64), Value::Int(_)),
                    "Register value must be an integer, but got {:?}",
                    Value::from_u64(value as u64)
                );
                match register {
                    0x01 => self.bp = value as usize,
                    0x02 => self.sp = value as usize,
                    _ => return Err(RuntimeError::UnknownRegister(register)),
                }
            }
            // load8
            0x44 => {
                let address = self.pop_address();
                self.push(self.memory[address as usize] as u8 as u64 as i64);
            }
            // store8
            0x45 => {
                let value = self.pop_i64() & 0xff;
                let address = self.pop_address();
                if print_stacks || print_memory_store {
                    println!("store8 0x{:x} {:x}", address as u32, value);
                }
                self.store_u8(address as u32, value as u8);
            }
            // load32
            0x46 => {
                let address = self.pop_address();
                self.push(self.load_i64(address) as u32 as u64 as i64);
            }
            // store32
            0x47 => {
                let value = self.pop_i64() & 0xffffffff;
                let address = self.pop_address();
                if print_stacks || print_memory_store {
                    println!("store32 0x{:x} {:x}", address as u32, value);
                }
                self.store_u32(address as u32, value as u32);
            }

            // int to float
            0x50 => {
                let a = self.pop_i64();
                self.push_f32(a as f32);
            }
            // float to int
            0x51 => {
                let a = self.pop_f32();
                self.push(a as f32 as i32 as i64);
            }
            // int to byte
            0x52 => {
                let a = self.pop_i64();
                self.push(a as u8 as i64);
            }

            // debug
            // source map
            0x61 => {
                let start = self.consume_u64();
                let end = self.consume_u64();

                let mut hit = None;
                for bp in &self.breakpoints {
                    if *bp >= self.prev_source_map.1 && *bp < end as usize {
                        hit = Some(bp);
                    }
                }

                self.prev_source_map = (start as usize, end as usize);

                if hit.is_some() {
                    return Ok(ControlFlow::HitBreakpoint);
                }
            }

            code => {
                println!(
                    "{:x} {:x?} {:x?}",
                    self.pc,
                    &self.program[0..self.pc],
                    &self.program[self.pc..]
                );
                return Err(RuntimeError::UnknownInstruction(code));
            }
        }

        Ok(if self.pc < self.program.len() && self.pc < 0xffffffff {
            ControlFlow::Continue
        } else {
            ControlFlow::Finish
        })
    }

    pub fn exec(
        &mut self,
        print_stacks: bool,
        print_memory_store: bool,
    ) -> Result<(), RuntimeError> {
        while !matches!(
            self.step(print_stacks, print_memory_store)?,
            ControlFlow::Finish
        ) {}

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_working_with_memory() {
        let cases = vec![
            (
                vec![
                    0x40, // load
                ],
                8,
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                ],
            ),
            (
                vec![
                    0x44, // load8
                ],
                8,
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x4a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
            ),
            (
                vec![
                    0x46, // load32
                ],
                8,
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x4a, 0x5b, 0x6c, 0x7d, 0x00, 0x00, 0x00, 0x00, //
                ],
            ),
            (
                vec![
                    0x46, // load32
                ],
                8,
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x8e, 0x9f, 0xa0, 0xb1, 0x00, 0x00, 0x00, 0x00, //
                ],
            ),
            (
                vec![
                    0x47, // store32
                ],
                8,
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x8e, 0x9f, 0xa0, 0xb1, //
                    0x12, 0x34, 0x56, 0x78, 0x9a, 0x0b, 0x1c, 0x2d, //
                    0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
                vec![
                    0x4a, 0x5b, 0x6c, 0x7d, 0x12, 0x34, 0x56, 0x78, //
                    0x12, 0x34, 0x56, 0x78, 0x9a, 0x0b, 0x1c, 0x2d, //
                    0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, //
                ],
            ),
        ];

        for (program, sp, memory, want) in cases {
            let mut runtime = Runtime::new(memory.len(), program.clone());
            runtime.sp = sp;
            runtime.bp = sp;
            runtime.memory = memory;
            runtime.exec(false, false).unwrap();

            assert_eq!(
                runtime.memory, want,
                "{:x?}, {:x?}",
                program, runtime.memory
            );
        }
    }
}
