use std::{
    io::{stdout, BufWriter, Write},
    sync::{Arc, Mutex},
};

use thiserror::Error;

use super::{ir::Value, vm::Instruction};

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
        self.memory[address as usize..(address as usize + 8)].copy_from_slice(&value.to_le_bytes());
    }

    fn store_u32(&mut self, address: u32, value: u32) {
        self.memory[address as usize..(address as usize + 4)].copy_from_slice(&value.to_le_bytes());
    }

    fn store_u8(&mut self, address: u32, value: u8) {
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
        print!("[{}] |", self.pc);
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

    pub fn step(&mut self, print_stacks: bool) -> Result<ControlFlow, RuntimeError> {
        if print_stacks {
            self.print_stack();
        }

        for bp in &self.breakpoints {
            if &self.pc == bp {
                println!("breakpoint at {}", bp);
            }
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
                    10000 => {
                        #[cfg(feature = "gui")]
                        unsafe {
                            let mut init_options = libui_ffi::uiInitOptions {
                                Size: std::mem::size_of::<libui_ffi::uiInitOptions>(),
                            };

                            let err = libui_ffi::uiInit(&mut init_options);
                            if !err.is_null() {
                                let error_string = std::ffi::CStr::from_ptr(err).to_str().unwrap();
                                panic!("Error: {}", error_string);
                            }
                        }

                        #[cfg(not(feature = "gui"))]
                        todo!();

                        self.push(Value::Nil.as_u64() as i64);
                    }
                    10001 => {
                        let title_address = self.pop_address();
                        let width = self.pop_i64();
                        let height = self.pop_i64();
                        let has_menubar = self.pop_i64();

                        #[cfg(feature = "gui")]
                        unsafe {
                            extern "C" fn c_callback(
                                window: *mut libui_ffi::uiWindow,
                                _data: *mut std::ffi::c_void,
                            ) -> i32 {
                                unsafe {
                                    libui_ffi::uiControlDestroy(
                                        window as *mut libui_ffi::uiControl,
                                    );
                                    libui_ffi::uiQuit();
                                }

                                0
                            }

                            let window = libui_ffi::uiNewWindow(
                                std::ffi::CString::from_vec_unchecked(
                                    self.memory[title_address as usize..].to_vec(),
                                )
                                .as_ptr(),
                                width as i32,
                                height as i32,
                                has_menubar as i32,
                            );

                            libui_ffi::uiWindowOnClosing(
                                window,
                                Some(c_callback),
                                std::ptr::null_mut(),
                            );
                            libui_ffi::uiControlShow(window as *mut libui_ffi::uiControl);
                            libui_ffi::uiMain();
                        }

                        #[cfg(not(feature = "gui"))]
                        todo!();

                        self.push(Value::Nil.as_u64() as i64);
                    }
                    _ => todo!(),
                }
            }
            // return
            0x04 => {
                self.pc = self.pop_i64() as usize;
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

                for i in 0..length {
                    let b = self.consume();
                    self.memory[offset as usize + i as usize] = b;
                }
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
                if print_stacks {
                    println!("store {} {:x}", address as u32, value);
                }
                self.store_i64(address as u32 as u64, value);
            }
            // store into register
            0x43 => {
                let register = self.consume();
                let value = self.pop_i64();
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
                let value = self.pop_i64();
                let address = self.pop_address();
                if print_stacks {
                    println!("store8 {} {:x}", address as u32, value);
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
                let value = self.pop_i64();
                let address = self.pop_address();
                if print_stacks {
                    println!("store32 {} {:x}", address as u32, value);
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

    pub fn exec(&mut self, print_stacks: bool) -> Result<(), RuntimeError> {
        while !matches!(self.step(print_stacks)?, ControlFlow::Finish) {}

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
            runtime.exec(false).unwrap();

            assert_eq!(
                runtime.memory, want,
                "{:x?}, {:x?}",
                program, runtime.memory
            );
        }
    }
}
