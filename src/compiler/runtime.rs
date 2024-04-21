use super::{ir::Value, vm::Instruction};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UnknownInstruction(u8),
    UnknownRegister(u8),
}

pub struct Runtime {
    pub(crate) memory: Vec<u8>,
    pub(crate) sp: usize,
    pub(crate) bp: usize,
    pc: usize,
    pub(crate) program: Vec<u8>,
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
        }
    }

    pub fn init(&mut self, size: usize, program: Vec<u8>) {
        self.memory = vec![0; size];
        self.bp = self.memory.len();
        self.sp = self.bp;
        self.pc = 0;
        self.program = program;
    }

    pub fn get_stack_frames(&self) -> Vec<usize> {
        let mut frames = vec![];
        let mut bp = self.bp;
        while bp > 0 && bp < self.memory.len() {
            frames.push(bp);
            bp = self.load_i64(bp as u64) as usize;
        }

        frames
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

    fn print_stack(&self, next: &Instruction) {
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

        println!(
            " >> next: {}",
            match next {
                Instruction::Push(val) => format!("Push({:?})", val),
                _ => format!("{:?}", next),
            },
        );
    }

    fn consume(&mut self) -> u8 {
        let code = self.program[self.pc];
        self.pc += 1;

        code
    }

    pub fn step(&mut self, print_stacks: bool) -> Result<bool, RuntimeError> {
        if print_stacks {
            self.print_stack(&{
                let inst =
                    Instruction::from_byte(&self.program[self.pc..]).unwrap_or(Instruction::Nop);

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
            });
        }

        let inst = self.consume();
        match inst {
            // push
            0x01 => {
                let imm = i64::from_le_bytes([
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                ]);
                self.push(imm);
            }
            // pop
            0x02 => {
                self.pop_i64();
            }
            // call
            0x03 => {
                let pc = self.pop_i64();
                self.push(self.pc as i64);
                self.pc = pc as usize;
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
                let offset = u64::from_le_bytes([
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                ]);
                let length = u64::from_le_bytes([
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                    self.consume(),
                ]);

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

        Ok(self.pc < self.program.len() && self.pc < 0xffffffff)
    }

    pub fn exec(&mut self, print_stacks: bool) -> Result<(), RuntimeError> {
        while self.step(print_stacks)? {}

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
