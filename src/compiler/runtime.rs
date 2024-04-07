use super::vm::Instruction;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UnknownInstruction(u8),
    UnknownRegister(u8),
}

pub struct Runtime {
    memory: Vec<u8>,
    sp: usize,
    bp: usize,
    pc: usize,
    program: Vec<u8>,
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

    pub fn memory_view_32(&self) -> Vec<u32> {
        let mut view = vec![];
        for i in (self.sp..self.memory.len()).step_by(4) {
            view.push(u32::from_le_bytes([
                self.memory[i],
                self.memory[i + 1],
                self.memory[i + 2],
                self.memory[i + 3],
            ]));
        }

        view.reverse();

        view
    }

    fn load_i32(&self, address: u32) -> i32 {
        i32::from_le_bytes([
            self.memory[address as usize],
            self.memory[address as usize + 1],
            self.memory[address as usize + 2],
            self.memory[address as usize + 3],
        ])
    }

    fn store_i32(&mut self, address: u32, value: i32) {
        self.memory[address as usize..(address as usize + 4)].copy_from_slice(&value.to_le_bytes());
    }

    pub fn pop(&mut self) -> i32 {
        let val = self.load_i32(self.sp as u32);
        self.sp += 4;
        val
    }

    fn push(&mut self, val: i32) {
        self.sp -= 4;
        self.store_i32(self.sp as u32, val);
    }

    fn pop_address(&mut self) -> u32 {
        self.pop() as u32
    }

    fn print_stack(&self, next: &Instruction) {
        let mut p = self.sp;
        while p < self.memory.len() {
            let val = self.load_i32(p as u32);

            if p == self.sp {
                print!("S {:>4} ", val as u32);
            } else if p == self.bp {
                print!("B {:>4} ", val as u32);
            } else {
                print!("| {:>4} ", val as u32);
            }
            p += 4;
        }

        println!("| next: {:?}", next);
    }

    fn consume(&mut self) -> u8 {
        let code = self.program[self.pc];
        self.pc += 1;

        code
    }

    pub fn exec(&mut self) -> Result<(), RuntimeError> {
        while self.pc < self.program.len() && self.pc < 0xffffffff {
            self.print_stack(&{
                let inst = Instruction::from_byte(&self.program[self.pc..]).unwrap();

                match inst {
                    Instruction::Push(_) => Instruction::Push(u32::from_le_bytes([
                        self.program[self.pc + 1],
                        self.program[self.pc + 2],
                        self.program[self.pc + 3],
                        self.program[self.pc + 4],
                    ])),
                    _ => inst,
                }
            });

            match self.consume() {
                // push
                0x01 => {
                    let imm = i32::from_le_bytes([
                        self.consume(),
                        self.consume(),
                        self.consume(),
                        self.consume(),
                    ]);
                    self.push(imm);
                }
                // pop
                0x02 => {
                    self.pop();
                }
                // call
                0x03 => {
                    let address = self.pop_address();
                    self.push(self.pc as i32);
                    self.pc = address as usize;
                }
                // return
                0x04 => {
                    self.pc = self.pop() as usize;
                }
                // jump
                0x05 => {
                    self.pc = self.pop() as usize;
                }
                // jump_if
                0x06 => {
                    let address = self.pop_address();
                    let val = self.pop();
                    if val != 0 {
                        self.pc = address as usize;
                    }
                }

                // add
                0x10 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b + a);
                }
                // sub
                0x11 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b - a);
                }
                // mul
                0x12 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b * a);
                }
                // div
                0x13 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b / a);
                }

                // xor
                0x20 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b ^ a);
                }
                // and
                0x21 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b & a);
                }
                // or
                0x22 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b | a);
                }
                // not
                0x23 => {
                    let a = self.pop();
                    self.push(if a == 0 { 1 } else { 0 });
                }

                // eq
                0x30 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b == a { 1 } else { 0 });
                }
                // not_eq
                0x31 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b != a { 1 } else { 0 });
                }
                // lt
                0x32 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b < a { 1 } else { 0 });
                }
                // gt
                0x33 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b > a { 1 } else { 0 });
                }
                // le
                0x34 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b <= a { 1 } else { 0 });
                }
                // ge
                0x35 => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b >= a { 1 } else { 0 });
                }

                // load
                0x40 => {
                    let address = self.pop_address();
                    self.push(self.load_i32(address));
                }
                // load from register
                0x41 => {
                    let register = self.consume();
                    match register {
                        0x01 => self.push(self.bp as i32),
                        0x02 => self.push(self.sp as i32),
                        _ => return Err(RuntimeError::UnknownRegister(register)),
                    }
                }
                // store
                0x42 => {
                    let value = self.pop();
                    let address = self.pop_address();
                    self.store_i32(address, value);
                }
                // store into register
                0x43 => {
                    let register = self.consume();
                    let value = self.pop();
                    match register {
                        0x01 => self.bp = value as usize,
                        0x02 => self.sp = value as usize,
                        _ => return Err(RuntimeError::UnknownRegister(register)),
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
        }

        Ok(())
    }
}
