#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Add,
    Sub,
    Mul,
    Div,
    Load,
    Store,
    Push(i32),
    PushLocal(usize),
    Pop,
    Jump,
    JumpIf,
    Call,
    Return,
    Xor,
    And,
    Or,
    Not,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
}

pub struct Vm {
    memory: Vec<u8>,
    sp: usize,
    pc: usize,
    program: Vec<Instruction>,
}

impl Vm {
    pub fn new(size: usize, program: Vec<Instruction>) -> Self {
        let memory = vec![0; size];
        let sp = memory.len() - 4;

        Self {
            memory,
            sp,
            pc: 0,
            program,
        }
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
        for (i, byte) in value.to_le_bytes().iter().enumerate() {
            self.memory[address as usize + i] = *byte;
        }
    }

    pub fn pop(&mut self) -> i32 {
        self.sp += 4;
        self.load_i32(self.sp as u32)
    }

    fn push(&mut self, val: i32) {
        self.store_i32(self.sp as u32, val);
        self.sp -= 4;
    }

    fn pop_address(&mut self) -> u32 {
        (self.pop() as u32) & 0xFFFF_FFFC
    }

    fn print_stack(&self) {
        print!("| ");
        let mut p = 0;
        while p < self.memory.len() {
            let val = self.load_i32(p as u32);

            if p == self.sp {
                print!("{:>4} >", val as u32);
            } else {
                print!("{:>4} |", val as u32);
            }
            p += 4;
        }

        println!();
    }

    pub fn exec(&mut self) {
        while self.pc < self.program.len() {
            self.print_stack();

            match self.program[self.pc] {
                Instruction::Add => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b + a);
                }
                Instruction::Sub => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b - a);
                }
                Instruction::Mul => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b * a);
                }
                Instruction::Div => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b / a);
                }
                Instruction::Load => {
                    let addr = self.pop_address();
                    self.push(self.load_i32(addr));
                }
                Instruction::Store => {
                    let val = self.pop();
                    let addr = self.pop_address();
                    self.store_i32(addr, val);
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::Push(val) => {
                    self.push(val);
                }
                Instruction::PushLocal(index) => {
                    let addr = self.sp + 4 * index;
                    self.push(addr as i32);
                }
                Instruction::Jump => {
                    self.pc = self.pop() as usize;
                    continue;
                }
                Instruction::JumpIf => {
                    let addr = self.pop();
                    if self.pop() != 0 {
                        self.pc = addr as usize;
                        continue;
                    }
                }
                Instruction::Call => {
                    let addr = self.pop_address();
                    self.push(self.pc as i32);
                    self.pc = addr as usize;
                    continue;
                }
                Instruction::Return => {
                    self.pc = self.pop() as usize;
                    continue;
                }
                Instruction::Xor => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b ^ a);
                }
                Instruction::And => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b & a);
                }
                Instruction::Or => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(b | a);
                }
                Instruction::Not => {
                    let a = self.pop();
                    self.push(!a);
                }
                Instruction::Eq => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b == a { 1 } else { 0 });
                }
                Instruction::Lt => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b < a { 1 } else { 0 });
                }
                Instruction::Gt => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b > a { 1 } else { 0 });
                }
                Instruction::Le => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b <= a { 1 } else { 0 });
                }
                Instruction::Ge => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(if b >= a { 1 } else { 0 });
                }
            }

            self.pc += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instructions() {
        use Instruction::*;

        let memory_size = 40;
        let cases = vec![
            (vec![Push(1), Push(2), Push(3)], vec![3, 2, 1]),
            (vec![Push(1), Push(2), Push(3), Pop, Pop], vec![1]),
            (vec![Push(1), Push(2), Add], vec![3]),
            (vec![Push(10), Push(2), Sub], vec![8]),
            (vec![Push(10), Push(2), Mul], vec![20]),
            (vec![Push(10), Push(2), Div], vec![5]),
            (
                vec![Push(1), Push(2), Push(3), Push(memory_size - 4 * 2), Load],
                vec![2, 3, 2, 1],
            ),
            (
                vec![
                    Push(1),
                    Push(2),
                    Push(3),
                    Push(memory_size - 4 * 2),
                    Push(10),
                    Store,
                ],
                vec![3, 10, 1],
            ),
            (vec![Push(1), PushLocal(1), Push(2), Store], vec![2]),
        ];

        for (program, expected) in cases {
            let mut vm = Vm::new(memory_size as usize, program);
            vm.exec();
            assert_eq!(
                vm.memory[(vm.sp + 4)..],
                expected
                    .iter()
                    .flat_map(|&x| (x as u32).to_le_bytes().to_vec())
                    .collect::<Vec<u8>>()
            );
        }
    }

    #[test]
    fn fib() {
        use Instruction::*;

        let memory_size: i32 = 40;
        let mut vm = Vm::new(
            memory_size as usize,
            vec![
                Push(1),                   // a
                Push(1),                   // b
                Push(0),                   // c
                Push(10),                  // *while 10
                Push(memory_size - 4 * 3), // load(c)
                Load,
                Sub, // c-10
                Push(0),
                Eq,
                Push(33),
                JumpIf,                    // jump to *end if c-10 == 0
                Push(memory_size - 4 * 1), // load(a)
                Load,
                Push(memory_size - 4 * 2), // load(b)
                Load,
                Add,                       // t
                Push(memory_size - 4 * 1), // store(a)
                Push(memory_size - 4 * 2), // load(b)
                Load,
                Store,
                Push(memory_size - 4 * 2), // store(b)
                Push(memory_size - 4 * 4), // load(t)
                Load,
                Store,
                Push(memory_size - 4 * 3), // store(c)
                Push(1),
                Push(memory_size - 4 * 3), // load(c)
                Load,
                Add, // c+1
                Store,
                Pop,
                Push(3), // jump *while
                Jump,
                Push(memory_size - 4 * 2), // *end load(b)
                Load,
            ],
        );
        vm.exec();

        assert_eq!(vm.pop(), 144);
    }
}
