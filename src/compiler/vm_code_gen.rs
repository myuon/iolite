use nanoid::nanoid;

use super::{
    ir::{IrOp, IrTerm},
    vm::Instruction,
};

#[derive(Debug)]
pub enum VmCodeGeneratorError {}

#[derive(Debug)]
pub struct VmCodeGenerator {
    locals: Vec<(String, usize)>,
    stack_pointer: usize,
    pub code: Vec<Instruction>,
}

impl VmCodeGenerator {
    pub fn new() -> Self {
        Self {
            locals: vec![],
            stack_pointer: 0,
            code: vec![],
        }
    }

    fn find_symbol_in_stack(&self, name: &str) -> usize {
        self.stack_pointer - self.locals.iter().rev().find(|s| s.0 == name).unwrap().1 + 1
    }

    fn pop_until(&mut self, size: usize) {
        while self.stack_pointer > size {
            self.emit(Instruction::Pop);
        }
    }

    fn emit(&mut self, inst: Instruction) {
        use Instruction::*;

        match inst {
            Add | Sub | Mul | Div => {
                self.stack_pointer -= 1;
            }
            Load => {}
            Store => {
                self.stack_pointer -= 2;
            }
            Push(_) | PushLocal(_) => {
                self.stack_pointer += 1;
            }
            Pop => {
                self.stack_pointer -= 1;
            }
            Jump => {
                self.stack_pointer -= 1;
            }
            JumpIf => {
                self.stack_pointer -= 2;
            }
            Instruction::Call => todo!(),
            Instruction::Return => todo!(),
            Xor | And | Or => {
                self.stack_pointer -= 1;
            }
            Not => {}
            Eq | NotEq | Lt | Gt | Le | Ge => {
                self.stack_pointer -= 1;
            }
            Label(_) => {}
            JumpTo(_) => {}
            JumpIfTo(_) => {
                self.stack_pointer -= 1;
            }
        }

        self.code.push(inst);
    }

    fn copy_into(&mut self, index: usize) {
        self.emit(Instruction::PushLocal(index));
        self.emit(Instruction::PushLocal(2));
        self.emit(Instruction::Load);
        self.emit(Instruction::Store);
    }

    fn term_left_value(&mut self, ir: IrTerm) -> Result<(), VmCodeGeneratorError> {
        match ir {
            IrTerm::Ident(i) => {
                let index = self.find_symbol_in_stack(&i);
                self.emit(Instruction::PushLocal(index));
            }
            _ => todo!(),
        }

        Ok(())
    }

    pub fn term(&mut self, ir: IrTerm) -> Result<(), VmCodeGeneratorError> {
        match ir {
            IrTerm::Nil => {
                self.emit(Instruction::Push(0));
            }
            IrTerm::Integer(n) => {
                self.emit(Instruction::Push(n));
            }
            IrTerm::Ident(i) => {
                let index = self.find_symbol_in_stack(&i);
                self.emit(Instruction::PushLocal(index));
            }
            IrTerm::Let { name, value } => {
                self.term(*value)?;
                self.locals.push((name, self.stack_pointer));
            }
            IrTerm::Op { op, args } => {
                for arg in args {
                    self.term(arg)?;
                }

                let op = match op {
                    IrOp::Add => Instruction::Add,
                    IrOp::Sub => Instruction::Sub,
                    IrOp::Mul => Instruction::Mul,
                    IrOp::Div => Instruction::Div,
                    IrOp::And => Instruction::And,
                    IrOp::Or => Instruction::Or,
                    IrOp::Eq => Instruction::Eq,
                    IrOp::Lt => Instruction::Lt,
                    IrOp::Gt => Instruction::Gt,
                    IrOp::Le => Instruction::Le,
                    IrOp::Ge => Instruction::Ge,
                    IrOp::NotEq => Instruction::NotEq,
                };
                self.emit(op);
            }
            IrTerm::Block { terms } => {
                let stack_pointer = self.stack_pointer;

                for term in terms {
                    self.term(term)?;
                }

                // NOTE: copy the result of this block to the original position
                self.copy_into(self.stack_pointer - stack_pointer);
                self.pop_until(stack_pointer + 1);
            }
            IrTerm::Return(value) => {
                self.term(*value)?;
                self.emit(Instruction::Return);
            }
            IrTerm::Load(term) => {
                self.term(*term)?;
                self.emit(Instruction::Load);
            }
            IrTerm::Store(addr, value) => {
                self.term_left_value(*addr)?;
                self.term(*value)?;
                self.emit(Instruction::Store);
            }
            IrTerm::While { cond, body } => {
                let label_id = nanoid!();
                let label_while_start = format!("while_start_{}", label_id);
                let label_while_end = format!("while_end_{}", label_id);

                self.emit(Instruction::Label(label_while_start.clone()));

                self.term(*cond)?;
                self.emit(Instruction::Not);

                self.emit(Instruction::JumpIfTo(label_while_end.clone()));

                self.term(*body)?;

                self.emit(Instruction::JumpTo(label_while_start.clone()));

                self.emit(Instruction::Label(label_while_end.clone()));
            }
            IrTerm::If { cond, then, else_ } => {
                let label_id = nanoid!();
                let label_if_else = format!("if_else_{}", label_id);
                let label_if_end = format!("if_end_{}", label_id);
                let stack_pointer = self.locals.len();

                self.term(*cond)?;
                self.emit(Instruction::Not);
                self.emit(Instruction::JumpIfTo(label_if_else.clone()));

                self.term(*then)?;
                self.emit(Instruction::JumpTo(label_if_end.clone()));

                self.pop_until(stack_pointer);

                self.emit(Instruction::Label(label_if_else.clone()));

                self.term(*else_)?;
                self.emit(Instruction::Label(label_if_end.clone()));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{ir::IrOp, vm::Vm};

    use super::*;

    #[test]
    fn test_term() {
        let cases = vec![
            (
                IrTerm::Op {
                    op: IrOp::Add,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![Instruction::Push(1), Instruction::Push(2), Instruction::Add],
            ),
            (
                IrTerm::Op {
                    op: IrOp::Sub,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![Instruction::Push(1), Instruction::Push(2), Instruction::Sub],
            ),
            (
                IrTerm::Op {
                    op: IrOp::Mul,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![Instruction::Push(1), Instruction::Push(2), Instruction::Mul],
            ),
            (
                IrTerm::Op {
                    op: IrOp::Div,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![Instruction::Push(1), Instruction::Push(2), Instruction::Div],
            ),
            (
                IrTerm::Block {
                    terms: vec![
                        IrTerm::Let {
                            name: "a".to_string(),
                            value: Box::new(IrTerm::Integer(1)),
                        },
                        IrTerm::Store(
                            Box::new(IrTerm::Ident("a".to_string())),
                            Box::new(IrTerm::Integer(2)),
                        ),
                        IrTerm::Load(Box::new(IrTerm::Ident("a".to_string()))),
                    ],
                },
                vec![
                    Instruction::Push(1),
                    Instruction::PushLocal(1),
                    Instruction::Push(2),
                    Instruction::Store,
                    Instruction::PushLocal(1),
                    Instruction::Load,
                    Instruction::PushLocal(2),
                    Instruction::PushLocal(2),
                    Instruction::Load,
                    Instruction::Store,
                    Instruction::Pop,
                ],
            ),
        ];

        for (ir, expected) in cases {
            let mut gen = VmCodeGenerator::new();
            gen.term(ir.clone()).unwrap();

            assert_eq!(gen.code, expected, "ir: {:?}", ir);
        }
    }

    #[test]
    fn test_insert_at() {
        let mut gen = VmCodeGenerator::new();
        gen.emit(Instruction::Push(1));
        gen.emit(Instruction::Push(2));
        gen.emit(Instruction::Push(3));
        gen.emit(Instruction::Push(10));
        gen.copy_into(3);

        let mut vm = Vm::new(1000, gen.code);
        vm.exec().unwrap();

        assert_eq!(vm.memory_view_32(), vec![1, 10, 3, 10]);
    }
}
