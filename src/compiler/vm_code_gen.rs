use super::{ir::IrTerm, vm::Instruction};

#[derive(Debug)]
pub enum VmCodeGeneratorError {}

#[derive(Debug)]
pub struct VmCodeGenerator {
    locals: Vec<String>,
    pub code: Vec<Instruction>,
}

impl VmCodeGenerator {
    pub fn new() -> Self {
        Self {
            locals: vec![],
            code: vec![],
        }
    }

    fn find_symbol_in_stack(&self, name: &str) -> usize {
        self.locals.len() - self.locals.iter().position(|s| s == name).unwrap()
    }

    pub fn term(&mut self, ir: IrTerm) -> Result<(), VmCodeGeneratorError> {
        match ir {
            IrTerm::Nil => {
                self.code.push(Instruction::Push(0));
            }
            IrTerm::Integer(n) => {
                self.code.push(Instruction::Push(n));
            }
            IrTerm::Ident(i) => {
                let index = self.find_symbol_in_stack(&i);
                self.code.push(Instruction::PushLocal(index));
            }
            IrTerm::Let { name, value } => {
                self.term(*value)?;
                self.locals.push(name);
            }
            IrTerm::Op { op, args } => {
                for arg in args {
                    self.term(arg)?;
                }

                match op {
                    super::ir::IrOp::Add => {
                        self.code.push(Instruction::Add);
                    }
                    super::ir::IrOp::Sub => {
                        self.code.push(Instruction::Sub);
                    }
                    super::ir::IrOp::Mul => {
                        self.code.push(Instruction::Mul);
                    }
                    super::ir::IrOp::Div => {
                        self.code.push(Instruction::Div);
                    }
                }
            }
            IrTerm::Block { terms } => {
                for term in terms {
                    self.term(term)?;
                }
            }
            IrTerm::Return(value) => {
                self.term(*value)?;
                self.code.push(Instruction::Return);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::ir::IrOp;

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
        ];

        for (ir, expected) in cases {
            let mut gen = VmCodeGenerator::new();
            gen.term(ir).unwrap();

            assert_eq!(gen.code, expected);
        }
    }
}
