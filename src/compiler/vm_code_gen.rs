use nanoid::nanoid;

use super::{
    ir::{IrOp, IrTerm},
    vm::Instruction,
};

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

    fn term_left_value(&mut self, ir: IrTerm) -> Result<(), VmCodeGeneratorError> {
        match ir {
            IrTerm::Ident(i) => {
                let index = self.find_symbol_in_stack(&i);
                self.code.push(Instruction::PushLocal(index));

                self.locals.push(format!("{}_left_value", i));
            }
            _ => todo!(),
        }

        Ok(())
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
                self.code.push(op);
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
            IrTerm::Load(term) => {
                self.term(*term)?;
                self.code.push(Instruction::Load);
            }
            IrTerm::Store(addr, value) => {
                self.term_left_value(*addr)?;
                self.term(*value)?;
                self.code.push(Instruction::Store);
                self.locals.pop();
            }
            IrTerm::While { cond, body } => {
                let label_id = nanoid!();
                let label_while_start = format!("while_start_{}", label_id);
                let label_while_end = format!("while_end_{}", label_id);

                self.code
                    .push(Instruction::Label(label_while_start.clone()));

                self.term(*cond)?;
                self.code.push(Instruction::Not);

                self.code
                    .push(Instruction::JumpIfTo(label_while_end.clone()));

                self.term(*body)?;
                self.code
                    .push(Instruction::JumpTo(label_while_start.clone()));

                self.code.push(Instruction::Label(label_while_end.clone()));
            }
            IrTerm::If { cond, then, else_ } => {
                let label_id = nanoid!();
                let label_if_else = format!("if_else_{}", label_id);
                let label_if_end = format!("if_end_{}", label_id);

                self.term(*cond)?;
                self.code.push(Instruction::Not);
                self.code.push(Instruction::JumpIfTo(label_if_else.clone()));

                self.term(*then)?;
                self.code.push(Instruction::JumpTo(label_if_end.clone()));

                self.code.push(Instruction::Label(label_if_else.clone()));
                self.term(*else_)?;

                self.code.push(Instruction::Label(label_if_end.clone()));
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
                ],
            ),
        ];

        for (ir, expected) in cases {
            let mut gen = VmCodeGenerator::new();
            gen.term(ir).unwrap();

            assert_eq!(gen.code, expected);
        }
    }
}
