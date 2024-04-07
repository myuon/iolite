use nanoid::nanoid;

use super::{
    ir::{IrDecl, IrModule, IrOp, IrTerm},
    vm::Instruction,
};

#[derive(Debug)]
pub enum VmCodeGeneratorError {}

#[derive(Debug)]
struct Scope {
    locals: Vec<(String, usize)>,
    stack_pointer: usize,
}

#[derive(Debug)]
pub struct VmCodeGenerator {
    locals: Vec<Scope>,
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
        for scope in self.locals.iter().rev() {
            if let Some((_, index)) = scope.locals.iter().rev().find(|s| s.0 == name) {
                return self.stack_pointer + 1 - index;
            }
        }

        panic!("Symbol not found in stack: {}", name);
    }

    fn new_scope(&mut self) {
        self.locals.push(Scope {
            locals: vec![],
            stack_pointer: self.stack_pointer,
        });
    }

    fn rewind_scope(&mut self) {
        let scope = self.locals.pop().unwrap();

        self.copy_into(self.stack_pointer - scope.stack_pointer);
        // NOTE: block returns a value, so we need to keep the stack pointer
        self.pop_until(scope.stack_pointer + 1);
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
            LoadBp => {
                self.stack_pointer += 1;
            }
            StoreBp => {
                self.stack_pointer -= 1;
            }
            LoadSp => {}
            StoreSp => {}
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
            Instruction::Return => {}
            Xor | And | Or => {
                self.stack_pointer -= 1;
            }
            Not => {}
            Eq | NotEq | Lt | Gt | Le | Ge => {
                self.stack_pointer -= 1;
            }
            Label(_) => {}
            JumpTo(_) => {}
            CallLabel(_) => {}
            JumpIfTo(_) => {
                self.stack_pointer -= 1;
            }
            Debug(_) => {}
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
                self.emit(Instruction::Push(n as u32));
            }
            IrTerm::Ident(i) => {
                let index = self.find_symbol_in_stack(&i);
                assert!(
                    index > 0,
                    "Index must be greater than 0: index={}, ident={}",
                    index,
                    i
                );

                self.emit(Instruction::PushLocal(index));
            }
            IrTerm::Let { name, value } => {
                self.term(*value)?;
                self.locals
                    .last_mut()
                    .unwrap()
                    .locals
                    .push((name, self.stack_pointer));
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
                self.new_scope();

                for term in terms {
                    self.term(term)?;
                }

                // // NOTE: copy the result of this block to the original position
                // self.copy_into(self.stack_pointer - stack_pointer);
                // self.pop_until(stack_pointer + 1);
                self.rewind_scope();
            }
            IrTerm::Return(value) => {
                self.term(*value)?;

                // copy the return value
                self.emit(Instruction::LoadBp);
                self.emit(Instruction::Push(4 * 2));
                self.emit(Instruction::Add);
                self.emit(Instruction::PushLocal(2));
                self.emit(Instruction::Load);
                self.emit(Instruction::Store);
                self.emit(Instruction::Debug("copy return value".to_string()));

                // %sp = %bp
                self.emit(Instruction::LoadBp);
                self.emit(Instruction::StoreSp);
                self.emit(Instruction::Debug("%sp = %bp".to_string()));

                // %bp = %prev_bp
                self.emit(Instruction::StoreBp);
                self.emit(Instruction::Debug("restore %bp".to_string()));

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
                // discard the result of the block
                self.emit(Instruction::Pop);

                self.emit(Instruction::JumpTo(label_while_start.clone()));

                self.emit(Instruction::Label(label_while_end.clone()));
            }
            IrTerm::If { cond, then, else_ } => {
                let label_id = nanoid!();
                let label_if_else = format!("if_else_{}", label_id);
                let label_if_end = format!("if_end_{}", label_id);
                let stack_pointer = self.stack_pointer;

                self.term(*cond)?;
                self.emit(Instruction::Not);
                self.emit(Instruction::JumpIfTo(label_if_else.clone()));

                self.term(*then)?;
                self.emit(Instruction::JumpTo(label_if_end.clone()));

                self.stack_pointer = stack_pointer;
                self.emit(Instruction::Label(label_if_else.clone()));

                self.term(*else_)?;
                self.emit(Instruction::Label(label_if_end.clone()));
                self.stack_pointer = stack_pointer + 1;
            }
        }

        Ok(())
    }

    fn decl(&mut self, decl: IrDecl) -> Result<(), VmCodeGeneratorError> {
        match decl {
            IrDecl::Fun { name, args, body } => {
                self.emit(Instruction::Label(name.clone()));

                // prologue
                self.emit(Instruction::Debug(format!("prologue start: {}", name)));
                self.emit(Instruction::LoadBp);
                self.emit(Instruction::LoadSp);
                self.emit(Instruction::StoreBp);

                self.emit(Instruction::Debug(format!("prologue end: {}", name)));

                self.term(*body)?;
            }
        }

        Ok(())
    }

    pub fn module(&mut self, module: IrModule) -> Result<(), VmCodeGeneratorError> {
        for decl in module.decls {
            self.decl(decl)?;
        }

        Ok(())
    }

    pub fn program(&mut self, module: IrModule) -> Result<(), VmCodeGeneratorError> {
        self.emit(Instruction::Push(0)); // 1 word for the return value
        self.emit(Instruction::Push(0xffffffff)); // return address

        self.module(module)?;

        self.emit(Instruction::Label("exit".to_string()));

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
