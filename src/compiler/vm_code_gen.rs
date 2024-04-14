use nanoid::nanoid;

use super::{
    ir::{IrDecl, IrModule, IrOp, IrTerm},
    vm::Instruction,
};

#[derive(Debug)]
pub enum VmCodeGeneratorError {
    ArityNotFound(String),
    LocalNotFound(String),
    IdentNotFound(String),
}

#[derive(Debug)]
struct Scope {
    locals: Vec<(String, usize)>,
    stack_pointer: usize,
}

#[derive(Debug)]
pub struct VmCodeGenerator {
    arity: Vec<String>,
    locals: Vec<Scope>,
    globals: Vec<String>,
    stack_pointer: usize,
    pub code: Vec<Instruction>,
}

impl VmCodeGenerator {
    pub fn new() -> Self {
        Self {
            arity: vec![],
            locals: vec![],
            globals: vec![],
            stack_pointer: 0,
            code: vec![],
        }
    }

    fn is_arity(&self, name: &str) -> bool {
        self.arity.iter().any(|s| s == name)
    }

    fn is_local(&self, name: &str) -> bool {
        self.locals
            .iter()
            .any(|s| s.locals.iter().any(|(n, _)| n == name))
    }

    fn push_arity(&mut self, name: &str) -> Result<(), VmCodeGeneratorError> {
        let index = self
            .arity
            .iter()
            .position(|s| s == name)
            .ok_or(VmCodeGeneratorError::ArityNotFound(name.to_string()))?;

        self.emit(Instruction::LoadBp);
        // NOTE: 2 words for return address and return value
        self.emit(Instruction::Push((2 + index as u32) * 4));
        self.emit(Instruction::AddInt);

        Ok(())
    }

    fn push_local(&mut self, index: usize) {
        assert!(index > 0, "Index must be greater than 0: index={}", index);
        self.emit(Instruction::LoadSp);
        if index > 1 {
            self.emit(Instruction::Push((index - 1) as u32 * 4));
            self.emit(Instruction::AddInt);
        }
    }

    fn find_symbol_in_stack(&self, name: &str) -> Result<usize, VmCodeGeneratorError> {
        for scope in self.locals.iter().rev() {
            if let Some((_, index)) = scope.locals.iter().rev().find(|s| s.0 == name) {
                return Ok(self.stack_pointer + 1 - index);
            }
        }

        Err(VmCodeGeneratorError::LocalNotFound(name.to_string()))
    }

    fn new_scope(&mut self) {
        self.locals.push(Scope {
            locals: vec![],
            stack_pointer: self.stack_pointer,
        });
    }

    fn rewind_scope(&mut self) {
        let scope = self.locals.pop().unwrap();

        if scope.stack_pointer == self.stack_pointer {
            return;
        }
        self.copy_into(self.stack_pointer - scope.stack_pointer);
        // NOTE: block returns a value, so we need to keep the stack pointer
        self.pop_until(scope.stack_pointer + 1);
    }

    fn push_global(&mut self, name: String) {
        let index = self
            .globals
            .iter()
            .position(|s| s == &name)
            .unwrap_or_else(|| {
                self.globals.push(name.clone());
                self.globals.len() - 1
            });
        self.emit(Instruction::Push(index as u32 * 4));
    }

    fn is_global(&self, name: &str) -> bool {
        self.globals.iter().any(|s| s == name)
    }

    fn pop_until(&mut self, size: usize) {
        while self.stack_pointer > size {
            self.emit(Instruction::Pop);
        }
    }

    fn emit(&mut self, inst: Instruction) {
        use Instruction::*;

        match inst {
            AddInt | SubInt | MulInt | DivInt | AddFloat | SubFloat | MulFloat | DivFloat => {
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
            LoadSp => {
                self.stack_pointer += 1;
            }
            StoreSp => {
                self.stack_pointer -= 1;
            }
            Push(_) => {
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
        assert!(index > 0, "Index must be greater than 0: index={}", index);
        self.push_local(index);
        self.push_local(2);
        self.emit(Instruction::Load);
        self.emit(Instruction::Store);
    }

    fn ident(&mut self, name: String) -> Result<(), VmCodeGeneratorError> {
        if self.is_arity(&name) {
            self.push_arity(&name)?;
        } else if self.is_local(&name) {
            let index = self.find_symbol_in_stack(&name)?;
            assert!(
                index > 0,
                "Index must be greater than 0: index={}, ident={}",
                index,
                name
            );

            self.push_local(index);
        } else if self.is_global(&name) {
            self.push_global(name);
        } else {
            return Err(VmCodeGeneratorError::IdentNotFound(name));
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
            IrTerm::Float(f) => {
                self.emit(Instruction::Push(f.to_bits()));
            }
            IrTerm::Ident(i) => {
                self.ident(i)?;
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
                    IrOp::AddInt => Instruction::AddInt,
                    IrOp::SubInt => Instruction::SubInt,
                    IrOp::MulInt => Instruction::MulInt,
                    IrOp::DivInt => Instruction::DivInt,
                    IrOp::And => Instruction::And,
                    IrOp::Or => Instruction::Or,
                    IrOp::Eq => Instruction::Eq,
                    IrOp::Lt => Instruction::Lt,
                    IrOp::Gt => Instruction::Gt,
                    IrOp::Le => Instruction::Le,
                    IrOp::Ge => Instruction::Ge,
                    IrOp::NotEq => Instruction::NotEq,
                    IrOp::AddFloat => Instruction::AddFloat,
                    IrOp::SubFloat => Instruction::SubFloat,
                    IrOp::MulFloat => Instruction::MulFloat,
                    IrOp::DivFloat => Instruction::DivFloat,
                };
                self.emit(op);
            }
            IrTerm::Block { terms } => {
                self.new_scope();

                for term in terms {
                    self.term(term)?;
                }

                self.rewind_scope();
            }
            IrTerm::Return(value) => {
                self.term(*value)?;

                // copy the return value
                self.emit(Instruction::LoadBp);
                self.emit(Instruction::Push(4 * 2));
                self.emit(Instruction::AddInt);
                self.push_local(2);
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
                self.term(*addr)?;
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
            IrTerm::Call { name, args } => {
                self.emit(Instruction::Push(0));
                self.emit(Instruction::Debug(
                    "allocated for the return value".to_string(),
                ));

                // NOTE: push args in the reverse order
                for arg in args.into_iter().rev() {
                    self.term(arg)?;
                }

                self.emit(Instruction::CallLabel(name));
            }
            IrTerm::Index { array, index } => {
                self.term(*array)?;
                self.term(*index)?;
                self.emit(Instruction::AddInt);
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

                self.arity = args;

                self.term(*body)?;
            }
            IrDecl::Let { name } => {
                self.globals.push(name);
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
        self.emit(Instruction::JumpTo("main".to_string()));

        self.module(module)?;

        self.emit(Instruction::Label("exit".to_string()));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::{byte_code_emitter::ByteCodeEmitter, runtime::Runtime};

    use super::*;

    #[test]
    fn test_term() {
        let cases = vec![
            (
                IrTerm::Op {
                    op: IrOp::AddInt,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![
                    Instruction::Push(1),
                    Instruction::Push(2),
                    Instruction::AddInt,
                ],
            ),
            (
                IrTerm::Op {
                    op: IrOp::SubInt,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![
                    Instruction::Push(1),
                    Instruction::Push(2),
                    Instruction::SubInt,
                ],
            ),
            (
                IrTerm::Op {
                    op: IrOp::MulInt,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![
                    Instruction::Push(1),
                    Instruction::Push(2),
                    Instruction::MulInt,
                ],
            ),
            (
                IrTerm::Op {
                    op: IrOp::DivInt,
                    args: vec![IrTerm::Integer(1), IrTerm::Integer(2)],
                },
                vec![
                    Instruction::Push(1),
                    Instruction::Push(2),
                    Instruction::DivInt,
                ],
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
                    Instruction::LoadSp,
                    Instruction::Push(2),
                    Instruction::Store,
                    Instruction::LoadSp,
                    Instruction::Load,
                    Instruction::LoadSp,
                    Instruction::Push(4),
                    Instruction::AddInt,
                    Instruction::LoadSp,
                    Instruction::Push(4),
                    Instruction::AddInt,
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

        let mut emitter = ByteCodeEmitter::new();
        emitter.exec(gen.code).unwrap();

        let mut vm = Runtime::new(1000, emitter.buffer);
        vm.exec().unwrap();

        assert_eq!(vm.memory_view_32(), vec![1, 10, 3, 10]);
    }
}
