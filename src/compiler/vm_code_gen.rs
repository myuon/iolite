use nanoid::nanoid;

use super::{
    ir::{IrDecl, IrModule, IrOp, IrTerm, TypeTag, Value},
    vm::Instruction,
};

#[derive(Debug, PartialEq, Clone)]
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
        self.push_value(Value::Int((2 + index as i32) * Value::size()));
        self.emit(Instruction::AddInt);

        Ok(())
    }

    fn push_local(&mut self, index: usize) {
        assert!(index > 0, "Index must be greater than 0: index={}", index);
        self.emit(Instruction::LoadSp);
        if index > 1 {
            self.push_value(Value::Int((index - 1) as i32 * Value::size()));
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
        self.push_value(Value::Int(index as i32 * Value::size()));
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
            IntToFloat | FloatToInt | IntToByte => {}
            Load | LoadByte => {}
            Store | StoreByte => {
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
            Nop => {}
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

    fn push_value(&mut self, value: Value) {
        self.emit(Instruction::Push(value.as_u64()));
    }

    fn reset_tag_bits(&mut self) {
        self.emit(Instruction::Push(0x00000000FFFFFFFF));
        self.emit(Instruction::And);
    }

    fn attach_tag_bits(&mut self, tag: TypeTag) {
        self.emit(Instruction::Push((tag.to_byte() as u64) << 32));
        self.emit(Instruction::Or);
    }

    pub fn term(&mut self, ir: IrTerm) -> Result<(), VmCodeGeneratorError> {
        match ir {
            IrTerm::Nil => {
                self.push_value(Value::Nil);
            }
            IrTerm::Bool(b) => {
                self.push_value(Value::Bool(b));
            }
            IrTerm::Int(n) => {
                self.push_value(Value::Int(n));
            }
            IrTerm::Float(f) => {
                self.push_value(Value::Float(f));
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
                    self.reset_tag_bits();
                }

                match op {
                    IrOp::IntToPointer => {
                        self.attach_tag_bits(TypeTag::Pointer);
                    }
                    IrOp::PointerToInt => {
                        self.attach_tag_bits(TypeTag::Int);
                    }
                    IrOp::NegateInt => {
                        self.push_value(Value::Int(-1));
                        self.reset_tag_bits();

                        self.emit(Instruction::MulInt);

                        self.attach_tag_bits(TypeTag::Int);
                    }
                    IrOp::NegateFloat => {
                        self.push_value(Value::Float(-1.0));
                        self.reset_tag_bits();

                        self.emit(Instruction::MulFloat);

                        self.attach_tag_bits(TypeTag::Float);
                    }
                    _ => {
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
                            IrOp::IntToFloat => Instruction::IntToFloat,
                            IrOp::FloatToInt => Instruction::FloatToInt,
                            IrOp::IntToByte => Instruction::IntToByte,
                            _ => todo!("{:?}", op),
                        };
                        self.emit(op.clone());

                        self.attach_tag_bits(match op {
                            Instruction::AddInt
                            | Instruction::SubInt
                            | Instruction::MulInt
                            | Instruction::DivInt => TypeTag::Int,
                            Instruction::And
                            | Instruction::Or
                            | Instruction::Lt
                            | Instruction::Gt
                            | Instruction::Le
                            | Instruction::Ge
                            | Instruction::Eq
                            | Instruction::NotEq => TypeTag::Bool,
                            Instruction::AddFloat
                            | Instruction::SubFloat
                            | Instruction::MulFloat
                            | Instruction::DivFloat => TypeTag::Float,
                            Instruction::IntToFloat => TypeTag::Float,
                            Instruction::FloatToInt => TypeTag::Int,
                            Instruction::IntToByte => TypeTag::Byte,
                            _ => unreachable!(),
                        });
                    }
                }
            }
            IrTerm::Items(terms) => {
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
                self.push_value(Value::Int((2 + self.arity.len() as i32) * Value::size()));
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
            IrTerm::Load {
                size,
                address: term,
            } => {
                self.term(*term)?;
                self.reset_tag_bits();
                self.emit(match size {
                    1 => Instruction::LoadByte,
                    8 => Instruction::Load,
                    _ => todo!(),
                });
            }
            IrTerm::Store {
                size,
                address: addr,
                value,
            } => {
                self.term(*addr)?;
                self.reset_tag_bits();
                self.term(*value)?;
                self.emit(match size {
                    1 => Instruction::StoreByte,
                    8 => Instruction::Store,
                    _ => todo!(),
                });
            }
            IrTerm::While { cond, body } => {
                let label_id = nanoid!();
                let label_while_start = format!("while_start_{}", label_id);
                let label_while_end = format!("while_end_{}", label_id);

                self.emit(Instruction::Label(label_while_start.clone()));

                self.term(*cond)?;
                self.reset_tag_bits();
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
                self.reset_tag_bits();
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

                let args_len = args.len();

                // NOTE: push args in the reverse order
                for arg in args.into_iter().rev() {
                    self.term(arg)?;
                }

                self.emit(Instruction::CallLabel(name));

                // NOTE: pop arity
                for _ in 0..args_len {
                    self.emit(Instruction::Pop);
                }
            }
            IrTerm::Index { ptr, index } => {
                self.term(*ptr)?;
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
        self.emit(Instruction::Push(Value::Pointer(0xffffffff).as_u64())); // return address
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
                    args: vec![IrTerm::Int(1), IrTerm::Int(2)],
                },
                vec![
                    Instruction::Push(Value::Int(1).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::Push(Value::Int(2).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::AddInt,
                    Instruction::Push(0),
                    Instruction::Or,
                ],
            ),
            (
                IrTerm::Op {
                    op: IrOp::SubInt,
                    args: vec![IrTerm::Int(1), IrTerm::Int(2)],
                },
                vec![
                    Instruction::Push(Value::Int(1).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::Push(Value::Int(2).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::SubInt,
                    Instruction::Push(0),
                    Instruction::Or,
                ],
            ),
            (
                IrTerm::Op {
                    op: IrOp::MulInt,
                    args: vec![IrTerm::Int(1), IrTerm::Int(2)],
                },
                vec![
                    Instruction::Push(Value::Int(1).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::Push(Value::Int(2).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::MulInt,
                    Instruction::Push(0),
                    Instruction::Or,
                ],
            ),
            (
                IrTerm::Op {
                    op: IrOp::DivInt,
                    args: vec![IrTerm::Int(1), IrTerm::Int(2)],
                },
                vec![
                    Instruction::Push(Value::Int(1).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::Push(Value::Int(2).as_u64()),
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::DivInt,
                    Instruction::Push(0),
                    Instruction::Or,
                ],
            ),
            (
                IrTerm::Items(vec![
                    IrTerm::Let {
                        name: "a".to_string(),
                        value: Box::new(IrTerm::Int(1)),
                    },
                    IrTerm::Store {
                        size: 8,
                        address: Box::new(IrTerm::Ident("a".to_string())),
                        value: Box::new(IrTerm::Int(2)),
                    },
                    IrTerm::Load {
                        size: 8,
                        address: Box::new(IrTerm::Ident("a".to_string())),
                    },
                ]),
                vec![
                    Instruction::Push(Value::Int(1).as_u64()),
                    Instruction::LoadSp,
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::Push(Value::Int(2).as_u64()),
                    Instruction::Store,
                    Instruction::LoadSp,
                    Instruction::Push(0x00000000FFFFFFFF),
                    Instruction::And,
                    Instruction::Load,
                    Instruction::LoadSp,
                    Instruction::Push(Value::Int(8).as_u64()),
                    Instruction::AddInt,
                    Instruction::LoadSp,
                    Instruction::Push(Value::Int(8).as_u64()),
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
        gen.emit(Instruction::Push(Value::Int(1).as_u64()));
        gen.emit(Instruction::Push(Value::Int(2).as_u64()));
        gen.emit(Instruction::Push(Value::Int(3).as_u64()));
        gen.emit(Instruction::Push(Value::Int(10).as_u64()));
        gen.copy_into(3);

        let mut emitter = ByteCodeEmitter::new();
        emitter.exec(gen.code).unwrap();

        let mut vm = Runtime::new(1000, emitter.buffer);
        vm.exec(false).unwrap();

        assert_eq!(vm.memory_view_64(), vec![1, 10, 3, 10]);
    }
}
