use anyhow::Result;
use nanoid::nanoid;

use crate::compiler::ir::Value;

use super::ir::{IrDecl, IrModule, IrTerm};

#[derive(Clone)]
pub struct EscapeResolver {
    escaped: Vec<String>,
}

impl EscapeResolver {
    pub fn new() -> Self {
        Self { escaped: vec![] }
    }

    fn term(&mut self, term: &mut IrTerm) -> Result<()> {
        match term {
            IrTerm::Nil => {}
            IrTerm::Bool(_) => {}
            IrTerm::Int(_) => {}
            IrTerm::Float(_) => {}
            IrTerm::Ident(name) => {
                if self.escaped.contains(name) {
                    *term = IrTerm::Load {
                        size: Value::size() as usize,
                        address: Box::new(IrTerm::Ident(name.clone())),
                    };
                }
            }
            IrTerm::DataPointer(_) => {}
            IrTerm::Let { name, value } => {
                if self.escaped.contains(name) {
                    let ident_name = format!("escaped_{}", nanoid!());

                    let mut block = vec![];
                    block.push(IrTerm::Let {
                        name: ident_name.clone(),
                        value: Box::new(IrTerm::Call {
                            callee: Box::new(IrTerm::Ident("alloc".to_string())),
                            args: vec![IrTerm::Int(Value::size())],
                        }),
                    });
                    block.push(IrTerm::Store {
                        size: Value::size() as usize,
                        address: Box::new(IrTerm::Index {
                            ptr: Box::new(IrTerm::Load {
                                size: Value::size() as usize,
                                address: Box::new(IrTerm::Ident(ident_name.clone())),
                            }),
                            index: Box::new(IrTerm::Int(0)),
                        }),
                        value: value.clone(),
                    });
                    block.push(IrTerm::Load {
                        size: Value::size() as usize,
                        address: Box::new(IrTerm::Ident(ident_name)),
                    });

                    *value = Box::new(IrTerm::Items(block));

                    eprintln!("Escaped: {}", name);
                }

                self.term(value)?;
            }
            IrTerm::Op { op: _, args } => {
                for arg in args {
                    self.term(arg)?;
                }
            }
            IrTerm::Items(terms) => {
                for term in terms {
                    self.term(term)?;
                }
            }
            IrTerm::Return(term) => {
                self.term(term)?;
            }
            IrTerm::Load { size: _, address } => {
                self.term(address)?;
            }
            IrTerm::Store {
                size: _,
                address,
                value,
            } => {
                self.term(address)?;
                self.term(value)?;
            }
            IrTerm::While { cond, body } => {
                self.term(cond)?;
                self.term(body)?;
            }
            IrTerm::If { cond, then, else_ } => {
                self.term(cond)?;
                self.term(then)?;
                self.term(else_)?;
            }
            IrTerm::Call { callee, args } => {
                self.term(callee)?;
                for arg in args {
                    self.term(arg)?;
                }
            }
            IrTerm::Index { ptr, index } => {
                self.term(ptr)?;
                self.term(index)?;
            }
            IrTerm::SourceMap { span: _ } => {}
            IrTerm::Function(_) => {}
            IrTerm::HeapPtrOffset => {}
            IrTerm::Discard(term) => {
                self.term(term)?;
            }
        }

        Ok(())
    }

    pub fn decl(&mut self, decl: &mut IrDecl) -> Result<()> {
        match decl {
            IrDecl::Fun {
                name: _,
                args: _,
                body,
                escaped,
            } => {
                self.escaped = escaped.clone();
                self.term(body)?;
                self.escaped.clear();
            }
            IrDecl::Let { name: _ } => {}
        }

        Ok(())
    }

    pub fn module(&mut self, module: &mut IrModule) -> Result<()> {
        for decl in &mut module.decls {
            self.decl(decl)?;
        }

        Ok(())
    }
}
