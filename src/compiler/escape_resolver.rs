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

    fn escape(&mut self, name: &str, term: &mut IrTerm) -> Result<()> {
        let ident_name = format!("escaped_{}", nanoid!());

        let mut block = vec![];
        block.push(IrTerm::Let {
            name: ident_name.clone(),
            value: Box::new(IrTerm::StaticCall {
                callee: "alloc".to_string(),
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
            value: Box::new(term.clone()),
        });
        block.push(IrTerm::Load {
            size: Value::size() as usize,
            address: Box::new(IrTerm::Ident(ident_name)),
        });

        *term = IrTerm::Items(block);

        eprintln!("Escaped: {}", name);

        Ok(())
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
            IrTerm::Qualified(_, _) => {}
            IrTerm::DataPointer(_) => {}
            IrTerm::Let { name, value } => {
                if self.escaped.contains(name) {
                    self.escape(name, &mut *value)?;
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
            IrTerm::DynamicCall { callee, args } => {
                self.term(callee)?;
                for arg in args {
                    self.term(arg)?;
                }
            }
            IrTerm::StaticCall { callee: _, args } => {
                for arg in args {
                    self.term(arg)?;
                }
            }
            IrTerm::ExtCall { callee: _, args } => {
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
                args,
                body,
                escaped,
            } => {
                self.escaped = escaped.clone();
                self.term(body)?;
                for arg in args {
                    if self.escaped.contains(arg) {
                        let arg_new = format!("{}_{}", arg, nanoid!());

                        let mut ident_arity = IrTerm::Load {
                            size: Value::size() as usize,
                            address: Box::new(IrTerm::Ident(arg_new.clone())),
                        };
                        self.escape(&arg, &mut ident_arity)?;

                        let mut new_body = vec![IrTerm::Let {
                            name: arg.clone(),
                            value: Box::new(ident_arity),
                        }];
                        new_body.push(*body.clone());

                        *arg = arg_new.clone();

                        *body = Box::new(IrTerm::Items(new_body));
                    }
                }
                self.escaped.clear();
            }
            IrDecl::Let { name: _ } => {}
            IrDecl::Declared(_) => {}
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
