use std::{
    collections::HashMap,
    io::{stderr, BufWriter, Write},
    sync::{Arc, Mutex},
};

use anyhow::Result;
use thiserror::Error;

use super::{
    ir::Value,
    vm::{Instruction, VmProgram},
};

#[derive(Error, Debug)]
pub enum LinkerError {
    #[error("Global variable not found: {0}")]
    GlobalVariableNotFound(String),
}

#[derive(Debug)]
pub struct Linker {
    pub(crate) trap_stderr: Option<Arc<Mutex<BufWriter<Vec<u8>>>>>,
    pub(crate) verbose: bool,
}

impl Linker {
    pub fn new(verbose: bool) -> Self {
        Linker {
            trap_stderr: None,
            verbose,
        }
    }

    pub fn trap_stdout(&mut self, trap_stderr: Arc<Mutex<BufWriter<Vec<u8>>>>) {
        self.trap_stderr = Some(trap_stderr);
    }

    pub fn link(&mut self, vm: VmProgram) -> Result<Vec<Instruction>, LinkerError> {
        let mut code = vec![];

        code.extend(vec![
            Instruction::Push(0),                       // 1 word for the return value
            Instruction::Push(Value::Int(-1).as_u64()), // return address
        ]);

        let mut data_offset = 0;
        let mut data_pointers = HashMap::new();
        for module in &vm.modules {
            for (id, offset, value) in module.data_section.clone() {
                let target = data_offset as u64 + offset as u64;
                code.push(Instruction::Data {
                    offset: target,
                    length: value.len() as u64,
                    data: value,
                });
                data_pointers.insert(id, target);
            }

            data_offset += module
                .data_section
                .iter()
                .map(|t| t.2.len() + Value::size() as usize)
                .sum::<usize>();
        }

        let mut global_offset = data_offset;
        let mut global_offsets = HashMap::new();
        for module in &vm.modules {
            for g in &module.global_section {
                global_offsets.insert(g.clone(), global_offset);
                global_offset += Value::size() as usize;
            }
        }

        let heap_ptr_offset = data_offset + global_offset;

        for module in &vm.modules {
            code.extend(vec![
                // allocate for the return value
                Instruction::Push(Value::Int(0).as_u64()),
                Instruction::CallLabel(module.init_function_name.clone()),
                // pop the return value
                Instruction::LoadSp,
                Instruction::Push(Value::Int(Value::size()).as_u64()),
                Instruction::AddInt,
                Instruction::StoreSp,
            ]);
        }

        code.push(Instruction::JumpTo("main".to_string()));

        for module in vm.modules {
            if self.verbose {
                let data = format!("Linking module: {}\n", module.name);
                match self.trap_stderr.clone() {
                    Some(buf) => buf.lock().unwrap().write(data.as_bytes()),
                    None => stderr().write(data.as_bytes()),
                }
                .unwrap();
            }

            for inst in module.instructions {
                match inst {
                    Instruction::PushHeapPtrOffset => {
                        code.push(Instruction::Debug("pushHeapPtrOffset".to_string()));
                        code.push(Instruction::Push(heap_ptr_offset as u64));
                    }
                    Instruction::PushGlobal(name) => {
                        let offset = global_offsets
                            .get(&name)
                            .ok_or(LinkerError::GlobalVariableNotFound(name))?;
                        code.push(Instruction::Push(*offset as u64));
                    }
                    Instruction::PushDataPointer(name) => {
                        let offset = data_pointers[&name];
                        code.push(Instruction::Push(offset as u64));
                    }
                    _ => {
                        code.push(inst);
                    }
                }
            }
        }

        Ok(code)
    }
}
