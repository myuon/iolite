use std::collections::HashMap;

use anyhow::Result;
use thiserror::Error;

use super::{
    ir::Value,
    vm::{Instruction, VmProgram},
};

#[derive(Error, Debug)]
pub enum LinkerError {}

#[derive(Debug)]
pub struct Linker {}

impl Linker {
    pub fn new() -> Self {
        Linker {}
    }

    pub fn link(&mut self, vm: VmProgram) -> Result<Vec<Instruction>, LinkerError> {
        let mut code = vec![];

        code.extend(vec![
            Instruction::Push(0), // 1 word for the return value
            Instruction::Push(Value::Pointer(0xffffffff).as_u64()), // return address
        ]);

        let mut data_offset = 0;
        for module in &vm.modules {
            for (_id, offset, value) in module.data_section.clone() {
                code.push(Instruction::Data {
                    offset: data_offset as u64 + offset as u64,
                    length: value.len() as u64,
                    data: value,
                });
            }

            data_offset += module
                .data_section
                .iter()
                .map(|t| t.2.len() + 8)
                .sum::<usize>();
        }

        let mut global_offset = data_offset;
        let mut global_offsets = HashMap::new();
        for module in &vm.modules {
            global_offsets.insert(module.name.clone(), global_offset);

            global_offset += module.global_section.len() * Value::size() as usize;
        }

        let heap_ptr_offset = data_offset + global_offset;

        for module in &vm.modules {
            code.push(Instruction::CallLabel(module.init_function_name.clone()));
        }

        code.extend(vec![Instruction::JumpTo("main".to_string())]);

        for module in vm.modules {
            println!("linking module: {}", module.name);
            for inst in module.instructions {
                match inst {
                    Instruction::PushHeapPtrOffset => {
                        code.push(Instruction::Debug("pushHeapPtrOffset".to_string()));
                        code.push(Instruction::Push(heap_ptr_offset as u64));
                    }
                    Instruction::PushGlobal(g) => {
                        let offset = global_offsets[&module.name] + g * Value::size() as usize;
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
