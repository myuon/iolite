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

        let mut module_offset = 0;
        for module in &vm.modules {
            for (_id, offset, value) in module.data_section.clone() {
                code.push(Instruction::Data {
                    offset: module_offset as u64 + offset as u64,
                    length: value.len() as u64,
                    data: value,
                });
            }

            module_offset += module
                .data_section
                .iter()
                .map(|t| t.2.len() + 8)
                .sum::<usize>();
        }

        code.extend(vec![Instruction::JumpTo("main".to_string())]);

        for module in vm.modules {
            println!("linking module: {}", module.name);
            code.extend(module.instructions);

            if module.name == "main" {
                code.push(Instruction::JumpTo("exit".to_string()));
            }
        }

        code.extend(vec![Instruction::Label("exit".to_string())]);

        Ok(code)
    }
}
