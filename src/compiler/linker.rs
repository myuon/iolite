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
            Instruction::JumpTo("main".to_string()),
        ]);

        for module in vm.modules {
            println!("linking module: {}", module.name);
            code.extend(module.instructions);

            if module.name == "main" {
                code.extend(vec![Instruction::JumpTo("exit".to_string())]);
            }
        }

        code.extend(vec![Instruction::Label("exit".to_string())]);

        Ok(code)
    }
}
