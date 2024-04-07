use std::{collections::HashMap, io::Write};

use super::vm::Instruction;

#[derive(Debug)]
pub enum ByteCodeEmitterError {
    LabelNotFound(String),
    IoError(std::io::Error),
}

pub struct ByteCodeEmitter {
    position: usize,
    buffer: Vec<u8>,
}

impl ByteCodeEmitter {
    pub fn new() -> Self {
        ByteCodeEmitter {
            position: 0,
            buffer: vec![],
        }
    }

    fn write(&mut self, bytes: &[u8]) -> Result<(), ByteCodeEmitterError> {
        self.buffer
            .write_all(bytes)
            .map_err(|err| ByteCodeEmitterError::IoError(err))?;

        self.position += bytes.len();

        Ok(())
    }

    fn push_placeholder(&mut self) -> Result<usize, ByteCodeEmitterError> {
        self.write(&Instruction::Push(0).to_byte())?;

        let position = self.position;
        self.write(&[0xff, 0xff, 0xff, 0xff])?;

        Ok(position)
    }

    pub fn exec(&mut self, code: Vec<Instruction>) -> Result<(), ByteCodeEmitterError> {
        let mut labels = HashMap::new();
        let mut placeholders = HashMap::new();

        for inst in code {
            use Instruction::*;

            match inst {
                Push(val) => {
                    self.write(&Instruction::Push(0).to_byte())?;
                    self.write(&val.to_le_bytes())?;
                }
                Debug(_) => {}
                Label(label) => {
                    labels.insert(label, self.position);
                }
                JumpTo(label) => {
                    placeholders.insert(self.push_placeholder()?, label);
                    self.write(&Instruction::Jump.to_byte())?;
                }
                JumpIfTo(label) => {
                    placeholders.insert(self.push_placeholder()?, label);
                    self.write(&Instruction::JumpIf.to_byte())?;
                }
                CallLabel(label) => {
                    placeholders.insert(self.push_placeholder()?, label);
                    self.write(&Instruction::Call.to_byte())?;
                }
                _ => {
                    self.write(&inst.to_byte())?;
                }
            }
        }

        for (position, label) in placeholders {
            let target = labels
                .get(&label)
                .ok_or(ByteCodeEmitterError::LabelNotFound(label))?;

            self.buffer[position..position + 4].copy_from_slice(&(*target as u32).to_le_bytes());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_labels() {
        let mut emitter = ByteCodeEmitter::new();
        let code = vec![
            Instruction::Load,
            Instruction::Debug("debug information".to_string()),
            Instruction::Label("start".to_string()),
            Instruction::JumpTo("end".to_string()),
            Instruction::Push(1),
            Instruction::Push(2),
            Instruction::Label("end".to_string()),
            Instruction::Push(3),
            Instruction::JumpIfTo("start".to_string()),
        ];

        emitter.exec(code).unwrap();

        assert_eq!(
            vec![
                0x40, // load
                0x01, 0x11, 0x00, 0x00, 0x00, // push *end
                0x05, // jump
                0x01, 0x01, 0x00, 0x00, 0x00, // push 1
                0x01, 0x02, 0x00, 0x00, 0x00, // push 2
                0x01, 0x03, 0x00, 0x00, 0x00, // push 3
                0x01, 0x01, 0x00, 0x00, 0x00, // push *start
                0x06, // jump_if
            ],
            emitter.buffer,
        );
    }
}
