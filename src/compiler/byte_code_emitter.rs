use std::{collections::HashMap, io::Write, iter::repeat};

use anyhow::{bail, Result};
use thiserror::Error;

use crate::compiler::ast::Span;

use super::{ir::Value, vm::Instruction};

#[derive(Debug, Error)]
pub enum ByteCodeEmitterError {
    #[error("Label not found: {0}")]
    LabelNotFound(String),
    #[error("IO error: {0}")]
    IoError(std::io::Error),
}

pub struct ByteCodeEmitter {
    position: usize,
    pub(crate) buffer: Vec<u8>,
    pub(crate) labels: HashMap<String, usize>,
}

impl ByteCodeEmitter {
    pub fn new() -> Self {
        ByteCodeEmitter {
            position: 0,
            buffer: vec![],
            labels: HashMap::new(),
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
        self.write(&Instruction::Push(Value::Nil.as_u64()).to_byte())?;

        let position = self.position;
        self.write(&repeat(0xff).take(8).collect::<Vec<_>>())?;

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
                Debug(_) => {
                    self.write(&Instruction::Nop.to_byte())?;
                }
                SourceMap(span) => {
                    self.write(&Instruction::SourceMap(Span::unknown()).to_byte())?;
                    self.write(&(span.start.unwrap_or(0xFFFFFFFF) as u64).to_le_bytes())?;
                    self.write(&(span.end.unwrap_or(0xFFFFFFFF) as u64).to_le_bytes())?;
                }
                Data {
                    offset,
                    length,
                    data,
                } => {
                    self.write(
                        &Instruction::Data {
                            offset,
                            length,
                            data: vec![],
                        }
                        .to_byte(),
                    )?;
                    self.write(&offset.to_le_bytes())?;
                    self.write(&length.to_le_bytes())?;
                    self.write(&data)?;
                }
                Label(label) => {
                    self.labels.insert(label.clone(), self.position);
                    labels.insert(label, self.position);
                }
                PushLabel(label) => {
                    placeholders.insert(self.push_placeholder()?, label);
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
                ExtCall(label) => {
                    self.write(&Instruction::ExtCall(0).to_byte())?;
                    self.write(&label.to_le_bytes())?;
                }
                _ => {
                    self.write(&inst.to_byte())?;
                }
            }
        }

        for (position, label) in placeholders.clone() {
            let target = labels
                .get(&label)
                .ok_or(ByteCodeEmitterError::LabelNotFound(label))?;

            self.buffer[position..position + 8].copy_from_slice(&(*target as u64).to_le_bytes());
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
            Instruction::Push(Value::Int(1).as_u64()),
            Instruction::Push(Value::Int(2).as_u64()),
            Instruction::Label("end".to_string()),
            Instruction::Push(Value::Int(3).as_u64()),
            Instruction::JumpIfTo("start".to_string()),
        ];

        emitter.exec(code).unwrap();

        assert_eq!(
            vec![
                0x40, // load
                0x07, // nop
                // .start
                0x01, 0x1e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // push *end
                0x05, // jump
                0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // push 1
                0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // push 2
                // .end
                0x01, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // push 3
                0x01, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // push *start
                0x06, // jump_if
            ],
            emitter.buffer,
            "{:x?}",
            emitter.buffer
        );
    }
}

pub fn emit_disassemble(writer: &mut impl std::io::Write, binary: Vec<u8>) -> Result<()> {
    let mut position = 0;
    let consume = |position: &mut usize| -> u8 {
        let byte = binary[*position];
        *position += 1;

        byte
    };
    let consume_u64 = |position: &mut usize| -> u64 {
        u64::from_le_bytes([
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
            consume(position),
        ])
    };

    while position < binary.len() {
        write!(writer, "{:x}: {:02x} ", position, binary[position])?;

        match consume(&mut position) {
            0x01 => {
                let value = consume_u64(&mut position);
                write!(
                    writer,
                    "{} ",
                    binary[position - 8..position]
                        .iter()
                        .map(|t| format!("{:02x}", t))
                        .collect::<Vec<_>>()
                        .join(" "),
                )?;
                writeln!(writer, ";; PUSH {:?}", Value::from_u64(value))?;
            }
            0x02 => {
                writeln!(writer, ";; CALL")?;
            }
            0x03 => {
                let value = consume_u64(&mut position);
                writeln!(writer, ";; EXTCALL {}", value)?;
            }
            0x04 => {
                writeln!(writer, ";; RET")?;
            }
            0x05 => {
                writeln!(writer, ";; JUMP")?;
            }
            0x06 => {
                writeln!(writer, ";; JUMP_IF")?;
            }
            0x07 => {
                writeln!(writer, ";; NOP")?;
            }
            0x08 => {
                let offset = consume_u64(&mut position);
                let length = consume_u64(&mut position);
                let data = &binary[position..(position + length as usize)];
                write!(
                    writer,
                    "{:02x} {:02x} {} ",
                    offset,
                    length,
                    data.iter()
                        .map(|t| format!("{:02x}", t))
                        .collect::<Vec<_>>()
                        .join(" ")
                )?;

                write!(writer, ";; DATA {} {} [", offset, length)?;
                for i in 0..length {
                    if i > 0 {
                        write!(writer, " ")?;
                    }
                    let b = consume(&mut position);
                    write!(
                        writer,
                        "{}",
                        std::char::from_u32(b as u32)
                            .map(|t| if t.is_ascii_graphic() {
                                format!("{}", t)
                            } else {
                                format!("{:02x}", b)
                            })
                            .unwrap_or(format!("{:02x}", b))
                    )?;
                }
                writeln!(writer, "]")?;
            }
            0x09 => {
                writeln!(writer, ";; ABORT")?;
            }
            0x10 => {
                write!(writer, ";; ")?;
                writeln!(writer, "ADD")?;
            }
            0x11 => {
                write!(writer, ";; ")?;
                writeln!(writer, "SUB")?;
            }
            0x12 => {
                write!(writer, ";; ")?;
                writeln!(writer, "MUL")?;
            }
            0x13 => {
                write!(writer, ";; ")?;
                writeln!(writer, "DIV")?;
            }
            0x14 => {
                write!(writer, ";; ")?;
                writeln!(writer, "ADD_FLOAT")?;
            }
            0x15 => {
                write!(writer, ";; ")?;
                writeln!(writer, "SUB_FLOAT")?;
            }
            0x16 => {
                write!(writer, ";; ")?;
                writeln!(writer, "MUL_FLOAT")?;
            }
            0x17 => {
                write!(writer, ";; ")?;
                writeln!(writer, "DIV_FLOAT")?;
            }
            0x18 => {
                write!(writer, ";; ")?;
                writeln!(writer, "MOD_INT")?;
            }
            0x20 => {
                write!(writer, ";; ")?;
                writeln!(writer, "XOR")?;
            }
            0x21 => {
                write!(writer, ";; ")?;
                writeln!(writer, "AND")?;
            }
            0x22 => {
                write!(writer, ";; ")?;
                writeln!(writer, "OR")?;
            }
            0x23 => {
                write!(writer, ";; ")?;
                writeln!(writer, "NOT")?;
            }
            0x30 => {
                write!(writer, ";; ")?;
                writeln!(writer, "EQ")?;
            }
            0x31 => {
                write!(writer, ";; ")?;
                writeln!(writer, "NOT_EQ")?;
            }
            0x32 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LT")?;
            }
            0x33 => {
                write!(writer, ";; ")?;
                writeln!(writer, "GT")?;
            }
            0x34 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LE")?;
            }
            0x35 => {
                write!(writer, ";; ")?;
                writeln!(writer, "GE")?;
            }
            0x40 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LOAD")?;
            }
            0x41 => {
                write!(writer, ";; ")?;
                match consume(&mut position) {
                    0x01 => writeln!(writer, "LOAD_BP")?,
                    0x02 => writeln!(writer, "LOAD_SP")?,
                    _ => bail!("Invalid LOAD instruction"),
                }
            }
            0x42 => {
                write!(writer, ";; ")?;
                writeln!(writer, "STORE")?;
            }
            0x43 => {
                write!(writer, ";; ")?;
                match consume(&mut position) {
                    0x01 => writeln!(writer, "STORE_BP")?,
                    0x02 => writeln!(writer, "STORE_SP")?,
                    _ => bail!("Invalid STORE instruction"),
                }
            }
            0x44 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LOAD_8")?;
            }
            0x45 => {
                write!(writer, ";; ")?;
                writeln!(writer, "STORE_8")?;
            }
            0x46 => {
                write!(writer, ";; ")?;
                writeln!(writer, "LOAD_32")?;
            }
            0x47 => {
                write!(writer, ";; ")?;
                writeln!(writer, "STORE_32")?;
            }
            0x50 => {
                write!(writer, ";; ")?;
                writeln!(writer, "INT_TO_FLOAT")?;
            }
            0x51 => {
                write!(writer, ";; ")?;
                writeln!(writer, "FLOAT_TO_INT")?;
            }
            0x52 => {
                write!(writer, ";; ")?;
                writeln!(writer, "INT_TO_BYTE")?;
            }
            0x61 => {
                let start = consume_u64(&mut position);
                let end = consume_u64(&mut position);

                write!(writer, ";; ")?;
                writeln!(writer, "SOURCE_MAP {}:{}", start, end)?;
            }
            p => todo!("Unknown instruction: {:02x}", p),
        }
    }

    Ok(())
}
