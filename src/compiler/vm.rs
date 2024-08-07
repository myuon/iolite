use proptest::prelude::*;

use super::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionError {
    UnknownInstruction(u8),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    ModInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    IntToFloat,
    FloatToInt,
    Load,
    Load8,
    Load32,
    Store,
    Store8,
    Store32,
    LoadBp,
    StoreBp,
    LoadSp,
    StoreSp,
    Push(u64),
    Jump,
    JumpIf,
    Call,
    Return,
    Xor,
    And,
    Or,
    Not,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    Label(String),
    PushLabel(String),
    JumpTo(String),
    JumpIfTo(String),
    CallLabel(String),
    ExtCall(usize),
    Debug(String),
    Nop,
    Data {
        offset: u64,
        length: u64,
        data: Vec<u8>,
    },
    SourceMap(Span),
    PushHeapPtrOffset,
    PushGlobal(String),
    PushDataPointer(String),
    Abort,
}

impl Instruction {
    pub fn to_byte(&self) -> Vec<u8> {
        use Instruction::*;

        match self {
            // Control flow
            Push(_) => vec![0x01],
            Call => vec![0x02],
            ExtCall(_) => vec![0x03],
            Return => vec![0x04],
            Jump => vec![0x05],
            JumpIf => vec![0x06],
            Nop => vec![0x07],
            Data { .. } => vec![0x08],
            Abort => vec![0x09],

            // Arithmetic
            AddInt => vec![0x10],
            SubInt => vec![0x11],
            MulInt => vec![0x12],
            DivInt => vec![0x13],
            AddFloat => vec![0x14],
            SubFloat => vec![0x15],
            MulFloat => vec![0x16],
            DivFloat => vec![0x17],
            ModInt => vec![0x18],

            // Bitwise
            Xor => vec![0x20],
            And => vec![0x21],
            Or => vec![0x22],
            Not => vec![0x23],

            // Comparison
            Eq => vec![0x30],
            NotEq => vec![0x31],
            Lt => vec![0x32],
            Gt => vec![0x33],
            Le => vec![0x34],
            Ge => vec![0x35],

            // Memory
            Load => vec![0x40],
            LoadBp => vec![0x41, 0x01],
            LoadSp => vec![0x41, 0x02],
            Store => vec![0x42],
            StoreBp => vec![0x43, 0x01],
            StoreSp => vec![0x43, 0x02],
            Load8 => vec![0x44],
            Store8 => vec![0x45],
            Load32 => vec![0x46],
            Store32 => vec![0x47],

            // Conversion
            IntToFloat => vec![0x50],
            FloatToInt => vec![0x51],

            // Debug
            Debug(_) => vec![0x60],
            SourceMap(_) => vec![0x61],

            // Labels
            Label(_) => todo!(),
            PushLabel(_) => todo!(),
            JumpTo(_) => todo!(),
            JumpIfTo(_) => todo!(),
            CallLabel(_) => todo!(),
            PushHeapPtrOffset => todo!(),
            PushGlobal(_) => todo!(),
            PushDataPointer(_) => todo!(),
        }
    }

    pub fn from_byte(bytes: &[u8]) -> Result<Instruction, InstructionError> {
        Ok(match bytes[0] {
            0x01 => Instruction::Push(0),
            0x02 => Instruction::Call,
            0x03 => Instruction::ExtCall(0),
            0x04 => Instruction::Return,
            0x05 => Instruction::Jump,
            0x06 => Instruction::JumpIf,
            0x07 => Instruction::Nop,
            0x08 => Instruction::Data {
                offset: 0,
                length: 0,
                data: vec![],
            },
            0x09 => Instruction::Abort,

            0x10 => Instruction::AddInt,
            0x11 => Instruction::SubInt,
            0x12 => Instruction::MulInt,
            0x13 => Instruction::DivInt,
            0x14 => Instruction::AddFloat,
            0x15 => Instruction::SubFloat,
            0x16 => Instruction::MulFloat,
            0x17 => Instruction::DivFloat,
            0x18 => Instruction::ModInt,

            0x20 => Instruction::Xor,
            0x21 => Instruction::And,
            0x22 => Instruction::Or,
            0x23 => Instruction::Not,

            0x30 => Instruction::Eq,
            0x31 => Instruction::NotEq,
            0x32 => Instruction::Lt,
            0x33 => Instruction::Gt,
            0x34 => Instruction::Le,
            0x35 => Instruction::Ge,

            0x40 => Instruction::Load,
            0x41 => match bytes[1] {
                0x01 => Instruction::LoadBp,
                0x02 => Instruction::LoadSp,
                _ => todo!(),
            },
            0x42 => Instruction::Store,
            0x43 => match bytes[1] {
                0x01 => Instruction::StoreBp,
                0x02 => Instruction::StoreSp,
                _ => todo!(),
            },
            0x44 => Instruction::Load8,
            0x45 => Instruction::Store8,
            0x46 => Instruction::Load32,
            0x47 => Instruction::Store32,

            0x50 => Instruction::IntToFloat,
            0x51 => Instruction::FloatToInt,

            0x60 => Instruction::Debug("".to_string()),
            0x61 => Instruction::SourceMap(Span::unknown()),

            p => return Err(InstructionError::UnknownInstruction(p)),
        })
    }
}

impl Arbitrary for Instruction {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: ()) -> Self::Strategy {
        prop_oneof![
            Just(Instruction::AddInt),
            Just(Instruction::SubInt),
            Just(Instruction::MulInt),
            Just(Instruction::DivInt),
            Just(Instruction::AddFloat),
            Just(Instruction::SubFloat),
            Just(Instruction::MulFloat),
            Just(Instruction::DivFloat),
            Just(Instruction::Load),
            Just(Instruction::Store),
            Just(Instruction::LoadBp),
            Just(Instruction::StoreBp),
            Just(Instruction::LoadSp),
            Just(Instruction::StoreSp),
            Just(Instruction::Load8),
            Just(Instruction::Store8),
            Just(Instruction::Load32),
            Just(Instruction::Store32),
            Just(Instruction::Push(0)),
            Just(Instruction::Jump),
            Just(Instruction::JumpIf),
            Just(Instruction::Call),
            Just(Instruction::Return),
            Just(Instruction::Xor),
            Just(Instruction::And),
            Just(Instruction::Or),
            Just(Instruction::Not),
            Just(Instruction::Eq),
            Just(Instruction::NotEq),
            Just(Instruction::Lt),
            Just(Instruction::Gt),
            Just(Instruction::Le),
            Just(Instruction::Ge),
            Just(Instruction::IntToFloat),
            Just(Instruction::FloatToInt),
            Just(Instruction::Nop),
            Just(Instruction::Debug("".to_string())),
            Just(Instruction::SourceMap(Span::unknown())),
        ]
        .boxed()
    }
}

proptest! {
    #[test]
    fn test_instruction_roundtrip(instruction in any::<Instruction>()) {
        let bytes = instruction.to_byte();
        let new_instruction = Instruction::from_byte(&bytes).unwrap();

        prop_assert_eq!(instruction, new_instruction);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VmModule {
    pub(crate) name: String,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) data_section: Vec<(String, usize, Vec<u8>)>,
    pub(crate) global_section: Vec<String>,
    pub(crate) init_function_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VmProgram {
    pub(crate) modules: Vec<VmModule>,
}
