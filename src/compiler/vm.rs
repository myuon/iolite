use proptest::prelude::*;

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
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    IntToFloat,
    FloatToInt,
    Load,
    Store,
    LoadBp,
    StoreBp,
    LoadSp,
    StoreSp,
    Push(u32),
    Pop,
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
    JumpTo(String),
    JumpIfTo(String),
    CallLabel(String),
    Debug(String),
}

impl Instruction {
    pub fn to_byte(&self) -> Vec<u8> {
        use Instruction::*;

        match self {
            // Control flow
            Push(_) => vec![0x01],
            Pop => vec![0x02],
            Call => vec![0x03],
            Return => vec![0x04],
            Jump => vec![0x05],
            JumpIf => vec![0x06],

            // Arithmetic
            AddInt => vec![0x10],
            SubInt => vec![0x11],
            MulInt => vec![0x12],
            DivInt => vec![0x13],
            AddFloat => vec![0x14],
            SubFloat => vec![0x15],
            MulFloat => vec![0x16],
            DivFloat => vec![0x17],

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

            // Conversion
            IntToFloat => vec![0x50],
            FloatToInt => vec![0x51],

            // Debug
            Debug(_) => todo!(),

            // Labels
            Label(_) => todo!(),
            JumpTo(_) => todo!(),
            JumpIfTo(_) => todo!(),
            CallLabel(_) => todo!(),
        }
    }

    pub fn from_byte(bytes: &[u8]) -> Result<Instruction, InstructionError> {
        Ok(match bytes[0] {
            0x01 => Instruction::Push(0),
            0x02 => Instruction::Pop,
            0x03 => Instruction::Call,
            0x04 => Instruction::Return,
            0x05 => Instruction::Jump,
            0x06 => Instruction::JumpIf,

            0x10 => Instruction::AddInt,
            0x11 => Instruction::SubInt,
            0x12 => Instruction::MulInt,
            0x13 => Instruction::DivInt,
            0x14 => Instruction::AddFloat,
            0x15 => Instruction::SubFloat,
            0x16 => Instruction::MulFloat,
            0x17 => Instruction::DivFloat,

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

            0x50 => Instruction::IntToFloat,
            0x51 => Instruction::FloatToInt,

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
            Just(Instruction::Push(0)),
            Just(Instruction::Pop),
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
