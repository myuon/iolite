use super::ast::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum IrOp {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    And,
    Or,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    IntToFloat,
    FloatToInt,
    IntToPointer,
    PointerToInt,
    NegateInt,
    NegateFloat,
    IntToByte,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrTerm {
    Nil,
    Bool(bool),
    Int(i32),
    Float(f32),
    Ident(String),
    Let {
        name: String,
        value: Box<IrTerm>,
    },
    Op {
        op: IrOp,
        args: Vec<IrTerm>,
    },
    Items(Vec<IrTerm>),
    Return(Box<IrTerm>),
    Load {
        address: Box<IrTerm>,
    },
    Store {
        address: Box<IrTerm>,
        value: Box<IrTerm>,
    },
    While {
        cond: Box<IrTerm>,
        body: Box<IrTerm>,
    },
    If {
        cond: Box<IrTerm>,
        then: Box<IrTerm>,
        else_: Box<IrTerm>,
    },
    Call {
        name: String,
        args: Vec<IrTerm>,
    },
    Index {
        ptr: Box<IrTerm>,
        index: Box<IrTerm>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrDecl {
    Fun {
        name: String,
        args: Vec<String>,
        body: Box<IrTerm>,
    },
    Let {
        name: String,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct IrModule {
    pub name: String,
    pub decls: Vec<IrDecl>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeTag {
    Pointer,
    Int,
    Float,
    Bool,
    Byte,
}

impl TypeTag {
    pub fn to_byte(&self) -> u8 {
        match self {
            TypeTag::Int => 0b0,
            TypeTag::Pointer => 0b1,
            TypeTag::Float => 0b10,
            TypeTag::Bool => 0b100,
            TypeTag::Byte => 0b1000,
        }
    }

    pub fn from_byte(byte: u8) -> Self {
        match byte {
            0b0 => TypeTag::Int,
            0b1 => TypeTag::Pointer,
            0b10 => TypeTag::Float,
            0b100 => TypeTag::Bool,
            0b1000 => TypeTag::Byte,
            _ => panic!("Invalid type tag: {}", byte),
        }
    }

    pub fn from_type(ty: &Type) -> Self {
        match ty {
            Type::Int => TypeTag::Int,
            Type::Ptr(_) => TypeTag::Pointer,
            Type::Nil => TypeTag::Pointer,
            Type::Float => TypeTag::Float,
            Type::Bool => TypeTag::Bool,
            Type::Byte => TypeTag::Byte,
            _ => panic!("Invalid type"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Int(i32),
    Float(f32),
    Bool(bool),
    Pointer(u32),
    Byte(u8),
}

impl Value {
    pub fn as_u64(&self) -> u64 {
        match self {
            Value::Nil => (TypeTag::Pointer.to_byte() as u64) << 32 | 0b0,
            Value::Int(val) => (TypeTag::Int.to_byte() as u64) << 32 | *val as u32 as u64,
            Value::Float(val) => {
                (TypeTag::Float.to_byte() as u64) << 32 | f32::to_bits(*val) as u64
            }
            Value::Bool(val) => (TypeTag::Bool.to_byte() as u64) << 32 | *val as u64,
            Value::Pointer(val) => (TypeTag::Pointer.to_byte() as u64) << 32 | *val as u64,
            Value::Byte(val) => (TypeTag::Byte.to_byte() as u64) << 32 | *val as u64,
        }
    }

    pub fn from_u64(val: u64) -> Self {
        let tag = (val >> 32) as u8;
        let value = val as u32;

        match TypeTag::from_byte(tag) {
            TypeTag::Pointer => Value::Pointer(value),
            TypeTag::Int => Value::Int(value as i32),
            TypeTag::Float => Value::Float(f32::from_bits(value)),
            TypeTag::Bool => Value::Bool(value != 0),
            TypeTag::Byte => Value::Byte(value as u8),
            _ => panic!("Invalid type tag"),
        }
    }

    pub fn size() -> i32 {
        8
    }
}
