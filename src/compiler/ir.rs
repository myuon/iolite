use super::ast::{Span, Type};

#[derive(Debug, PartialEq, Clone)]
pub enum IrOp {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    ModInt,
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
    Cast(TypeTag),
    IntToFloat,
    FloatToInt,
    NegateInt,
    NegateFloat,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrTerm {
    Nil,
    Bool(bool),
    Int(i32),
    Float(f32),
    Ident(String),
    Qualified(String, String),
    DataPointer(String),
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
        size: usize,
        address: Box<IrTerm>,
    },
    Store {
        size: usize,
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
    DynamicCall {
        callee: Box<IrTerm>,
        args: Vec<IrTerm>,
    },
    StaticCall {
        callee: String,
        args: Vec<IrTerm>,
    },
    ExtCall {
        callee: String,
        args: Vec<IrTerm>,
    },
    Index {
        ptr: Box<IrTerm>,
        index: Box<IrTerm>,
    },
    SourceMap {
        span: Span,
    },
    Function(String),
    HeapPtrOffset,
    Discard(Box<IrTerm>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrDecl {
    Fun {
        name: String,
        args: Vec<String>,
        body: Box<IrTerm>,
        escaped: Vec<String>,
    },
    Let {
        name: String,
    },
    Declared(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IrModule {
    pub name: String,
    pub init_function: Option<String>,
    pub decls: Vec<IrDecl>,
    pub data_section: Vec<(String, usize, Vec<u8>)>,
    pub global_section: Vec<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IrProgram {
    pub modules: Vec<IrModule>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeTag {
    Pointer,
    Int,
    Float,
    Bool,
    Byte,
    None,
}

impl TypeTag {
    pub fn to_byte(&self) -> u8 {
        match self {
            TypeTag::Int => 0b0,
            TypeTag::Pointer => 0b1,
            TypeTag::Float => 0b10,
            TypeTag::Bool => 0b100,
            TypeTag::Byte => 0b1000,
            TypeTag::None => 0b0,
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

#[derive(PartialEq, Clone, PartialOrd)]
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

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "<nil>"),
            Value::Int(val) => write!(f, "{} (0x{:x})", val, val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Pointer(val) => {
                if *val == 0 {
                    write!(f, "<nil>")
                } else {
                    write!(f, "<0x{:x}>", val)
                }
            }
            Value::Byte(val) => write!(f, "Byte({})", val),
        }
    }
}
