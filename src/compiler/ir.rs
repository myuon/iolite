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
}

#[derive(Debug, PartialEq, Clone)]
pub enum IrTerm {
    Nil,
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
    Load(Box<IrTerm>),
    Store(Box<IrTerm>, Box<IrTerm>),
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

impl IrTerm {
    pub fn tagged_value(tag: TypeTag, term: IrTerm) -> Vec<IrTerm> {
        vec![IrTerm::Int(tag.to_byte() as i32), term]
    }

    pub fn tagged_int(data: i32) -> Vec<IrTerm> {
        IrTerm::tagged_value(TypeTag::Int, IrTerm::Int(data))
    }

    pub fn tagged_float(data: f32) -> Vec<IrTerm> {
        IrTerm::tagged_value(TypeTag::Float, IrTerm::Float(data))
    }

    pub fn tagged_pointer(data: i32) -> Vec<IrTerm> {
        IrTerm::tagged_value(TypeTag::Pointer, IrTerm::Int(data))
    }

    pub fn tagged_bool(data: bool) -> Vec<IrTerm> {
        IrTerm::tagged_value(TypeTag::Bool, IrTerm::Int(data as i32))
    }
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
            TypeTag::Pointer => 0b0,
            TypeTag::Int => 0b1,
            TypeTag::Float => 0b10,
            TypeTag::Bool => 0b100,
            TypeTag::Byte => 0b1000,
        }
    }

    pub fn from_byte(byte: u8) -> Self {
        match byte {
            0b0 => TypeTag::Pointer,
            0b1 => TypeTag::Int,
            0b10 => TypeTag::Float,
            0b100 => TypeTag::Bool,
            0b1000 => TypeTag::Byte,
            _ => panic!("Invalid type tag"),
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
}
