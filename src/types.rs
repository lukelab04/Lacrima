use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperatorTypes {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equality,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperatorTypes {
    Negate,
}

#[derive(Clone)]
#[allow(dead_code)]
pub enum Type {
    Atomic(AtomicType),
    Array(ArrayType),
    Structural,
    Algebraic(AlgebraicType),
    Functional(FunctionType),
    Inferred,
    External,
    EmptyType,
}

impl Type {
    pub fn get_type_as_str(&self) -> String {
        match self {
            Type::Atomic(t) => format!("{:?}", t),
            Type::Array(n) => {
                format!("[{}, {}]", n.elem_ty.get_type_as_str(), n.elements)
            },
            Type::Inferred => format!("Inferred"),
            Type::Functional(n) => {
                format!("{} -> {}", n.arg_type.get_type_as_str(), n.ret_type.get_type_as_str())
            }
            Type::External => format!("External"),
            Type::Structural => format!("Structural"),
            Type::Algebraic(n) => match n {
                AlgebraicType::Tuple(n) => {
                    let mut t_str = String::from("(");

                    for i in 0..(n.types.len() as i32 - 1) {
                        t_str.push_str(format!("{}, ", n.types[i as usize].as_ref().borrow().get_type_as_str()).as_str());
                    }
                    match n.types.last() {
                        Some(n) => t_str.push_str(format!("{}", n.as_ref().borrow().get_type_as_str()).as_str()),
                        _ => ()
                    }

                    t_str.push(')');

                    t_str
                }
            }
            Type::EmptyType => format!("Empty"),
        }
    }
    pub fn is_equal(&self, t2: &Type) -> bool {
        match self {
            Type::Atomic(n) => if let Type::Atomic(n2) = t2 {n == n2} else {false},
            Type::Array(n) => {
                if let Type::Array(n2) = t2 {
                    n2.elements == n.elements && n2.elem_ty.is_equal(&n.elem_ty)
                } else {false}
            }
            Type::External => false,
            Type::Structural => todo!(),
            Type::Algebraic(n) => {
                match n {
                    //Less complex than it looks; just checks if both objects are tuples, then compares the types inside the tuples
                    AlgebraicType::Tuple(n) => {
                        if let Type::Algebraic(AlgebraicType::Tuple(n2)) = t2 {
                            if n2.types.len() == n.types.len() {
                                for i in 0..n.types.len() {
                                    if !n.types[i].as_ref().borrow().is_equal(n2.types[i].as_ref().borrow().deref()) {return false;}
                                }
                                return true;
                            } else {false}
                        } else {false}
                    }
                }
            },
            Type::Functional(n) => {
                if let Type::Functional(n2) = t2 {
                    n.arg_type.is_equal(&n2.arg_type) && n.ret_type.is_equal(&n2.arg_type)
                } else {false}
            },
            Type::Inferred => panic!("Error in `Type::Inferred` case of `is_equal`. (This is a compiler bug, please report.)"),
            Type::EmptyType => if let Type::EmptyType = t2 {true} else {false},
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum AtomicType {
    Number,
    String,
    Boolean,
    Empty,
}

#[derive(Clone)]
pub enum AlgebraicType {
    Tuple(TupleType),
}


#[derive(Clone)]
pub struct FunctionType {
    //Tuple type
    pub arg_type: Box<Type>,
    pub ret_type: Box<Type>,
}

#[derive(Clone)]
pub struct ArrayType {
    pub elements: usize,
    pub elem_ty: Box<Type>,
}

#[derive(Clone)]
pub struct TupleType {
    pub types: Vec<Rc<RefCell<Type>>>,
}







