use std::cell::RefCell;
#[macro_export]
macro_rules! convert {
    ($x: expr, $y: ident) => {
        match $x {
            Value::$y(z) => z,
            _ => panic!("Error in `convert` macro. (This is a compiler bug, please report.)"),
        }
    };
}

#[derive(Clone)]
pub enum Value {
    Empty,
    Number(f64),
    Boolean(bool),
    String(Box<String>),
    Function(Box<Function>),
}

#[derive(Clone)]
pub struct Function {
    pub code: RefCell<Vec<u32>>,
}


impl Value {
    pub fn add(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn sub(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn div(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn mul(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn rem(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a % b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn and(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a && *b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn or(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a || *b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn eq(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a == *b),
            (Value::Number(a), Value::Number(b)) => Value::Boolean(*a == *b),
            (Value::String(a), Value::String(b)) => Value::Boolean(*a == *b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn neq(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(*a != *b),
            (Value::Number(a), Value::Number(b)) => Value::Boolean(*a != *b),
            (Value::String(a), Value::String(b)) => Value::Boolean(*a != *b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn gt(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a > b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn gte(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a >= b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn lt(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a < b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn lte(&self, rhs: &Value) -> Value {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Boolean(a <= b),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn not(&self) -> Value {
        match self {
            Value::Boolean(a) => Value::Boolean(!*a),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }

    pub fn neg(&self) -> Value {
        match self {
            Value::Number(a) => Value::Number(-*a),
            _ => panic!("Error implementing operator for Value. (This is a compiler bug, please report.)"),
        }
    }
}