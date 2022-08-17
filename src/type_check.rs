use std::{rc::Rc, cell::RefCell};
use std::ops::DerefMut;
use crate::ast::*;
use crate::errors::*;
use crate::types::*;

pub struct TypeChecker {
    
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {  }
    }

    fn check_type_equality(&self, t1: Type, t2: Type) -> Result<(), String> {
        if t1.is_equal(&t2) {Ok(())} 
        else {Err(type_inequality(&t1.get_type_as_str(), &t2.get_type_as_str()))}

    }

    fn bin_op_ret_type(&self, opt: BinaryOperatorTypes) -> AtomicType {
        match opt {
            BinaryOperatorTypes::Add => AtomicType::Number,
            BinaryOperatorTypes::Sub => AtomicType::Number,
            BinaryOperatorTypes::Mul => AtomicType::Number,
            BinaryOperatorTypes::Div => AtomicType::Number,
            BinaryOperatorTypes::Mod => AtomicType::Number,
            BinaryOperatorTypes::Equality => AtomicType::Boolean,
            BinaryOperatorTypes::NotEqual => AtomicType::Boolean,
            BinaryOperatorTypes::Less => AtomicType::Boolean,
            BinaryOperatorTypes::Greater => AtomicType::Boolean,
            BinaryOperatorTypes::LessEqual => AtomicType::Boolean,
            BinaryOperatorTypes::GreaterEqual => AtomicType::Boolean,
            BinaryOperatorTypes::And => AtomicType::Boolean,
            BinaryOperatorTypes::Or => AtomicType::Boolean,
        }
    }

    fn bin_op_defined(&self, t1: &Type, op: BinaryOperatorTypes, t2: &Type, lex: &str) -> Result<(), String> {
        use BinaryOperatorTypes::*;
        if let (Type::Atomic(t1), Type::Atomic(t2)) = (t1, t2) {
            match (t1, t2) {
                (AtomicType::Number, AtomicType::Number) => {
                    match op {
                        Add | Sub | Mul | Div | Mod | Equality | NotEqual | Less | Greater | LessEqual | GreaterEqual => Ok(()),
                        _ => Err(binary_type_error(&lex.to_string(), &Type::Atomic(t1.clone()).get_type_as_str(), &Type::Atomic(t2.clone()).get_type_as_str()))
                    }
                },
                (AtomicType::String, AtomicType::String) => {
                    match op {
                        Equality | NotEqual => Ok(()),
                        _ => Err(binary_type_error(&lex.to_string(), &Type::Atomic(t1.clone()).get_type_as_str(), &Type::Atomic(t2.clone()).get_type_as_str()))
                    }
                },
                (AtomicType::Boolean, AtomicType::Boolean) => match op {
                    Equality | NotEqual | And | Or => Ok(()),
                    _ => Err(binary_type_error(&lex.to_string(), &Type::Atomic(t1.clone()).get_type_as_str(), &Type::Atomic(t2.clone()).get_type_as_str())),
                }
                _ => Err(binary_type_error(&lex.to_string(), &Type::Atomic(t1.clone()).get_type_as_str(), &Type::Atomic(t2.clone()).get_type_as_str()))
            }
        } else {Err(basic_error("Operators only defined on atomic types"))}
    }

    fn un_op_defined(&self, t1: &Type, op: UnaryOperatorTypes, lex: &str) -> Result<(), String> {
        if let Type::Atomic(t1) = t1 {
            match t1 {
                AtomicType::Number => {
                    match op {
                        _negate => Ok(())
                    }
                },
                _ => Err(unary_type_error(&lex.to_string(), &Type::Atomic(t1.clone()).get_type_as_str()))
            }
        } else {Err(basic_error("Operators only defined on atomic types"))}
    }

    pub fn check_types(&mut self, ast: &mut Ast) -> Result<(), String> {
        self.type_check(ast)?;
        Ok(())
    }

    fn type_check(&mut self, ast: &mut Ast) -> Result<Type, String> {
        match ast {
            Ast::Start(n) => {
                for c in &mut n.children {self.type_check(c.borrow_mut().deref_mut())?;}
                Ok(Type::EmptyType)
            },
            Ast::VarDecl(n) => {
                if let Some(v) = n.value.as_mut() {

                    let mut vtype = self.type_check(n.ty.borrow_mut().deref_mut())?;

                    if let Type::Inferred = vtype {
                        let exprtype = self.type_check(v.borrow_mut().deref_mut())?;
                        n.ty = Rc::new(RefCell::new(Ast::Type(self.infer_type(&exprtype))));
                        vtype = self.type_check(n.ty.borrow_mut().deref_mut())?;
                    }

                    let exprtype = self.type_check(v.borrow_mut().deref_mut())?;

                    match self.check_type_equality(vtype.clone(), exprtype.clone()) {
                        Ok(_) => Ok(vtype.clone()),
                        Err(e) => Err(e),
                    }

                } else {
                    self.type_check(n.ty.borrow_mut().deref_mut())
                }
            },
            Ast::Function(n) => {
                self.type_check(n.ty.borrow_mut().deref_mut())?;
                self.type_check(n.body.borrow_mut().deref_mut())
            }
            Ast::Type(t) => {
                Ok(t.clone())
            },
            Ast::IfStmt(n) => {
                let ctype  = self.type_check(n.condition.borrow_mut().deref_mut())?;
                self.check_type_equality(ctype, Type::Atomic(AtomicType::Boolean))?;
                let t1 = self.type_check(n.block.borrow_mut().deref_mut())?;

                if let Some(f) = n.follow.as_mut() {
                    let t2 = self.type_check(f.borrow_mut().deref_mut())?;
                    self.check_type_equality(t1.clone(), t2)?;
                }

                n.ty = Rc::new(RefCell::new(Ast::Type(t1.clone())));

                Ok(t1)
            },
            Ast::WhileStmt(n) => {
                let ctype = self.type_check(n.condition.as_ref().borrow_mut().deref_mut())?;
                self.check_type_equality(ctype, Type::Atomic(AtomicType::Boolean))?;
                let t1 = self.type_check(n.block.as_ref().borrow_mut().deref_mut())?;
                n.ty = Rc::new(RefCell::new(Ast::Type(t1.clone())));
                Ok(t1)
            },
            Ast::Block(n) => {
                for c in &mut n.children {self.type_check(c.borrow_mut().deref_mut())?;}
                self.type_check(n.ty.borrow_mut().deref_mut())
            },
            Ast::UnaryOperator(n) => {
                let t = self.type_check(n.operand.as_ref().borrow_mut().deref_mut())?;
                n.ty = Rc::new(RefCell::new(Ast::Type(t.clone())));
                n.operand_ty = Rc::new(RefCell::new(Ast::Type(t.clone())));

                match self.un_op_defined(&t, n.op_type, &n.token.lexeme) {
                    Ok(()) => Ok(t.clone()),
                    Err(e) => Err(e),
                }
            },
            Ast::BinaryOperator(n) => {
                let t1 = self.type_check(n.left.as_ref().borrow_mut().deref_mut())?;
                let t2 = self.type_check(n.right.as_ref().borrow_mut().deref_mut())?;

                self.check_type_equality(t1.clone(), t2.clone())?;
                match self.bin_op_defined(&t1, n.op_type, &t2, &n.token.lexeme) {
                    Ok(()) => Ok(Type::Atomic(self.bin_op_ret_type(n.op_type))),
                    Err(e) => Err(e),
                }
            },
            Ast::Identifier(n) => {
                Ok(n.symbol_table_node.as_ref().unwrap().borrow().get_type())
            },
            Ast::Boolean(s) => {
                self.type_check(s.ty.as_ref().borrow_mut().deref_mut())
            }
            Ast::Print(n) => {
                n.ty = Rc::new(RefCell::new(Ast::Type(self.type_check(n.node.as_ref().borrow_mut().deref_mut())?)));
                Ok(Type::EmptyType)
            },
            Ast::Assign(n) => {
                let t1 = self.type_check(n.left.as_ref().borrow_mut().deref_mut())?;
                let t2 = self.type_check(n.expr.as_ref().borrow_mut().deref_mut())?;

                match self.check_type_equality(t1.clone(), t2.clone()) {
                    Ok(()) => Ok(t1.clone()),
                    Err(e) => Err(e),
                }
            },
            Ast::Call(n) => {
                let ty;
                if let Type::Functional(f) = n.callee.as_ref().borrow().get_node_type() {
                    ty = f.clone();
                    n.ty = Rc::new(RefCell::new(Ast::Type(f.ret_type.as_ref().clone())));
                } else {
                    return Err(basic_error(format!("Cannot call non-function type {}", n.ty.as_ref().borrow().get_node_type().get_type_as_str()).as_str()));
                }

                match ty.arg_type.as_ref() {
                    Type::Algebraic(AlgebraicType::Tuple(t)) => {
                        if n.args.len() != t.types.len() {return Err(incorrect_arg_num(t.types.len(), n.args.len(), &n.token));}
                    }
                    _ => panic!("Error in `Ast::Call` case of `type_check`. (This is a compiler bug, please report.)"),
                }

                Ok(n.ty.as_ref().borrow().get_node_type())
            }
            Ast::Number(n) => self.type_check(n.ty.as_ref().borrow_mut().deref_mut()),
            Ast::String(n) => self.type_check(n.ty.as_ref().borrow_mut().deref_mut()),
            Ast::Return(n) => {
                n.ty = Rc::new(RefCell::new(Ast::Type(self.type_check(n.elem.borrow_mut().deref_mut())?)));
                let ty = self.type_check(n.ty.borrow_mut().deref_mut())?;
                if let Type::Functional(ftype) = n.symbol_table_node.as_ref().unwrap().borrow().get_type() {
                    self.check_type_equality(*ftype.ret_type, ty)?;
                } else {panic!("Error in `Ast::Return` case of `type_check`. (This is a compiler bug, please report.)")}
                Ok(Type::EmptyType)
            }
        }
    }

    fn infer_type(&self, t2: &Type) -> Type {
        if let Type::Inferred = t2 {panic!("Error in `infer_type`. (This is a compiler bug, please report.)")}
        t2.clone()
    }

}