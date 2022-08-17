use std::{rc::Rc, cell::RefCell};
use std::rc::Weak;

use crate::{lex::Token, symbol_table::*, types::*};

///Convert a value of type `&Ast` into a specific `Ast` variant.
///# Panics
/// Panics if the macro fails to match any `Ast` variant.
#[macro_export]
macro_rules! convert_node {
    ($a: expr, $b: ident) => {
        match $a {
            Ast::$b(x) => x,
            _ => panic!("Error in `convert_node` macro. (This is a compiler bug, please report.)"),
        }
    };
}

///Enum containing all possible node types for an `Ast` node.
///<br><br>
/// All variants currently defined here have a structural or enum types, although this is not a requirement.
/// Simple nodes with no children would be better represented by a variant with no type, for example.
#[allow(dead_code)]
pub enum Ast {
    Start(Start),

    VarDecl(VarDecl),

    Type(Type),

    IfStmt(IfStmt),
    WhileStmt(WhileStmt),

    Block(Block),

    Return(Return),

    UnaryOperator(UnaryOperator),
    BinaryOperator(BinaryOperator),
    Call(Call),
    Assign(Assign),

    Identifier(Identifier),

    Boolean(Boolean),
    Number(Number),
    String(Str),
    Function(Function),

    Print(Print),
}

impl Ast {
    ///Debugging function that matches the node type and prints relevant information.
    #[allow(dead_code)]
    pub fn print(&self, i: usize) {
        let s = "| ".repeat(i);

        match self {
            Ast::Start(n) => {
                for c in &n.children {c.as_ref().borrow().print(i + 1)}
            },
            Ast::VarDecl(n) => {
                println!("{}+- VarDec", s);
                n.iden.as_ref().borrow().print(i + 1);
                n.ty.as_ref().borrow_mut().print(i + 1);
                if let Some(n) = n.value.as_ref() { n.as_ref().borrow().print(i + 1) }
            },
            Ast::IfStmt(n) => {
                println!("{}+- IfStmt", s);
                n.condition.as_ref().borrow().print(i + 1);
                n.block.as_ref().borrow().print(i + 1);
                if let Some(n) = &n.follow {n.as_ref().borrow().print(i + 1)}
            },
            Ast::WhileStmt(n) => {
                println!("{}+- While", s);
                n.condition.as_ref().borrow().print(i + 1);
                n.block.as_ref().borrow().print(i + 1);
            },
            Ast::Block(n) => {
                println!("{}+- Block", s);
                for c in &n.children {c.as_ref().borrow().print(i + 1)}
                n.ty.as_ref().borrow().print(i + 1);
            },
            Ast::Identifier(n) => {
                println!("{}+- Identifier `{}`; Type {}", s, n.token.lexeme, self.get_node_type().get_type_as_str());
            },
            Ast::Boolean(n) => {
                println!("{}+- Boolean {}", s, n.val)
            }
            Ast::Print(n) => {
                println!("{}+- Print", s);
                n.node.borrow().print(i + 1);
            },

            Ast::Type(t) => {
                println!("{}+- {}", s, t.get_type_as_str())
            },
            Ast::UnaryOperator(n) => {
                println!("{}+- Operator `{}`", s, n.token.lexeme);
                n.operand.as_ref().borrow().print(i + 1);
            },
            Ast::BinaryOperator(n) => {
                println!("{}+- Operator `{}`", s, n.token.lexeme);
                n.ty.borrow().print(i + 1);
                n.left.borrow().print(i + 1);
                n.right.borrow().print(i + 1);
            },
            Ast::Assign(n) => {
                println!("{}+- Assign", s);
                n.left.borrow().print(i + 1);
                n.expr.borrow().print(i + 1);
            }
            Ast::Number(n) => println!("{}+- Number {}", s, n.val),
            Ast::String(n) => println!("{}+- String {}", s, n.val),
            Ast::Function(f) => {
                println!("{}+- Function", s);
                f.ty.as_ref().borrow_mut().print(i + 1);
                f.body.as_ref().borrow().print(i + 1);
            }
            Ast::Call(n) => {
                println!("{}+- Fn Call {}", s, n.ty.as_ref().borrow().get_node_type().get_type_as_str());
                for c in &n.args {c.as_ref().borrow().print(i + 1);}
                n.callee.as_ref().borrow().print(i + 1);
            }
            Ast::Return(n) => {
                println!("{}+- Return", s);
                n.elem.as_ref().borrow().print(i + 1);
            }
        }
    }

    ///Helper function to get the type of a node.
    /// <br><br>
    /// Most nodes have a `ty` field representing type, so this function is just a helpful way to extract the type from that field.
    #[allow(dead_code)]
    pub fn get_node_type(&self) -> Type {
        use Type::*;

        match self {
            Ast::Start(_) => EmptyType,
            Ast::VarDecl(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::Type(n) => n.clone(),
            Ast::IfStmt(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::WhileStmt(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::Block(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::UnaryOperator(n) =>n.ty.borrow().get_node_type(),
            Ast::BinaryOperator(n) => n.ty.borrow().get_node_type(),
            Ast::Assign(n) => n.ty.borrow().get_node_type(),
            Ast::Identifier(n) => n.symbol_table_node.as_ref().unwrap().borrow().get_type(),
            Ast::Boolean(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::Print(_) => EmptyType,
            Ast::Number(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::String(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::Function(f) => f.ty.as_ref().borrow().get_node_type(),
            Ast::Call(n) => n.ty.as_ref().borrow().get_node_type(),
            Ast::Return(_) => EmptyType,
        }
    }

    ///Helper function to retrieve the parent of a node.
    /// Returns a `Weak` pointer if there is a parent, otherwise `None`.
    pub fn get_parent(&self) -> Option<Weak<RefCell<Ast>>> {
        match self {
            Ast::Start(_n) => None,
            Ast::VarDecl(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Type(_n) => None,
            Ast::IfStmt(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::WhileStmt(n) =>if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Block(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Return(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::UnaryOperator(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::BinaryOperator(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Call(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Assign(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Identifier(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Boolean(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Number(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::String(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Function(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
            Ast::Print(n) => if let Some(p) = &n.parent {Some(p.clone())} else {None},
        }
    }
}

pub struct Start {
    #[allow(clippy::vec_box)]
    pub children: Vec<Rc<RefCell<Ast>>>,
}

pub struct Block {
    #[allow(clippy::vec_box)]
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub children: Vec<Rc<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
}

pub struct VarDecl {
    pub token: Token,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub iden: Rc<RefCell<Ast>>,
    pub ty: Rc<RefCell<Ast>>,
    pub value: Option<Rc<RefCell<Ast>>>,
}


pub struct Identifier {
    pub token: Token,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub symbol_table_node: Option<Rc<RefCell<SymbolTableEntry>>>,
}

pub struct Return {
    pub token: Token,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
    pub elem: Rc<RefCell<Ast>>,
    pub symbol_table_node: Option<Rc<RefCell<SymbolTableEntry>>>,
}

pub struct IfStmt {
    pub ty: Rc<RefCell<Ast>>,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub condition: Rc<RefCell<Ast>>,
    pub block: Rc<RefCell<Ast>>,
    pub follow: Option<Rc<RefCell<Ast>>>,
}

pub struct WhileStmt {
    pub ty: Rc<RefCell<Ast>>,
    pub condition: Rc<RefCell<Ast>>,
    pub block: Rc<RefCell<Ast>>,
    pub parent: Option<Weak<RefCell<Ast>>>,
}

pub struct Number {
    pub val: f64,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
}

pub struct Str {
    pub val: String,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
}

pub struct Boolean {
    pub val: bool,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
}

pub struct Function {
    pub token: Token,
    pub iden: Rc<RefCell<Ast>>,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
    //(Identifier, Type)
    pub args: Vec<(Rc<RefCell<Ast>>, Rc<RefCell<Ast>>)>,
    pub body: Rc<RefCell<Ast>>,
}

pub struct Call {
    pub token: Token,
    pub callee: Rc<RefCell<Ast>>,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub args: Vec<Rc<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
}

pub struct UnaryOperator {
    pub token: Token,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub op_type: UnaryOperatorTypes,
    pub ty: Rc<RefCell<Ast>>,
    pub operand: Rc<RefCell<Ast>>,
    pub operand_ty: Rc<RefCell<Ast>>,
}

pub struct BinaryOperator {
    pub token: Token,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub op_type: BinaryOperatorTypes,
    pub ty: Rc<RefCell<Ast>>,
    pub left: Rc<RefCell<Ast>>,
    pub right: Rc<RefCell<Ast>>,
    pub left_ty: Rc<RefCell<Ast>>,
    pub right_ty: Rc<RefCell<Ast>>,
}

pub struct Assign {
    pub token: Token,
    pub parent: Option<Weak<RefCell<Ast>>>,
    pub ty: Rc<RefCell<Ast>>,
    pub left: Rc<RefCell<Ast>>,
    pub expr: Rc<RefCell<Ast>>,
}

pub struct Print {
    pub ty: Rc<RefCell<Ast>>,
    pub node: Rc<RefCell<Ast>>,
    pub parent: Option<Weak<RefCell<Ast>>>,
}
