use std::{collections::HashMap, rc::Rc, cell::RefCell};
use std::ops::{DerefMut, Deref};
use std::rc::Weak;

use crate::{ast::*, lex::*, errors::*, types::*, convert_node, ExternalFunctions};

#[allow(dead_code)]
pub struct SymbolTable {
    head_node: Rc<RefCell<SymbolTableNode>>,
    curr_node: Rc<RefCell<SymbolTableNode>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let head = Rc::new(RefCell::new(SymbolTableNode::new()));
        let cl = Rc::clone(&head);
        SymbolTable { head_node: head, curr_node: cl }
    }

    fn push_scope(&mut self) {
        let newscope = Rc::new(RefCell::new(SymbolTableNode::new()));
        let cl = Rc::clone(&newscope);
        cl.borrow_mut().parent = Some(self.curr_node.clone());
        self.curr_node.borrow_mut().child_scopes.push(newscope);
        self.curr_node = cl;
    }

    #[allow(dead_code)]
    fn pop_scope(&mut self) {
        let cl = self.curr_node.borrow().parent.clone();
        if let Some(parent) = cl {
            self.curr_node = parent;
        } else {panic!("Error in `pop_scope` function. (This is a compiler bug, please report.)")}
    }

    fn add_entry(&mut self, name: &str, entry: Rc<RefCell<SymbolTableEntry>>) -> Result<Rc<RefCell<SymbolTableEntry>>, String> {
        if self.curr_node.borrow().entries.contains_key(&name.to_string()) {
            return Err(name_redec_error (
                &self.curr_node.borrow().entries[&name.to_string()].borrow().get_token(),
                &entry.borrow().get_token(),
            ));
        }

        self.curr_node.borrow_mut().entries.insert(name.to_string(), entry);

        Ok(self.curr_node.borrow_mut().entries[&name.to_string()].clone())
    }

    pub fn get_entry_from_node(node: &mut Rc<RefCell<SymbolTableNode>>, name: &str) -> Option<Rc<RefCell<SymbolTableEntry>>> {
        if node.borrow().entries.contains_key(&name.to_string()) {
            return Some(node.borrow().entries[&name.to_string()].clone());
        }

        if let Some(parent) = node.borrow_mut().parent.as_mut() {
            return SymbolTable::get_entry_from_node(parent, name);
        }

        None
    }

    pub fn build_table(&mut self, mut ast: Ast, externs: &ExternalFunctions) -> Result<Ast, String> {
        ast = self.link_child_nodes(ast);
        let rc = Rc::new(RefCell::new(ast));

        let mut backtrack_nodes = vec![];
        self.build_symbol_table(&rc, &mut backtrack_nodes, externs)?;

        for n in &backtrack_nodes {self.validate_backtrack_nodes(n);}

        match Rc::try_unwrap(rc) {
            Ok(val) => Ok(val.into_inner()),
            _ => panic!("Error in `build_table` method. (This is a compiler bug, please report.)"),
        }
    }

    fn build_symbol_table(&mut self, ast: &Rc<RefCell<Ast>>, backtrack_nodes: &mut Vec<Rc<RefCell<Ast>>>, externs: &ExternalFunctions) -> Result<(), String>{

        match ast.as_ref().borrow_mut().deref_mut() {
            Ast::Start(n) => {
                for c in &mut n.children { self.build_symbol_table(c, backtrack_nodes, externs)?;}
            },
            Ast::IfStmt(n) => {
                self.build_symbol_table(&n.condition, backtrack_nodes, externs)?;
                self.build_symbol_table(&n.block, backtrack_nodes, externs)?;
                if let Some(f) = n.follow.as_mut() {self.build_symbol_table(&f, backtrack_nodes, externs)?}
            },
            Ast::WhileStmt(n) => {
                self.build_symbol_table(&n.condition, backtrack_nodes, externs)?;
                self.build_symbol_table(&n.block, backtrack_nodes, externs)?;
            },
            Ast::Block(n) => {
                self.push_scope();
                self.build_symbol_table(&n.ty, backtrack_nodes, externs)?;
                for c in &mut n.children{self.build_symbol_table(&c, backtrack_nodes, externs)?}
                self.pop_scope();
            },
            Ast::Identifier(n) => {
                match SymbolTable::get_entry_from_node(&mut self.curr_node, n.token.lexeme.as_str()) {
                    Some(entry) => {n.symbol_table_node = Some(entry)},
                    None => return Err(undeclared_name(&n.token.clone())),
                }
            },
            Ast::Boolean(_n) => {},
            Ast::VarDecl(t) => {
                let entry = SymbolTableEntry::new_ident(&convert_node!(t.iden.as_ref().borrow().deref(), Identifier).token, t.ty.borrow_mut().deref_mut());
                self.add_entry(entry.get_token().lexeme.as_str(), Rc::new(RefCell::new(entry)))?;
                self.build_symbol_table(&t.ty, backtrack_nodes, externs)?;
                if let Some(v) = &mut t.value {self.build_symbol_table(&v, backtrack_nodes, externs)?;}
            },
            Ast::Function(n) => {
                let entry = SymbolTableEntry::new_ident(&convert_node!(n.iden.as_ref().borrow().deref(), Identifier).token, n.ty.as_ref().borrow_mut().deref_mut());
                let _entry = self.add_entry(entry.get_token().lexeme.as_str(), Rc::new(RefCell::new(entry)))?;

                if let Ast::Identifier(id) = n.iden.as_ref().borrow_mut().deref_mut() {
                    id.symbol_table_node = Some(SymbolTable::get_entry_from_node(&mut self.curr_node, &id.token.lexeme).unwrap());
                } else {panic!("Error in `Ast::Function` case of code generation. (This is compiler bug, please report.)")}

                self.push_scope();
                for elem in &mut n.args {
                    match (elem.0.as_ref().borrow().deref(), elem.1.as_ref().borrow().deref()) {
                        (Ast::Identifier(i), Ast::Type(_t)) => {
                            let iden_entry = SymbolTableEntry::new_ident(&i.token, elem.1.as_ref().borrow().deref());
                            self.add_entry(iden_entry.get_token().lexeme.as_str(), Rc::new(RefCell::new(iden_entry)))?;
                        },
                        _ => panic!("Error in `Ast::Function` case of code generation. (This is a compiler bug, please report.)"),
                    }
                }
                
                self.build_symbol_table(&n.ty, backtrack_nodes, externs)?;
                self.build_symbol_table(&n.body, backtrack_nodes, externs)?;
                self.pop_scope();
            }
            Ast::Extern(n) => {
                let tmp_stmt = n.stmt.as_ref().borrow();
                let ident = &convert_node!(tmp_stmt.deref(), Identifier).token;

                if !externs.functions.contains_key(&ident.lexeme) {
                    return Err(basic_error(&format!("Extern function {} has not been registered.", ident.lexeme)));
                }

                let entry = SymbolTableEntry::new_ident(&ident, &Ast::Type(Type::External));

                drop(tmp_stmt);

                let _entry = self.add_entry(entry.get_token().lexeme.as_str(), Rc::new(RefCell::new(entry)))?;
                if let Ast::Identifier(id) = n.stmt.as_ref().borrow_mut().deref_mut() {
                    id.symbol_table_node = Some(_entry);
                }
            }
            Ast::Return(n) => {
                self.build_symbol_table(&n.elem, backtrack_nodes, externs)?;
                backtrack_nodes.push(ast.clone());
            }
            Ast::Type(t) => {
                match t {
                    Type::Atomic(_) => (),
                    Type::Array(n) => {
                        self.build_symbol_table(&Rc::new(RefCell::new(Ast::Type(n.elem_ty.as_ref().clone()))), backtrack_nodes, externs)?
                    },
                    Type::Structural => todo!(),
                    Type::External => (),
                    Type::Algebraic(n) => {
                        match n {
                            AlgebraicType::Tuple(t) => {
                                for c in &mut t.types {self.build_symbol_table(&Rc::new(RefCell::new(Ast::Type(c.as_ref().borrow().clone()))), backtrack_nodes, externs)?;}
                            }
                        }
                    },
                    Type::Functional(n) => {
                        let arg_type = n.arg_type.as_ref().clone();
                        let ret_type = n.ret_type.as_ref().clone();
                        self.build_symbol_table(&Rc::new(RefCell::new(Ast::Type(arg_type))), backtrack_nodes, externs)?;
                        self.build_symbol_table(&Rc::new(RefCell::new(Ast::Type(ret_type))), backtrack_nodes, externs)?;
                    },
                    Type::Inferred => (),
                    Type::EmptyType => (),
                }
            },
            Ast::UnaryOperator(t) => {
                self.build_symbol_table(&t.operand, backtrack_nodes, externs)?;
            },
            Ast::BinaryOperator(t) => {
                self.build_symbol_table(&t.left, backtrack_nodes, externs)?;
                self.build_symbol_table(&t.right, backtrack_nodes, externs)?;
            },
            Ast::Assign(n) => {
                self.build_symbol_table(&n.left, backtrack_nodes, externs)?;
                self.build_symbol_table(&n.expr, backtrack_nodes, externs)?;
            }
            Ast::Call(n) => {
                self.build_symbol_table(&n.callee, backtrack_nodes, externs)?;
                self.build_symbol_table(&n.ty, backtrack_nodes, externs)?;
                for c in &mut n.args {self.build_symbol_table(c, backtrack_nodes, externs)?}
            }
            Ast::Number(_) => (),
            Ast::String(_) => (),
        }

        Ok(())
    }

    //Link children nodes back to parent nodes
    pub fn link_child_nodes(&self, ast: Ast) -> Ast {
       let rc = Rc::new(RefCell::new(ast));

        self.link_ch_nodes_rec(rc.clone(), None);

        match Rc::try_unwrap(rc) {
            Ok(v) => v.into_inner(),
            _ => panic!("Error in `link_child_nodes` function. (This is a compiler bug, please report.)"),
        }
    }

    fn link_ch_nodes_rec(&self, ast: Rc<RefCell<Ast>>, p_weak: Option<Weak<RefCell<Ast>>>) {

        let new_parent = Some(Rc::downgrade(&ast));

        match ast.as_ref().borrow_mut().deref_mut() {
            Ast::Start(n) => {
                for ch in &n.children {
                    self.link_ch_nodes_rec(ch.clone(), new_parent.clone());
                }
            }
            Ast::Extern(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.stmt.clone(), new_parent.clone());
            }
            Ast::VarDecl(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.iden.clone(), new_parent.clone());
                self.link_ch_nodes_rec(n.ty.clone(), new_parent.clone());
                if let Some(val) = &n.value {
                    self.link_ch_nodes_rec(val.clone(), new_parent.clone());
                }
            }
            Ast::Type(_n) => {}
            Ast::IfStmt(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.condition.clone(), new_parent.clone());
                self.link_ch_nodes_rec(n.block.clone(), new_parent.clone());
                if let Some(f) = &n.follow {
                    self.link_ch_nodes_rec(f.clone(), new_parent.clone());
                }
            }
            Ast::WhileStmt(n) => {
               n.parent = p_weak;
               self.link_ch_nodes_rec(n.condition.clone(), new_parent.clone());
               self.link_ch_nodes_rec(n.block.clone(), new_parent.clone());
            }
            Ast::Block(n) => {
                n.parent = p_weak;
                for child in &n.children {
                    self.link_ch_nodes_rec(child.clone(), new_parent.clone());
                }
            }
            Ast::Return(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.elem.clone(), new_parent.clone());
            }
            Ast::UnaryOperator(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.operand.clone(), new_parent.clone());
            }
            Ast::BinaryOperator(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.left.clone(), new_parent.clone());
                self.link_ch_nodes_rec(n.right.clone(), new_parent.clone());
            }
            Ast::Call(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.callee.clone(), new_parent.clone());
                for arg in &n.args {
                    self.link_ch_nodes_rec(arg.clone(), new_parent.clone());
                }
            }
            Ast::Assign(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.left.clone(), new_parent.clone());
                self.link_ch_nodes_rec(n.expr.clone(), new_parent.clone());
            }
            Ast::Identifier(n) => {
                n.parent = p_weak;
            }
            Ast::Boolean(n) => {
                n.parent = p_weak;
            }
            Ast::Number(n) => {
                n.parent = p_weak;
            }
            Ast::String(n) => {
                n.parent = p_weak;
            }
            Ast::Function(n) => {
                n.parent = p_weak;
                self.link_ch_nodes_rec(n.iden.clone(), new_parent.clone());
                for arg in &n.args {
                    self.link_ch_nodes_rec(arg.0.clone(), new_parent.clone());
                    self.link_ch_nodes_rec(arg.1.clone(), new_parent.clone());
                }
                self.link_ch_nodes_rec(n.body.clone(), new_parent.clone());
            }
        }
    }

    fn validate_backtrack_nodes(&mut self, node: &Rc<RefCell<Ast>>) {
        let fn_symbol: Option<Rc<RefCell<SymbolTableEntry>>>;
        match node.as_ref().borrow().deref() {
            Ast::Return(_) => {
                fn_symbol = Some(self.get_parent_fn_node(&node));
            },
            _ => panic!("Error in `validate_backtrack_nodes` function. (This is a compiler bug, please report.)"),
        }

        if let Some(fn_symbol) = fn_symbol {
            convert_node!(node.as_ref().borrow_mut().deref_mut(), Return).symbol_table_node = Some(fn_symbol);
        }
    }

    fn get_parent_fn_node(&mut self, ast: &Rc<RefCell<Ast>>) -> Rc<RefCell<SymbolTableEntry>> {
        match ast.as_ref().borrow().deref() {
            Ast::Function(f) => {
                let iden = f.iden.as_ref().borrow();
                let name = &convert_node!(&iden.deref(), Identifier).token.lexeme;
                match SymbolTable::get_entry_from_node(&mut self.curr_node, name) {
                    Some(entry) => entry,
                    _ => panic!("Error in `Ast::Function` case of `get_parent_fn_node`. (This is a compiler bug, please report.)"),
                }
            },
            other => {
                match other.get_parent() {
                    Some(parent) => self.get_parent_fn_node(&match parent.upgrade() {
                        Some(p) => p,
                        _ => panic!("Error in `other` case of `get_parent_fn_node`. (This is a compiler bug, please report.)")
                    }),
                    _ => panic!("Error in `get_parent_fn_node`. (This is a compiler bug, please report.)"),
                }
            }
        }
    }
}
pub struct SymbolTableNode {
    pub parent: Option<Rc<RefCell<SymbolTableNode>>>,
    child_scopes: Vec<Rc<RefCell<SymbolTableNode>>>,
    entries: HashMap<String, Rc<RefCell<SymbolTableEntry>>>,
}

impl SymbolTableNode {
    pub fn new() -> SymbolTableNode {
        SymbolTableNode { parent: None, child_scopes: vec![], entries: HashMap::new() }
    }
}
pub enum SymbolTableEntry {
    Ident(IdentEntry),
    Type(TypeEntry),
}

impl SymbolTableEntry {
    pub fn get_token (&self) -> Token {
        match self {
            SymbolTableEntry::Ident(t) => t.token.clone(),
            SymbolTableEntry::Type(t) => t.token.clone(),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            SymbolTableEntry::Ident(t) => t.ty.clone(),
            SymbolTableEntry::Type(t) => t.ty.clone(),
        }
    }

    pub fn new_ident(token: &Token, ty: &Ast) -> SymbolTableEntry {
        use SymbolTableEntry::*;

        let ty = match ty {
            Ast::Type(t) => {
                t.clone()
            },
            _ => panic!("Error in `new_ident` function. (This is a compiler bug, please report.)"),
        };

        Ident (
            IdentEntry {
                token: token.clone(),
                ty,
                stack_location: 0,
            }
        )
    }

    #[allow(dead_code)]
    pub fn new_type(token: &Token, ty: &Ast) -> SymbolTableEntry {
        use SymbolTableEntry::*;

        let ty = match ty {
            Ast::Type(t) => {
                t.clone()
            },
            _ => panic!("Error in `new_type` function. (This is a compiler bug, please report.)"),
        };

        Type (
            TypeEntry {
                token: token.clone(),
                ty
            }
        )
    }
}

pub struct IdentEntry {
    pub token: Token,
    pub ty: Type,
    pub stack_location: usize,
}

pub struct TypeEntry {
    pub token: Token,
    pub ty: Type,
}
