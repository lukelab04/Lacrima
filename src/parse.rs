use std::cell::RefCell;
use std::collections::{LinkedList, HashMap};
use std::rc::Rc;
use std::ops::Deref;

use crate::{ast, convert_node};
use crate::errors::*;
use crate::lex::*;
use crate::ast::*;
use crate::Type::Atomic;
use crate::types;
use crate::types::*;

type PrefixFn = fn(&mut Parser) -> Result<Ast, String>;
type PostfixFn = fn(&mut Parser, Ast) -> Result<Ast, String>;


#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[allow(dead_code)]
pub enum Prec {
    None            = 0,
    Assignment      = 10,
    Condition       = 15,
    Comparison      = 20,
    Sum             = 30,
    Prod            = 40,
    Exp             = 50,
    Prefix          = 60,
    Postfix         = 80,
    Call            = 90,
}

pub struct Parser {
    tokens: LinkedList<Token>,

    pub prefix_map: HashMap<TokenType, PrefixFn>,
    pub postfix_map: HashMap<TokenType, PostfixFn>,
    pub control_map: HashMap<TokenType, PrefixFn>,
    pub prec_map: HashMap<TokenType, Prec>,
}


impl Parser {
    pub fn new(tokens: LinkedList<Token>) -> Parser {
        Parser { tokens, prefix_map: HashMap::new(), postfix_map: HashMap::new(), 
            prec_map: HashMap::new(), control_map: HashMap::new()}
    }

    pub fn register_prefix(&mut self, t: TokenType, func: PrefixFn) {
        self.prefix_map.insert(t, func);
    }

    pub fn register_postfix(&mut self, t: TokenType, func: PostfixFn, p: Prec) {
        self.postfix_map.insert(t, func);
        self.prec_map.insert(t, p);
    }

    #[allow(dead_code)]
    pub fn register_control_flow(&mut self, t: TokenType, func: PrefixFn) {
        self.control_map.insert(t, func);
    }

    pub fn parse(&mut self) -> Result<Ast, String> {
        parse_start(self)
    }

    pub fn peek(&self) -> Token {
        self.tokens.front().unwrap_or(&Token::new("", TokenType::Eof, 0, 0)).clone()
    }

    pub fn accept(&mut self, t: TokenType) -> Result<Token, String> {
        match self.tokens.front() {
            Some(tok) => {
                match tok.token_type {
                    _ if tok.token_type == t => Ok(self.tokens.pop_front().unwrap()),
                    _ => Err(expected_type(t, tok)),
                }
            },
            _ => Err(unexpected_end())
        }
    }

    pub fn accept_any(&mut self) -> Result<Token, String> {
        match self.tokens.pop_front() {
            Some(tok) => Ok(tok),
            _ => Err(unexpected_end())
        }
    }

    pub fn get_precedence(&self, t: TokenType) -> Prec {
        match self.prec_map.get(&t) {
            Some(p) => *p,
            None => Prec::None,
        }
    }
}


fn parse_start(p: &mut Parser) -> Result<Ast, String> {

    let mut children: Vec<Rc<RefCell<Ast>>> = vec![];

    while p.peek().token_type != TokenType::Eof {
        let tok = p.peek();

        match tok.token_type {
            TokenType::Let => children.push(Rc::new(RefCell::new(parse_var_decl(p)?))),
            TokenType::Fn => children.push(Rc::new(RefCell::new(parse_fn_decl(p)?))),
            _ if p.prefix_map.contains_key(&tok.token_type) => children.push(Rc::new(RefCell::new(parse_expression_line(p, Prec::None)?))),
            _ if p.control_map.contains_key(&tok.token_type) => children.push(Rc::new(RefCell::new(parse_expression_line(p, Prec::None)?))),
            _ => {return Err(expected_but_got("Let or expression", &tok));},
        }
    };

    Ok(Ast::Start(Start {children}))
}

pub fn parse_var_decl(p: &mut Parser) -> Result<Ast, String> {
    let tok = p.accept(TokenType::Let)?;
    let iden = parse_identifier(p)?;
    p.accept(TokenType::Colon)?;
    let ty = parse_type(p)?;

    let next = p.peek();
    let value: Option<Rc<RefCell<Ast>>>;

    match next.token_type {
        TokenType::Semicolon => {
            p.accept(TokenType::Semicolon)?;
            value = None;
            println!("Variables should define a default value");
        },
        TokenType::Assign => {
            p.accept(TokenType::Assign)?;
            value = Some(Rc::new(RefCell::new(parse_expression_line(p, Prec::None)?)));
        },
        _ => return Err(expected_but_got("`;` or `=`", &next)),
    }

    Ok(
        Ast::VarDecl(
            VarDecl {
                token: tok,
                parent: None,
                iden: Rc::new(RefCell::new(iden)),
                ty: Rc::new(RefCell::new(ty)),
                value,
            }
        )
    )
}

pub fn parse_fn_decl(p: &mut Parser) -> Result<Ast, String> {
    //Get function name
    let tok = p.accept(TokenType::Fn)?;
    let iden = parse_identifier(p)?;

    //Init arg list (iden, type)
    let mut args = vec![];

    p.accept(TokenType::LParen)?;
    let mut next = p.peek();

    //Get all name + type pairs until rparen is hit
    while next.token_type != TokenType::RParen {
        let name = parse_identifier(p)?;
        p.accept(TokenType::Colon)?;
        let ty = parse_type(p)?;

        args.push((Rc::new(RefCell::new(name)), Rc::new(RefCell::new(ty))));

        next = p.peek();

        if next.token_type == TokenType::Comma {
            p.accept(TokenType::Comma)?;
            next = p.peek();
        }
    }

    //Tuple type representing arguments
    let mut arg_tup = vec![];

    //Populate argument tuple
    for arg in &args {
        arg_tup.push(Rc::new(RefCell::new(convert_node!(arg.1.as_ref().borrow().deref(), Type).clone())))
    }

    p.accept(TokenType::RParen)?;
    p.accept(TokenType::Arrow)?;

    let ret_type = parse_type(p)?;
    let body = Rc::new(RefCell::new(parse_block(p)?));

    Ok(Ast::Function(Function {
        token: tok,
        parent: None,
        iden: Rc::new(RefCell::new(iden)),
        ty: Rc::new(RefCell::new(Ast::Type(Type::Functional(FunctionType {
            ret_type: Box::new(convert_node!(ret_type, Type)),
            arg_type: Box::new(Type::Algebraic(AlgebraicType::Tuple(TupleType {
                types: arg_tup
            }))),
        })))),
        args,
        body,
    }))
}

fn parse_expression_line(p: &mut Parser, prec: Prec) -> Result<Ast, String> {
    let res = parse_expression(p, prec)?;
    p.accept(TokenType::Semicolon)?;
    Ok(res)
}

pub fn parse_expression(p: &mut Parser, prec: Prec) -> Result<Ast, String> {
    let next = p.peek();

    let mut left = match next.token_type {
        t if p.prefix_map.contains_key(&t) => parse_prefix(p),
        t if p.control_map.contains_key(&t) => {
            parse_control_flow(p)
        },
        _ => {
            return Err(expected_type(TokenType::Expression, &next));
        }
    }?;


    while prec < p.get_precedence(p.peek().token_type) {
        left = parse_postfix(p, left)?;
    }

    Ok(left)
}

pub fn parse_prefix(p: &mut Parser) -> Result<Ast, String> {
    let next = p.peek();

    if p.prefix_map.contains_key(&next.token_type) {p.prefix_map[&next.token_type](p)}
    else {Err(expected_but_got("prefix", &next))}
}

pub fn parse_control_flow(p: &mut Parser) -> Result<Ast, String> {
    let next = p.peek();
    if p.control_map.contains_key(&next.token_type) {p.control_map[&next.token_type](p)}
    else {Err(expected_but_got("control flow", &next))}
}

pub fn parse_if_stmt(p: &mut Parser) -> Result<Ast, String> {
    p.accept(TokenType::If)?;
    let condition = parse_expression(p, Prec::None)?;
    let block = parse_block(p)?;

    Ok(Ast::IfStmt(IfStmt{
        parent: None,
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        condition: Rc::new(RefCell::new(condition)),
        block: Rc::new(RefCell::new(block)),
        follow: parse_follow_if(p)?,
    }))
}

pub fn parse_while_stmt(p: &mut Parser) -> Result<Ast, String> {
    p.accept(TokenType::While)?;
    let expr = parse_expression(p, Prec::None)?;
    let block = parse_block(p)?;
    Ok(Ast::WhileStmt(WhileStmt{
        parent: None,
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        condition: Rc::new(RefCell::new(expr)),
        block: Rc::new(RefCell::new(block)),
    }))
}

pub fn parse_follow_if(p: &mut Parser) -> Result<Option<Rc<RefCell<Ast>>>, String> {
    let next = p.peek();
    match next.token_type {
        TokenType::Else => {
            p.accept(TokenType::Else)?;
            let next = p.peek();
            match next.token_type {
                TokenType::If => Ok(Some(Rc::new(RefCell::new(parse_if_stmt(p)?)))),
                TokenType::LCurly => Ok(Some(Rc::new(RefCell::new(parse_block(p)?)))),
                _ => Err(expected_but_got("If statement or Block", &next)),
            }
        },
        _ => Ok(None),
    }
}

pub fn parse_block(p: &mut Parser) -> Result<Ast, String> {
    p.accept(TokenType::LCurly)?;

    let mut children = Vec::new();

    let mut next = p.peek();
    while next.token_type != TokenType::RCurly {
        match next.token_type {
            TokenType::Let => children.push(Rc::new(RefCell::new(parse_var_decl(p)?))),
            _ if p.prefix_map.contains_key(&next.token_type) => children.push(Rc::new(RefCell::new(parse_expression_line(p, Prec::None)?))),
            _ if p.control_map.contains_key(&next.token_type) => children.push(Rc::new(RefCell::new(parse_expression_line(p, Prec::None)?))),
            _ => {return Err(expected_but_got("Let or expression", &next));},
        }
        next = p.peek();
    }

    p.accept(TokenType::RCurly)?;

    Ok(Ast::Block(Block{
        children,
        parent: None,
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
    }))
}

pub fn parse_identifier(p: &mut Parser) -> Result<Ast, String> {
    Ok (
        Ast::Identifier(
            Identifier {
                parent: None,
                token: p.accept(TokenType::Identifier)?,
                symbol_table_node: None,
            }
        )
    )
}

pub fn parse_number(p: &mut Parser) -> Result<Ast, String> {
    let num = p.accept(TokenType::AtomicType(TokAtomicTypes::Number))?;
    Ok(Ast::Number(Number{
        parent: None,
        val: num.lexeme.parse().unwrap(),
        ty: Rc::new(RefCell::new(Ast::Type(Type::Atomic(AtomicType::Number))))
    }))

}

pub fn parse_boolean(p: &mut Parser) -> Result<Ast, String> {
    use TokenType::*;
    use TokAtomicTypes::*;

    let b = p.accept_any()?;

    let val = if b.token_type == AtomicType(True) {
        true
    } else if b.token_type == AtomicType(False) {
        false
    } else {
        return Err(expected_but_got("`true` or `false`", &b));
    };

    Ok(Ast::Boolean(Boolean {
        parent: None,
        ty: Rc::new(RefCell::new(Ast::Type(Type::Atomic(types::AtomicType::Boolean)))),
        val
    }))
}

pub fn parse_inferred_type(p: &mut Parser) -> Result<Ast, String> {

    let _tok = p.accept(TokenType::Inferred)?;

    Ok(Ast::Type(Type::Inferred))

}

pub fn parse_str(p: &mut Parser) -> Result<Ast, String> {
    let tok = p.accept(TokenType::AtomicType(TokAtomicTypes::String))?;
    Ok(Ast::String(Str{
        parent: None,
        val: tok.lexeme.clone(),
        ty: Rc::new(RefCell::new(Ast::Type(Atomic(AtomicType::String)))),
    }))
}

pub fn parse_postfix(p: &mut Parser, left: Ast) -> Result<Ast, String> {
    match p.postfix_map.get(&p.peek().token_type) {
        Some(f) => f(p, left),
        _ => Err(expected_but_got("postfix", &p.peek())),
    }
}

pub fn parse_binary_operator(p: &mut Parser, left: Ast) -> Result<Ast, String> {
    let tok = p.accept_any()?;
    use BinaryOperatorTypes::*;
    
    let op_type = match tok.token_type {

        TokenType::Add => Add,
        TokenType::Sub => Sub,
        TokenType::Mul => Mul,
        TokenType::Div => Div,
        TokenType::Mod => Mod,
        TokenType::Equality => Equality,
        TokenType::NotEqual => NotEqual,
        TokenType::Less => Less,
        TokenType::Greater => Greater,
        TokenType::LessEqual => LessEqual,
        TokenType::GreaterEqual => GreaterEqual,
        TokenType::And => And,
        TokenType::Or => Or,

        _ => panic!("Error in `parse_binary_operator`. (This is a compiler bug, please report.)"),

    };

    Ok(Ast::BinaryOperator( BinaryOperator {
        parent: None,
        token: tok.clone(),
        op_type,
        left: Rc::new(RefCell::new(left)),
        right: Rc::new(RefCell::new(parse_expression(p, p.get_precedence(tok.token_type))?)),
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        left_ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        right_ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
    }))

}

pub fn parse_assignment(p: &mut Parser, left: Ast) -> Result<Ast, String> {
    let tok = p.accept(TokenType::Assign)?;

    Ok(Ast::Assign ( Assign {
        parent: None,
        token: tok,
        left: Rc::new(RefCell::new(left)),
        expr: Rc::new(RefCell::new(parse_expression(p, Prec::None)?)),
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
    }))
}

pub fn parse_unary_operator(p: &mut Parser) -> Result<Ast, String> {
    let tok = p.accept_any()?;
    use UnaryOperatorTypes::*;
    
    let op_type = match tok.token_type {
        
        TokenType::Sub => Negate,
        
        _ => panic!("Error in `parse_unary_operator` function. (This is compiler bug, please report)."),
    };

    Ok(Ast::UnaryOperator(UnaryOperator {
        parent: None,
        token: tok,
        op_type,
        operand: Rc::new(RefCell::new(parse_expression(p, Prec::Prefix)?)),
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        operand_ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
    }))
}

pub fn parse_call(p: &mut Parser, left: Ast) -> Result<Ast, String> {
    let tok = p.accept(TokenType::LParen)?;
    let mut args: Vec<Rc<RefCell<Ast>>> = vec![];

    let mut next = p.peek();
    while next.token_type != TokenType::RParen {
        args.push(Rc::new(RefCell::new(parse_expression(p, Prec::None)?)));
        next = p.peek();
        if next.token_type == TokenType::Comma {
            p.accept(TokenType::Comma)?;
            next = p.peek();
        }
    }
    p.accept(TokenType::RParen)?;

    Ok(Ast::Call(Call{
        parent: None,
        token: tok,
        callee: Rc::new(RefCell::new(left)),
        args,
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType)))
    }))
}

pub fn parse_return(p: &mut Parser) -> Result<Ast, String> {
    let token = p.accept(TokenType::Return)?;
    Ok(Ast::Return(Return {
        symbol_table_node: None,
        parent: None,
        token,
        ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        elem: Rc::new(RefCell::new(parse_expression(p, Prec::None)?)),
    }))
}

pub fn parse_paren_group(p: &mut Parser) -> Result<Ast, String> {
    p.accept(TokenType::LParen)?;
    let interior = parse_expression(p, Prec::None)?;
    p.accept(TokenType::RParen)?;
    Ok(interior)
}

pub fn parse_print(p: &mut Parser) -> Result<Ast, String> {
    p.accept(TokenType::Print)?;
    Ok(Ast::Print(
        Print {
            parent: None,
            node: Rc::new(RefCell::new(parse_expression(p, Prec::None)?)),
            ty: Rc::new(RefCell::new(Ast::Type(Type::EmptyType))),
        }
    ))
}

pub fn parse_type(p: &mut Parser) -> Result<Ast, String> {
    let next = p.peek();

    match next.token_type {
        TokenType::AtomicType(_) => parse_atomic_type(p),
        TokenType::Inferred => parse_inferred_type(p),
        TokenType::LParen => parse_tuple_type(p),
        _ => Err(parse_err(format!("Unknown type {}", next.lexeme.clone()).as_str(), &next)),
    }
}

pub fn parse_atomic_type(p: &mut Parser) -> Result<Ast, String> {
    use ast::Ast::Type;
    use types::AtomicType::*;

    match p.accept_any()?.token_type {
        TokenType::AtomicType(t) => match t {
            TokAtomicTypes::True => Ok(Type(Atomic(Boolean))),
            TokAtomicTypes::False => Ok(Type(Atomic(Boolean))),
            TokAtomicTypes::Number => Ok(Type(Atomic(Number))),
            TokAtomicTypes::String => Ok(Type(Atomic(String))),
        }
        _ => panic!("Error in `parse_atomic_type` function. (This is a compiler bug, please report.)"),
    }
}

pub fn parse_tuple_type(p: &mut Parser) -> Result<Ast, String> {
    p.accept(TokenType::LParen)?;
    let mut types = vec![];

    let mut next = p.peek();
    while next.token_type != TokenType::RParen {
        types.push(Rc::new(RefCell::new(convert_node!(parse_type(p)?, Type))));
        next = p.peek();

        if next.token_type == TokenType::Comma {
            p.accept(TokenType::Comma)?;
            next = p.peek();
        }
    }

    p.accept(TokenType::RParen)?;

    Ok(Ast::Type(Type::Algebraic(AlgebraicType::Tuple(TupleType {
        types
    }))))
}
