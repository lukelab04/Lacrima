use crate::lex::*;

pub fn basic_error(message: &str) -> String { message.to_string() }

pub fn lex_err(message: &str, line: usize, col: usize) -> String {
    format!("Error: {} (line {}, column {})", message, line, col)
}

pub fn parse_err(message: &str, t: &Token) -> String {
    format!("Error: {} (line {}, column {})", message, t.line, t.col)
}

pub fn expected_type(expected: TokenType, t: &Token) -> String {
    format!("Error: Expected type {:?} but got `{:?}` (line {}, column {})", expected, t.token_type, t.line, t.col)
}

pub fn unexpected_end() -> String {
    "Error: Unexpected end to token stream.".to_string()
}

pub fn expected_but_got(expected: &str, got: &Token) -> String {
    format!("Error: Expected {} but got `{}` (line {}, column {})",
            expected, got.lexeme, got.line, got.col)
}

pub fn name_redec_error(orig_def: &Token, redef: &Token) -> String {
    format!("Error: Identifier `{}` was declared (line {}, column {}). Redeclaration not allowed (line {}, column {})",
            orig_def.lexeme, orig_def.line, orig_def.col, redef.line, redef.col)
}

pub fn undeclared_name(tok: &Token) -> String {
    format!("Error: Identifier `{}` (line {}, column {}) was not declared.", tok.lexeme, tok.line, tok.col)
}

pub fn type_inequality(t1: &str, t2: &str) -> String {
    format!("Error: type {} does not match type {}", t1, t2)
}

pub fn unary_type_error(op: &str, t: &str) -> String {
    format!("Error: operator {} is not defined on type {}", op, t)
}

pub fn binary_type_error(op: &str, t1: &str, t2: &str) -> String {
    format!("Error: operator {} is not defined for types {} and {}", op, t1, t2)
}


pub fn incorrect_arg_num(expected: usize, got: usize, t: &Token) -> String {
    format!("Error: expected {} arguments but got {} (line {}, col {}", expected, got, t.line, t.col)
}