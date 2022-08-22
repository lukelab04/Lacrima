mod lex;
mod parse;
mod errors;
mod symbol_table;
mod ast;
mod type_check;
mod codegen;
mod vm;
mod value;
mod types;

use std::collections::HashMap;
use lex::*;
use parse::*;
use symbol_table::*;
use type_check::TypeChecker;
use codegen::*;
use vm::*;
use types::*;
use value::*;

pub type LCValue = Value;

pub struct ExternalFunctions {
    pub functions: HashMap<String, fn(Vec<Value>) -> ()>
}
impl ExternalFunctions {
    pub fn new() -> ExternalFunctions {ExternalFunctions {functions: HashMap::new()}}

    pub fn add_fn(&mut self, name: &str, func: fn(Vec<Value>) -> ()) {
       self.functions.insert(name.to_string(), func);
    }
}

///Returns a vector of 'basic words' (words that do not require more extensive patten matching, like numbers.)
fn get_basic_words() -> Vec<(&'static str, TokenType)> {
    use TokenType::*;
    use TokAtomicTypes::*;

    //Vector of tuples containing the string to match and its corresponding token type.
    vec![
        ("num", AtomicType(Number)), ("str", AtomicType(String)),
        ("true", AtomicType(True)), ("false", AtomicType(False)), ("bool", AtomicType(True)),
        ("auto", Inferred),

        ("let", Let), ("fn", Fn), ("external", External),

        ("if", If), ("else", Else), ("while", While), ("return", Return),

        ("(", LParen), (")", RParen), ("[", LBrace), ("]", RBrace), ("{", LCurly),
        ("}", RCurly), (".", Period), (",", Comma), (";", Semicolon), (":", Colon),
        ("=", Assign), ("->", Arrow),
    
        ("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("%", Mod),
        ("==", Equality), ("!=", NotEqual), ("<", Less), (">", Greater),
        ("<=", LessEqual), (">=", GreaterEqual),
        ("||", Or), ("&&", And),
    ]
}

///Initializes the parser with necessary token types and links those types to their respective parsing functions.
fn init_parser(p: &mut Parser) {
    use lex::TokenType::*;
    use lex::TokAtomicTypes::*;

    p.register_prefix(Identifier, parse_identifier);
    p.register_prefix(AtomicType(Number), parse_number);
    p.register_prefix(AtomicType(True), parse_boolean);
    p.register_prefix(AtomicType(False), parse_boolean);
    p.register_prefix(AtomicType(String), parse_str);
    p.register_prefix(LParen, parse_paren_group);
    p.register_prefix(Sub, parse_unary_operator);

    p.register_postfix(Add, parse_binary_operator, Prec::Sum);
    p.register_postfix(Sub, parse_binary_operator, Prec::Sum);
    p.register_postfix(Mul, parse_binary_operator, Prec::Prod);
    p.register_postfix(Div, parse_binary_operator, Prec::Prod);
    p.register_postfix(Mod, parse_binary_operator, Prec::Prod);

    p.register_postfix(Assign, parse_assignment, Prec::Assignment);

    p.register_postfix(Equality, parse_binary_operator, Prec::Comparison);
    p.register_postfix(NotEqual, parse_binary_operator, Prec::Comparison);
    p.register_postfix(Less, parse_binary_operator, Prec::Comparison);
    p.register_postfix(Greater, parse_binary_operator, Prec::Comparison);
    p.register_postfix(LessEqual, parse_binary_operator, Prec::Comparison);
    p.register_postfix(GreaterEqual, parse_binary_operator, Prec::Comparison);

    p.register_postfix(And, parse_binary_operator, Prec::Condition);
    p.register_postfix(Or, parse_binary_operator, Prec::Condition);

    p.register_postfix(LParen, parse_call, Prec::Call);


    p.register_control_flow(If, parse_if_stmt);
    p.register_control_flow(While, parse_while_stmt);
    p.register_control_flow(Return, parse_return);
}


///Container for all compiled code from every file supplied to `compile` function.
pub struct Bytecode {
    ///Vector of 32 bit bytecode.
    pub code: Vec<u32>,
    ///Constant values for the program.
    ///<br> <br>
    /// For example, in the snippet `let a: num = 3`, the constant `3` would be supplied from indexing into the consts value list.
    pub consts: Vec<Value>,
}


///Compile a program into bytecode.
/// <br><br>
/// Return `Bytecode` struct on success, otherwise `Err` containing `String` error message.
pub fn compile(program: &str, externs: &ExternalFunctions) -> Result<Bytecode, String> {

    let tokens = lex::lex(&program, get_basic_words())?;

    let mut p = Parser::new(tokens);
    init_parser(&mut p);

    let mut ast = p.parse()?;

    let mut st = SymbolTable::new();
    ast = st.build_table(ast, externs)?;

    let mut tc = TypeChecker::new();
    tc.check_types(&mut ast)?;


    let mut cg = CodeGenerator::new();
    let code = cg.generate(&mut ast);


    Ok(Bytecode {
        code,
        consts: cg.const_vals
    })

}

pub fn init_vm() -> Vm {
    Vm::new()
}

///Run bytecode on virtual machine.
/// # Panics
/// Panics on unknown opcode.
pub fn run(vm: &mut Vm, bytecode: &Bytecode, externs: &ExternalFunctions) {
    let start = std::time::Instant::now();
    vm.run(&bytecode.code, &bytecode.consts, externs);
    println!("Finished in {}ms", (std::time::Instant::now() - start).as_micros());
}
