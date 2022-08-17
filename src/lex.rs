use std::{collections::{LinkedList, HashMap}};

use crate::errors::*;

///Enum to specify which type a token is.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {

    Eof,

    //Types

    AtomicType(TokAtomicTypes),
    Inferred,
    Identifier,

    //Operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Assign,

    Equality,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,

    And,
    Or,

    //Keywords
    Let,
    Fn,
    Print,
    

    //Control Flow
    If,
    Else,
    While,
    Return,

    //Grammar
    LParen,
    RParen,
    LBrace,
    RBrace,
    LCurly,
    RCurly,
    Comma,
    Period,
    Semicolon,
    Colon,
    Arrow,


    //Just used for compiler errors
    Expression,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokAtomicTypes {
    Number, String, 
    True, False,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub lexeme: String,
    pub token_type: TokenType,
    pub line: usize,
    pub col: usize,
}
 
impl Token {
    pub fn new(lexeme: &str, token_type: TokenType, line: usize, col: usize) -> Token {
        Token { lexeme: lexeme.to_string(), token_type, line, col }
    }
}

///Essentially a tree structure to hold the possible simple words of our language.
struct WordNode {
    pub children: HashMap<char, WordNode>,
    pub output: Option<(String, TokenType)>,
}

impl WordNode {
    pub fn build(words: Vec<(&str, TokenType)>) -> WordNode {
        let mut head = WordNode{children: HashMap::new(), output: None};

        for word in &words {
            head.build_rec(*word, word.0);
        }

        head
    }

    fn build_rec(&mut self, word: (&str, TokenType), curr_word: &str) {
        if curr_word.is_empty() {
            self.output = Some((word.0.to_string(), word.1));
        }
        else {
            match self.children.get_mut(&curr_word.chars().next().unwrap()) {
                Some(child) => child.build_rec(word, &curr_word[1..]),
                None => {
                    let mut newnode = WordNode{children: HashMap::new(), output: None};
                    newnode.build_rec(word, &curr_word[1..]);
                    self.children.insert(curr_word.chars().next().unwrap(), newnode);
                }
            }
        }
    }

    fn get_longest(&self, letters: &Vec<char>, mut place: usize) -> Option<(String, TokenType)> {

        let mut curr_node = self;
        let mut curr_word = String::new();
        //curr_word.reserve(64);

        while place < letters.len() {
            match curr_node.children.get(&letters[place]) {
                Some(n) => {
                    curr_node = n;
                    curr_word.push(letters[place]);
                }
                None => {
                    return match &curr_node.output {
                        Some(output) => { Some(output.clone()) },
                        None => { None },
                    };
                }
            }

            place += 1;
        }

        curr_node.output.as_ref().cloned()
    }

    #[allow(dead_code)]
    fn print(&self, c: char, indent: usize) {
        println!("{}+- {}", "| ".repeat(indent),
            match &self.output {
                Some(o) => format!("{} {:?}", o.0, o.1),
                None => format!("{}", c),
            }
        );

        for pair in &self.children {
            pair.1.print(*pair.0, indent + 1);
        }
    }
}


fn build_char_vec(filepath: &str, vector: &mut Vec<char>) {
    let data = filepath.to_string();
    vector.reserve(data.len());
    for c in data.chars() {vector.push(c)};
}


fn complex_match(letters: &Vec<char>, mut place: usize, offset: &mut usize) -> Option<(String, TokenType)> {
    match letters.get(place) {
        Some(mut letter) => {
            let mut word = String::new();
            word.reserve(64);
            // (0-9)* .? (0-9)+
            if letter.is_numeric() || *letter == '.' {

                while letter.is_numeric() {
                    word.push(*letter);
                    place += 1;
                    if place >= letters.len() {return Some((word, TokenType::AtomicType(
                        TokAtomicTypes::Number
                    )));}
                    letter = &letters[place];
                }

                if *letter == '.' {
                    word.push('.');
                    place += 1;
                    if place >= letters.len() {return Some((word, TokenType::AtomicType(
                        TokAtomicTypes::Number
                    )));}
                    letter = &letters[place];

                    while letter.is_numeric() {
                        word.push(*letter);
                        place += 1;
                        if place >= letters.len() {return Some((word, TokenType::AtomicType(
                            TokAtomicTypes::Number
                        )));}
                        letter = &letters[place];
                    }
                }
                return Some((word, TokenType::AtomicType(TokAtomicTypes::Number)));
            }
            
            //(a-zA-Z_)(a-zA-Z_0-9)*

            if letter.is_alphabetic() || *letter == '_' {
                while letter.is_alphanumeric() || *letter == '_' {

                    word.push(*letter);

                    place += 1;
                    
                    if place >= letters.len() {return Some((word, TokenType::Identifier));}
                    letter = &letters[place];
                }

                return Some((word, TokenType::Identifier));
            }

            //'"' .* '"'

            if *letter == '"' {
                place += 1;
                while place < letters.len() && letters[place] != '"' {
                    word.push(letters[place]);
                    place += 1;
                }
                *offset = 2;
                return Some((word, TokenType::AtomicType(TokAtomicTypes::String)));
            }
            None
        },
        None => None
    }
}

pub fn lex(program: &str, words: Vec<(&str, TokenType)>) -> Result<LinkedList<Token>, String> {
    let mut list: LinkedList<Token> = LinkedList::new();
    let tree = WordNode::build(words);

    let mut charvec: Vec<char> = vec![];
    build_char_vec(program, &mut charvec);


    let mut place: usize = 0;
    let mut line = 1;
    let mut col = 1;

    while place < charvec.len() {
        if charvec[place].is_whitespace() {
            col += 1;
            if charvec[place] == '\n' {line += 1; col = 0;}
            place += 1;
            continue;
        }

        if charvec[place] == '/' && !(place >= charvec.len()) && charvec[place + 1] == '/' {
            while place < charvec.len() && charvec[place] != '\n' {place += 1;}
            continue;
        }

        let simple_match = tree.get_longest(&charvec, place);
        let mut offset = 0;
        let complex_match = complex_match(&charvec, place, &mut offset);

        match (simple_match, complex_match) {
            (None, None) => {
                return Err(lex_err(format!("Unknown character `{}`", charvec[place]).as_str(), line, col));
            },

            (Some(m), None) | (None, Some(m))=> {
                list.push_back(Token { lexeme: m.0.clone(), token_type: m.1, line, col});
                place += m.0.len() + offset;
                col += m.0.len() + offset;
            }

            (Some(a), Some(b)) => {
                let longest = if a.0.len() >= b.0.len() {a} else {b};
                list.push_back(Token { lexeme: longest.0.clone(), token_type: longest.1, line, col});
                place += longest.0.len();
                col += longest.0.len();
            }
        }

    }

    Ok(list)
}