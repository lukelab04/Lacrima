use std::cell::RefCell;
use std::ops::Deref;
use crate::ast::*;
use crate::{BinaryOperatorTypes, convert_node};
use crate::value::*;
use crate::value;


///Enum to hold the values and mnemonics for bytecode chunks.
/// <br><br>
/// Functions essentially as a C-style enum, just as a way to associate names with values.
#[allow(dead_code)]
enum Instructions {
    //Memory
    Mov             = 0x00,
    Load            = 0x01,
    LoadImm         = 0x02,


    //Math
    Add             = 0x10,
    Sub             = 0x11,
    Div             = 0x12,
    Mul             = 0x13,
    Mod             = 0x14,

    And             = 0x15,
    Or              = 0x16,
    Eq              = 0x17,
    Neq             = 0x18,
    Gt              = 0x19,
    Gte             = 0x1A,
    Lt              = 0x1B,
    Lte             = 0x1C,
    Not             = 0x1D,


    //Control Flow
    IncIf0          = 0x30,
    DecIf0          = 0x31,
    JZ              = 0x32,
    JNZ             = 0x33,
    Jump            = 0x34,
    Call            = 0x35,
    Return          = 0x36,

    //Stack
    Push            = 0x40,
    Pop             = 0x41,
    PushFrame       = 0x42,
    PopFrame        = 0x43,
    SetLocVar       = 0x44,
    GetLocVar       = 0x45,
    SetGlobVar      = 0x46,
    GetGlobVar      = 0x47,
    GetStackLen     = 0x48,
    SetStackLen     = 0x49,


    //Misc
    NoOp            = 0xF0,
    Print           = 0xF1,
}

///Structure to organize the scope and location of variables in the program.
struct VarLookup {

    /// A `Vec<usize` containing the local address of a variable.
    /// <br><br>
    /// When a function is called, `0` is pushed to `addr`, meaning the first variable
    /// of that function will have a local address of 0.
    /// <br><br>
    /// Likewise, when functions are returned from,
    /// the top of `addr` is popped, putting the previous local address back on the top of the stack.
    addr: Vec<usize>,

    /// A `Vec` containing variable information for every variable currently in scope.
    vars: Vec<VCell>,

    /// A `usize` representing the current scope.
    /// <br><br>
    /// When a scope is entered, this value is incremented.
    /// <br><br>
    /// When a scope is exited, this value is decremented.
    /// All variables defined in the exited scope are popped from the `vars` list, rendering them out of scope.
    scope: usize,

    /// A `usize` representing the current frame.
    ///<br><br>
    /// Useful for determining whether a variable is global or local.
    frame: usize,
}

impl VarLookup {
    /// Create new VarLookup instance
    pub fn new() -> VarLookup {VarLookup {addr: vec![0], vars: vec![], scope: 0, frame: 0}}

    /// Push a new variable onto the `vars` list.
    pub fn declare_var(&mut self, name: &str) -> VCell {
        self.vars.push(VCell {
            name: name.to_string(),
            addr: *self.addr.last().unwrap(),
            scope: self.scope,
            frame: self.frame,
        });

        *self.addr.last_mut().unwrap() += 1;
        self.vars.last().unwrap().clone()
    }

    ///Get a variable by its name.
    /// <br><br>
    /// Searches the list backwards, from most recent addition to least recent.
    /// This makes sure variables in more recent scopes will be found first, preserving scoping rules.
    /// <br><br>
    /// # Panics
    /// Panics if no variable with the given name can be found.
    pub fn get_var(&mut self, name: &str) -> VCell {
        for i in (0..self.vars.len()).rev() {
            if self.vars[i].name == name {return self.vars[i].clone();}
        }
        panic!("Error in `get_var` function. (This is a compiler bug, please report.)")
    }

    pub fn push_scope(&mut self) {
        self.scope += 1;
    }

    ///Revert the scope back to the previous scope.
    /// <br><br>
    /// Deletes every variable that was declared in the exited scope.
    /// This ensures future variables with the same name will not clash with past variables in similar scopes.
    pub fn pop_scope(&mut self) {
        while self.vars.len() > 0 && self.vars.last().unwrap().scope == self.scope {
            self.vars.pop();
        }
        self.scope -= 1;
    }

    ///Increment frame counter and push `0` to local address stack.
    pub fn push_frame(&mut self) {
        self.frame += 1;
        self.addr.push(0);
    }

    ///Decrement frame counter and revert local address stack to the previous frame.
    pub fn pop_frame(&mut self) {
        self.frame -= 1;
        self.addr.pop();
    }

}

#[derive(Clone)]
struct VCell {
    name: String,
    //Local address of the variable
    addr: usize,
    scope: usize,
    frame: usize,
}


pub struct CodeGenerator {
    vars: VarLookup,
    ///List to hold all constant values defined in the program (e.g. `100.332` or '"Hello, matey."`)
    pub const_vals: Vec<Value>
}

///Macro to generate an opcode from various arguments.
/// <br><br>
/// # Examples
/// ```
/// opcode!(NoOp)
/// ```
/// Generates an opcode with the first byte as the instruction number, and 0 for all other bytes.
/// ```
/// opcode!(Pop, 0x1)
/// ```
/// Generates an opcode with the first byte as the instruction number and the next byte as the `x` register number. All other bytes are 0.
/// ```
/// opcode!(Add, 0x1, 0x2, 0x3)
/// ```
/// Generates an opcode with the first byte as the instruction number, and the other three bytes as the corresponding arguments.
macro_rules! opcode {
    ($op: expr) => {
        {
            (($op as u8) as u32) << 24
        }
    };

    ($op: expr, $x: expr) => {
        {
            let mut op = (($op as u8) as u32) << 24;
            op |= (($x as u8) as u32) << 16;
            op
        }
    };

    ($op: expr, $x: expr, $y: expr, $z: expr) => {
        {
            let mut op = (($op as u8) as u32) << 24;
            op |= (($x as u8) as u32) << 16;
            op |= (($y as u8) as u32) << 8;
            op |= ($z as u8) as u32;
            op
        }
    };
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {CodeGenerator {vars: VarLookup::new(), const_vals: vec![]}}
    pub fn generate(&mut self, ast: &mut Ast) -> Vec<u32> {
        let mut code = vec![];
        self.gen_code_rec(ast, &mut code);
        code
    }

    ///Recurse down Ast and generate code for each node.
    /// Code is appended to `code` argument.
    fn gen_code_rec(&mut self, ast: &Ast, code: &mut Vec<u32>) {

        use Instructions::*;


        match ast {
            Ast::Start(n) => {
                for c in &n.children {
                    //Generate code for every child
                    self.gen_code_rec(c.as_ref().borrow().deref(), code);
                }
            },
            Ast::VarDecl(n) => {
                //Put the variable in the current variable list
                self.vars.declare_var(&convert_node!(n.iden.as_ref().borrow().deref(), Identifier).token.lexeme);
                let var = self.vars.get_var(&convert_node!(n.iden.as_ref().borrow().deref(), Identifier).token.lexeme);

                //If the variable is defined with a value, generate it. Otherwise, use the 0x0 register (always 0).
                let reg = if let Some(val) = &n.value {
                    //Put value into 0x1
                    self.gen_code_rec(val.as_ref().borrow().deref(), code);
                    0x1
                } else {0x0};

                //Load variable address into 0x2
                code.push(opcode!(LoadImm, 0x2));
                code.push(var.addr as u32);

                //Copy register with data to local position at 0x2
                code.push(opcode!(SetLocVar, 0x2, 0x0, reg));

            },
            Ast::Type(_) => todo!(),
            Ast::IfStmt(_n) => {

                //End positions is a vec containing all places where execution should jump to the end of the if..else chain.
                let mut end_positions = vec![];

                //Create a function to recurse down our if..else chain, generating the necessary code as we go.
                fn gen_conditional_code(curr_node: &Ast, code: &mut Vec<u32>, end_positions: &mut Vec<usize>, cg: &mut CodeGenerator) {

                    //Position where we need to insert the location of the end of the block
                    let mut modify_pos: usize = 0;
                    //Differentiate between if statements and blocks
                    let mut is_if_stmt = false;

                    match curr_node {
                        Ast::IfStmt(if_stmt) => {
                            is_if_stmt = true;
                            //Save the conditional in 0x1
                            cg.gen_code_rec(if_stmt.condition.as_ref().borrow().deref(), code);

                            //Load jump address in 0x2
                            code.push(opcode!(LoadImm, 0x2));
                            modify_pos = code.len();
                            code.push(0);

                            //If condition is false, jump to end of block
                            code.push(opcode!(JZ, 0x2, 0x0, 0x1));

                            //Generate the code for the block
                            cg.gen_code_rec(if_stmt.block.as_ref().borrow().deref(), code);

                            //Load end jump address into 0x2
                            code.push(opcode!(LoadImm, 0x2));
                            end_positions.push(code.len());
                            code.push(0);
                            code.push(opcode!(Jump, 0x2));
                        },
                        Ast::Block(_block) => {
                            //Generate code for block
                            cg.gen_code_rec(curr_node, code);
                        },
                        _ => panic!("Error in code generation for `IfStmt`. (This is a compiler bug, please report.)"),
                    }

                    //NoOp acts as end of section
                    code.push(opcode!(NoOp));

                    if modify_pos > 0 {
                        //If there is a modify pos, set the current PC as the jump location
                        code[modify_pos] = (code.len() - 1) as u32;
                    }

                    //If the current node is an if statement, its `follow` field will contain the optional else statement. Generate that code if necessary.
                    if is_if_stmt {
                        let follow = convert_node!(curr_node, IfStmt).follow.as_ref();
                        if let Some(follow) = follow {
                            gen_conditional_code(follow.as_ref().borrow().deref(), code, end_positions, cg);
                        }
                    }
                }

                gen_conditional_code(&ast, code, &mut end_positions, self);

                //Replace all end positions with the address of the last block
                for pos in end_positions {
                    code[pos] = (code.len() - 1) as u32;
                }
            },
            Ast::WhileStmt(n) => {
                let orig_pos = code.len();
                //Save condition in 0x1
                self.gen_code_rec(n.condition.as_ref().borrow().deref(), code);
                //Load jump address into 0x2
                code.push(opcode!(LoadImm, 0x2));
                let jump_idx = code.len();
                code.push(0);

                //If 0x1 is false, jump to end
                code.push(opcode!(JZ, 0x2, 0x0, 0x1));

                //Generate code for the block
                self.gen_code_rec(n.block.as_ref().borrow().deref(), code);

                //Load jump pos into 0x2
                code.push(opcode!(LoadImm, 0x2));
                code.push(orig_pos as u32);
                //Jump to beginning
                code.push(opcode!(Jump, 0x2));


                //Set end jump loc
                code[jump_idx] = code.len() as u32;
                //NoOp as loop end
                code.push(opcode!(NoOp));
            },
            Ast::Block(n) => {
                self.vars.push_scope();
                for c in &n.children {self.gen_code_rec(c.as_ref().borrow().deref(), code);}
                self.vars.pop_scope();
            },
            Ast::UnaryOperator(_) => todo!(),
            Ast::BinaryOperator(n) => {
                //Get rhs in 0x1
                self.gen_code_rec(n.right.as_ref().borrow().deref(), code);
                //Push right side to stack
                code.push(opcode!(Push, 0x1));

                //Get lhs in 0x1
                self.gen_code_rec(n.left.as_ref().borrow().deref(), code);

                //Get operator
                let op = match n.op_type {
                    BinaryOperatorTypes::Add => Add,
                    BinaryOperatorTypes::Sub => Sub,
                    BinaryOperatorTypes::Mul => Mul,
                    BinaryOperatorTypes::Div => Div,
                    BinaryOperatorTypes::Mod => Mod,
                    BinaryOperatorTypes::Equality => Eq,
                    BinaryOperatorTypes::NotEqual => Neq,
                    BinaryOperatorTypes::Less => Lt,
                    BinaryOperatorTypes::Greater => Gt,
                    BinaryOperatorTypes::LessEqual => Lte,
                    BinaryOperatorTypes::GreaterEqual => Gte,
                    BinaryOperatorTypes::And => And,
                    BinaryOperatorTypes::Or => Or,
                };

                //Pop right side to 0x2
                code.push(opcode!(Pop, 0x2));
                //Push op
                code.push(opcode!(op, 0x1, 0x1, 0x2));
            },
            Ast::Call(n) => {
                //Save frame addr in x
                code.push(opcode!(GetStackLen, 0x2));

                //Push argument values to stack
                for arg in &n.args {
                    //Push frame addr to stack
                    code.push(opcode!(Push, 0x2));
                    //Generate arg value
                    self.gen_code_rec(arg.as_ref().borrow().deref(), code);
                    //Pop frame addr back into 0x2
                    code.push(opcode!(Pop, 0x2));
                    //Push arg value to stack
                    code.push(opcode!(Push, 0x1));
                }

                //Get address of function in 0x1
                self.get_addr_and_locality(n.callee.as_ref().borrow().deref(), code);

                //Push frame
                self.vars.push_frame();
                code.push(opcode!(PushFrame, 0x2));

                //Call function
                code.push(opcode!(Call, 0x1));

                //Return from frame
                self.vars.pop_frame();
                code.push(opcode!(PopFrame, 0x2));
                //Clear stack
                code.push(opcode!(SetStackLen, 0x2));

            },
            Ast::Assign(n) => {
                //Put value in 0x1
                self.gen_code_rec(n.expr.as_ref().borrow().deref(), code);
                //Move value to 0x2
                code.push(opcode!(Mov, 0x2, 0x0, 0x1));
                //Put address of variable in 0x1, and get frame of var
                let frame = self.get_addr_and_locality(n.left.as_ref().borrow().deref(), code);
                //Set variable
                code.push(opcode!(if frame == 0 {SetGlobVar} else {SetLocVar}, 0x1, 0x0, 0x2));
            },
            Ast::Identifier(n) => {
                //Get variable
                let var = self.vars.get_var(&n.token.lexeme);
                //Put variable addr in 0x2
                code.push(opcode!(LoadImm, 0x2));
                code.push(var.addr as u32);

                //Put var at addr 0x2 into 0x1
                code.push(opcode!(if var.frame == 0 {GetGlobVar} else {GetLocVar}, 0x1, 0x0, 0x2));
            },
            Ast::Boolean(n) => {
                //Load bool into 0x1
                code.push(opcode!(Load, 0x1));
                //PUsh addr
                code.push(self.const_vals.len() as u32);
                self.const_vals.push(Value::Boolean(n.val));
            },
            Ast::Number(n) => {
                //Load the number into 0x1
                code.push(opcode!(Load, 0x1));
                //Push addr of const
                code.push(self.const_vals.len() as u32);
                self.const_vals.push(Value::Number(n.val));
            },
            Ast::String(n) => {
                //Load string into 0x1
                code.push(opcode!(Load, 0x1));
                //PUsh addr
                code.push(self.const_vals.len() as u32);
                self.const_vals.push(Value::String(Box::new(n.val.clone())));
            },
            Ast::Function(n) => {

                //Declare function in current frame
                let var = self.vars.declare_var(&convert_node!(n.iden.as_ref().borrow().deref(), Identifier).token.lexeme);

                //Push a frame
                self.vars.push_frame();

                //Declare locals
                for arg in &n.args {
                    self.vars.declare_var(&convert_node!(arg.0.as_ref().borrow().deref(), Identifier).token.lexeme);
                }

                //Generate function body
                let mut fbody = vec![];
                self.gen_code_rec(n.body.as_ref().borrow().deref(), &mut fbody);

                //Pop frame
                self.vars.pop_frame();

                //Load function object into 0x1
                code.push(opcode!(Load, 0x1));
                code.push(self.const_vals.len() as u32);
                self.const_vals.push(Value::Function(Box::new(value::Function {code: RefCell::new(fbody)})));

                //Load store address into 0x2
                code.push(opcode!(LoadImm, 0x2));
                code.push(var.addr as u32);

                //Store function object in stack
                code.push(opcode!(SetLocVar, 0x2, 0x0, 0x1));

            },
            Ast::Return(n) => {
               self.gen_code_rec(n.elem.as_ref().borrow().deref(), code);
               code.push(opcode!(Return));
            },
            Ast::Print(n) => {
                //Save print val in 0x1
                self.gen_code_rec(n.node.as_ref().borrow().deref(), code);
                code.push(opcode!(Print, 0x1));
            },
        }
    }


    //Get stack address of a node and return its locality
    fn get_addr_and_locality(&mut self, ast: &Ast, code: &mut Vec<u32>) -> usize{
        use Instructions::*;

        match ast {
            Ast::Identifier(n) => {
                let var = self.vars.get_var(&n.token.lexeme);
                //Load var addr into 0x1
                code.push(opcode!(LoadImm, 0x1));
                code.push(var.addr as u32);
                var.frame
            }
            _ => panic!("Error in `get_addr_and_locality` function. (This is a compiler bug, please report.)"),
        }
    }

}