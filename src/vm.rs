use crate::{convert, ExternalFunctions};
use crate::value::Value;

pub struct Vm {
    regs: [Value; 8],
    value_stack: Vec<Value>,
    //Frame stack points to first element of frame
    frame_stack: Vec<usize>,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            regs: [Value::Empty, Value::Empty, Value::Empty, Value::Empty,Value::Empty, Value::Empty, Value::Empty, Value::Empty],
            value_stack: vec![],
            frame_stack: vec![0],
        }
    }

    fn push_frame(&mut self, addr: usize) {
        self.frame_stack.push(addr)}
    fn pop_frame(&mut self) -> usize {self.frame_stack.pop().unwrap()}

    fn next(&mut self, ins: &Vec<u32>, pc: &mut usize) -> u32 {
        *pc += 1;
        ins[*pc]
    }

    fn push(&mut self, val: Value) {
        self.value_stack.push(val);
    }

    fn pop(&mut self) -> Value {
        self.value_stack.pop().unwrap()
    }

    fn set_global(&mut self, addr: usize, val: Value) {
        if addr >= self.value_stack.len() {
            self.value_stack.push(Value::Empty);
        }
        self.value_stack[addr] = val;
    }

    fn set_local(&mut self, addr: usize, val: Value) {
        if addr >= self.value_stack.len() {
            self.value_stack.push(Value::Empty);
        }
        self.value_stack[addr + self.frame_stack.last().unwrap()] = val;
    }

    fn get_global(&mut self, addr: usize) -> Value {
        self.value_stack[addr].clone()
    }

    fn get_local(&mut self, addr: usize) -> Value {
        self.value_stack[addr + self.frame_stack.last().unwrap()].clone()
    }

    pub fn run(&mut self, ins: &Vec<u32>, const_vals: &Vec<Value>, externs: &ExternalFunctions) {
        let mut pc: usize = 0;


        while pc < ins.len() {
            let op = (ins[pc] >> 24) as u8;
            let x = ((ins[pc] >> 16) as u8) as usize;
            let y = ((ins[pc] >> 8) as u8) as usize;
            let z = (ins[pc] as u8) as usize;


            match op {

                //Mov
                0x0 => self.regs[x] = self.regs[z].clone(),
                //Load
                0x1 => self.regs[x] = const_vals[self.next(&ins, &mut pc) as usize].clone(),
                //LoadImm
                0x2 => self.regs[x] = Value::Number(self.next(&ins, &mut pc) as f64),


                //Add
                0x10 => self.regs[x] = self.regs[y].add(&self.regs[z]),
                //Sub
                0x11 => self.regs[x] = self.regs[y].sub(&self.regs[z]),
                //Div
                0x12 => self.regs[x] = self.regs[y].div(&self.regs[z]),
                //Mul
                0x13 => self.regs[x] = self.regs[y].mul(&self.regs[z]),
                //Mod
                0x14 => self.regs[x] = self.regs[y].rem(&self.regs[z]),
                //And
                0x15 => self.regs[x] = self.regs[y].and(&self.regs[z]),
                //Or
                0x16 => self.regs[x] = self.regs[y].or(&self.regs[z]),
                //Eq
                0x17 => self.regs[x] = self.regs[y].eq(&self.regs[z]),
                //Neq
                0x18 => self.regs[x] = self.regs[y].neq(&self.regs[z]),
                //Gt
                0x19 => self.regs[x] = self.regs[y].gt(&self.regs[z]),
                //Gte
                0x1A => self.regs[x] = self.regs[y].gte(&self.regs[z]),
                //Lt
                0x1B => self.regs[x] = self.regs[y].lt(&self.regs[z]),
                //Lte
                0x1C => self.regs[x] = self.regs[y].lte(&self.regs[z]),
                //Not
                0x1D => self.regs[x] = self.regs[z].not(),
                //Negate
                0x1E => self.regs[x] = self.regs[z].neg(),


                //JZ
                0x32 => {
                    if convert!(self.regs[z], Boolean) == false {
                        pc = convert!(self.regs[x], Number) as usize;
                        continue;
                    }
                },
                //Jump
                0x34 => {
                    pc = convert!(self.regs[x], Number) as usize;
                    continue;
                }
                //Call
                0x35 => {
                    let num = &self.regs[x];
                    let func = &self.value_stack[*convert!(num, Number) as usize];
                    //Get function at absolute address
                    let func_code = convert!(func, Function).code.clone();
                    self.run(func_code.borrow().as_ref(), const_vals, externs);
                }
                //Return
                0x36 => {
                    return;
                }
                //ExternCall
                0x37 => {
                    let mut args = vec![];
                    let arg_len = convert!(self.pop(), Number) as usize;
                    for _ in 0..arg_len {
                        args.push(self.pop());
                    }
                    args.reverse();
                    let fname = convert!(&self.regs[x], String).as_ref().clone();
                    externs.functions[&fname](args);
                }


                //Push
                0x40 => self.push(self.regs[x].clone()),
                //Pop
                0x41 => self.regs[x] = self.pop(),
                //PushFrame
                0x42 => self.push_frame(convert!(self.regs[x], Number) as usize),
                //PopFrame
                0x43 => self.regs[x] = Value::Number(self.pop_frame() as f64),
                //SetLocalVar
                0x44 => self.set_local(convert!(self.regs[x], Number) as usize, self.regs[z].clone()),
                //GetLocalVar
                0x45 => self.regs[x] = self.get_local(convert!(self.regs[z], Number) as usize),
                //SetGlobalVar
                0x46 => self.set_global(convert!(self.regs[x], Number) as usize, self.regs[z].clone()),
                //GetGlobalVar
                0x47 => self.regs[x] = self.get_global(convert!(self.regs[z], Number) as usize),
                //GetStackLen
                0x48 => self.regs[x] = Value::Number(self.value_stack.len() as f64),
                //SetStackLen
                0x49 => {
                    self.value_stack.drain((convert!(self.regs[x], Number) as usize)..);
                },


                //NoOp
                0xF0 => (),
                //Print
                0xF1 => {
                    match &self.regs[x] {
                        Value::Empty => println!("Empty"),
                        Value::Number(n) => println!("{}", n),
                        Value::Boolean(n) => println!("{}", n),
                        Value::String(n) => println!("{}", n),
                        Value::Function(_n) => println!("Function"),
                    }
                }

                _ => panic!("Unknown opcode {}. (This is a VM bug, please report.)", op),
            }


            pc += 1;
        }

    }
}