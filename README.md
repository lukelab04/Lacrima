<h1 align="center"> Lacrima </h1>
<p align = "center"> 
Lacrima is a simple, fast, embeddable scripting language built with Rust. Among other things, it has a complete type system, first-class functions, bytecode compilation, and a modular, extensible compiler.
</p>

## Getting Started
To use Lacrima in your Rust project, simply clone this project and include 
```
[dependencies]
lacrima = {path = [path_to_project]}
```
in your `Cargo.toml` file.

## Basic Lacrima Documentation
Lacrima is, by design, a very simple language. At the top level scope, only four things can be written.
- Variable Declarations 
- Function Declarations 
- Expressions
- Extern Statements

Each of these will be explored in more detail later on. 
Lacrima is a strongly-typed language, including a few basic built in types.
- Number
- String
- Boolean

Other types, like function and tuple types, are used implicitly, but as of now there is no way to declare variables of those types.
Lacrima also includes a few control-flow statements.
- If/Else
- While

Let's go through each of these in a little more detail.

### Variable Declaration
Variables are declared with the syntax `let [name] : [type]`. Optionally, `= [expression]` can be added to initialize the variable. 
Here is a simple example of variable declaration.
```
let a: num = 10;
let b: num = 100.3325;
let c: num = a + b;
```
### Function Declarations
Functions are declared with the syntax `fn [name] ([arg]: [type]) -> [type]`. 
Any number of arguments can be declared, and the return type is specified after the arrow. Note that this return type is not optional -- in cases of no return type, 
write `-> ()`, symbolizing returning the empty type.

Here are a few simple examples of Lacrima functions.
```
//A function taking a num and a string and returning nothing
fn foo(a: num, b: str) -> () {
  ...
}
```
```
//A function taking a number and returning it
fn foo(a: num) -> num {
  return a;
}
```
Recursion works as one would expect, with no recursion limit (other than hardware restrictions, obviously). 
### Expressions
An expression in Lacrima is pretty much anything that can evaluate to a value. This includes control flow, regular expressions like `a + b`, blocks, etc.
```
//This is an expression
a + 100.44;
```
```
//This is also an expression
{
  a;
}
```
Note that as of now, blocks and control flow can only evaluate to empty types. This is set to change soon, and otherwise they can be used just like any other expression.

### Extern Statements
An extern statement allows linking a function declared in Rust code to a function in Lacrima. The exact details of how to do this will be shown later, so as of now, this will do.
```
//Declare a function defined in Rust called `print`.
external print;
print(100.4423);
```
External functions take any number of arguments of any type and do not return anything. 
### Control Flow
As stated earlier, Lacrima has two control-flow structures, `if/else` and `while`.
```
//Simple if block.
if a == 1 {
  ...
} else if a == 2 {
  ...
} else {
  ...
};

//Simple while statement
while true {
  print("looping");
};
```
Notice the semicolon after the last curly brace. Because control flow blocks are expressions, they also require semicolons.
Also keep in mind that the expression to be evaluated by `if` and `while` must be a boolean -- writing `if 1 {...};` will give a type error.

### Linking external functions
Linking an external function is simple and straightforward. 
After initializing the `ExternalValues` struct, simply call `add_fn` with the name of your function and a reference to the function object itself. 
As an example,
```
fn print_hello(args: Vec<LCValue>) {
  println!("Hello");
}

fn main() {
  ...
  externs.add_fn("print_hello", print_hello);
  ...
}
```
After that, your function can be called from Lacrima simply by creating an extern statement and calling the function.

### Examples
Here is an example showing how to embed Lacrima.
```
use lacrima;

pub fn print(args: Vec<lacrima::LCValue>) {
  use lacrima::LCValue;
  for arg in args {
    match arg {
      LCValue::Empty => {}
      LCValue::Number(n) => println!("{}", n),
      LCValue::Boolean(n) => println!("{}", n),
      LCValue::String(n) => println!("{}", n),
      LCValue::Function(_) => println!("Function"),
    }
  }
}

fn main() {
  let mut externs = lacrima::ExternalFunctions::new();
  externs.add_fn("print", print);
  
  let bc = match lacrima::compile("external print; print(\"Hello World\");") {
    Ok(t) => t,
    Err(s) => panic!("{}", s);
  };
  let mut vm = lacrima::init_vm();
  lacrima::run(&mut vm, &bc, &externs);
}
```
This is a complete hello world example.
