use crate::syntax::{Ast, ExprType};
use std::fmt;

mod asm;

pub struct Assembly {
    value: String,
}

impl Assembly {
    pub fn new(tree: Ast) -> Assembly {
        generate(tree)
    }

    pub fn new_str(input: String) -> Assembly {
        return Assembly { value: input };
    }

    pub fn to_file(&self, path: String) {
        todo!()
    }

    pub fn print(&self) {
        println!("{}", self.value);
    }
}

fn generate(tree: Ast) -> Assembly {
    let head = tree.get_head_ref();
    let mut file = String::new();
    asm::file_start(&mut file);

    if let ExprType::Cons(val, vec) = &head.expr_type {
        if val == "return" {
            _ = recurse(&vec[0], &mut file);
            return Assembly::new_str(file);
        }
    }
    eprint!("ERR: not yet supported. sorry!");
    return Assembly::new_str("".to_string());
}

fn recurse(node: &ExprType, file: &mut String) -> i32 {
    if let ExprType::Atom(val) = node {
        let (instr, reg) = asm::instr::load_imm(val.parse::<i32>().unwrap());
        file.push_str(instr.as_str());
        return reg as i32;
    } else if let ExprType::Cons(val, vec) = node {
        let left = recurse(&vec[0], file);
        let right = if vec.len() > 1 {
            recurse(&vec[1], file)
        } else {
            0
        };
        file.push_str(
            match val.as_str() {
                "+" => asm::instr::add(left, right),
                "-" => asm::instr::sub(left, right),
                "*" => asm::instr::mul(left, right),
                "/" => asm::instr::mul(left, right),
                _ => {
                    println!("ERR: operator not yet supported. Yikes!");
                    std::process::exit(-1);
                }
            }
            .as_str(),
        );
        return left;
    }
    0
}
