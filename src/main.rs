use std::env;
use std::error::Error;

use crate::ast::Ast;
use crate::ir::Inter;
use crate::lexer::Lexer;
use crate::parse::Parser;
use fs_err as fs;
use log::error;

pub mod ast;
mod gen;
mod ir;
pub mod lexer;
mod parse;
mod sem;

fn main() -> Result<(), Box<dyn Error>> {
    colog::init();
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        error!("Too few arguments! Please provide at minimum a file path: annex file.ax");
        return Err(Box::new(std::fmt::Error));
    }
    let file_path = &args[1];
    let file_contents = fs::read_to_string(file_path)?;

    let tokens = Lexer::new(file_contents)?;
    let parse_tree = Parser::new(tokens)?;
    let as_tree = Ast::new(parse_tree)?;
    match as_tree.sem_analysis() {
        Ok(()) => {},
        Err(errors) => {
            eprintln!("Semantic analysis failed with {} error(s):", errors.len());
            for error in errors {
                eprintln!("{}", error);
            }
            return Err("Semantic analysis failed".into());
        }
    }
    let _ir = Inter::new(as_tree)?;
    // assembly generation

    Ok(())
}
