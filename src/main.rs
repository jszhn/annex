use std::env;
use std::error::Error;

use fs_err as fs;
use log::error;

pub mod ast;
mod gen;
mod ir;
pub mod lexer;
mod parse;
mod sem;
pub mod util;

fn main() -> Result<(), Box<dyn Error>> {
    colog::init();
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        error!("Too few arguments! Please provide at minimum a file path: annex file.ax");
        return Err(Box::new(std::fmt::Error));
    }
    let file_path = &args[1];
    let file_contents = fs::read_to_string(file_path)?;
    let tokens = lexer::Lexer::new(file_contents)?;
    let parse_tree = parse::Parser::new(tokens)?;
    let as_tree = ast::Ast::new(parse_tree)?;
    as_tree.sem_analysis()?;
    let ir = ir::Inter::new(as_tree)?;
    // let asm = gen::Assembly::new(abstract_syntax_tree)?;
    Ok(())
}
