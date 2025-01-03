use std::env;
use std::error::Error;

use fs_err as fs;

pub mod ast;
pub mod gen;
pub mod lexer;
mod parse;
pub mod util;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        panic!("Too few arguments! Please provide at minimum a file path: annex file.ax");
    }
    colog::init();
    let file_path = &args[1];
    let file_contents = fs::read_to_string(file_path)?;
    let tokens = lexer::Lexer::new(file_contents)?;
    let parse_tree = parse::Parser::new(tokens)?;
    let abstract_syntax_tree = ast::Ast::new(parse_tree)?;
    let asm = gen::Assembly::new(syntax_tree)?;
    asm.to_file("file.s".to_string())?;
    Ok(())
}
