use fs_err as fs;
use std::env;
use std::error::Error;

pub mod ast;
pub mod gen;
pub mod lexer;
mod parse;
pub mod util;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        eprintln!("ERR: Too few arguments! Please provide at minimum a file path: annex file.ax");
        std::process::exit(-1);
    }

    let file_path = &args[1];
    let file_contents = fs::read_to_string(file_path)?;
    let tokens = lexer::Lexer::new(file_contents)?;
    // let parse_tree = parse::ParseTree::new(tokens)?;
    // let syntax_tree = ast::Ast::new(parse_tree)?;
    // let asm = gen::Assembly::new(syntax_tree)?;
    // asm.print();
    // asm.to_file("file.s".to_string()).unwrap()
    Ok(())
}
