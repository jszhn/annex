use std::env;
use std::error::Error;
use std::process::exit;

use annex::ast::Ast;
use annex::lexer::TokenStream;
use annex::parse::ParseTree;

use fs_err as fs;
use log::error;

fn main() -> Result<(), Box<dyn Error>> {
    colog::init();
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        error!("Too few arguments! Please provide at minimum a file path: annex file.ax");
        return Err(Box::new(std::fmt::Error));
    }
    let file_path = &args[1];
    let file_contents = fs::read_to_string(file_path)?;
    let tokens = TokenStream::new(file_contents)?;

    let parse_tree = ParseTree::new(tokens);
    match parse_tree {
        Ok(_) => {}
        Err(e) => {
            error!("The compiler has encountered an error while parsing the file...\n\t{e}");
            exit(1);
        }
    }
    let parse_tree = parse_tree?;

    let as_tree = Ast::new(parse_tree)?;
    as_tree.sem_analysis()?;

    Ok(())
}
