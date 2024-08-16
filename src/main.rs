use std::{env, fs};
use std::error::Error;

pub mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        eprintln!("ERR: Too few arguments! Please provide at minimum a file path: annex file.c");
        std::process::exit(1);
    }

    let file_path = &args[1];
    let file_contents: String = fs::read_to_string(file_path)?;
    let token_vec = token::tokenise(file_contents);

    Ok(())
}
