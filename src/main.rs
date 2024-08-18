use std::{env, fs};

pub mod token;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        eprintln!("ERR: Too few arguments! Please provide at minimum a file path: annex file.c");
        std::process::exit(1);
    }

    let file_path = &args[1];
    let file_contents = get_file_contents(file_path);
    let token_vec = token::tokenise(file_contents);
    token::io::print(&token_vec);
}

fn get_file_contents(path: &String) -> String {
    let file_contents = fs::read_to_string(path);
    return if let Ok(file_contents) = file_contents {
        file_contents
    } else {
        "".to_string()
    };
}
