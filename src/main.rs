use std::env;
use std::process::exit;

use annex::ast::Ast;
use annex::lexer::TokenStream;
use annex::parse::ParseTree;

use current_platform::COMPILED_ON;
use fs_err as fs;
use log::{error, info};

fn main() {
    colog::init();
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => version(),
        _ => compile(args),
    }
}

fn version() {
    let rustc = rustc_version_runtime::version();
    let rustc_version = format!("v{}.{}.{}", rustc.major, rustc.minor, rustc.patch);

    info!("{}c v{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
    info!("built on {COMPILED_ON} with rustc {rustc_version}");
    info!("usage: {}c file.ax", env!("CARGO_PKG_NAME"));
}

fn compile(args: Vec<String>) {
    let file_path = &args[1];
    let file_contents = fs::read_to_string(file_path);
    match file_contents {
        Ok(_) => {}
        Err(e) => {
            error!("The compiler has encountered an error while reading the file...\n\t{e}");
            exit(1);
        }
    };
    let file_contents = file_contents.unwrap();

    let tokens = TokenStream::new(file_contents);
    match tokens {
        Ok(_) => {}
        Err(e) => {
            error!("The compiler has encountered an error while lexing the file...\n\t{e}");
            exit(1);
        }
    }
    let tokens = tokens.unwrap();

    let parse_tree = ParseTree::new(tokens);
    match parse_tree {
        Ok(_) => {}
        Err(e) => {
            error!("The compiler has encountered an error while parsing the file...\n\t{e}");
            exit(1);
        }
    }
    let parse_tree = parse_tree.unwrap();

    let as_tree = Ast::new(parse_tree);
    match as_tree {
        Ok(_) => {}
        Err(e) => {
            error!("The compiler has encountered an error while generating the AST...\n\t{e}");
            exit(1);
        }
    }
    let as_tree = as_tree.unwrap();

    let sem_result = as_tree.sem_analysis();
    match sem_result {
        Ok(_) => {}
        Err(e) => {
            error!("The compiler has encountered an error while performing semantic analysis...\n\t{e}");
            exit(1);
        }
    }
}
