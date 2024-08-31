use std::env;

pub mod syntax;
pub mod token;
mod util;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        eprintln!("ERR: Too few arguments! Please provide at minimum a file path: annex file.ax");
        std::process::exit(-1);
    }

    let file_path = &args[1];
    let file_contents = util::get_file_contents(file_path);
    let token_vec = token::Lexer::new(file_contents);
    token_vec.print_all();
    let syntax_tree = syntax::Ast::new(token_vec);
}
