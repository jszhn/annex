use annex::lexer::{Lexer, Token, TokenType};

#[test]
fn basic_lexer_test() {
    let files: Vec<&str> = vec!["tests/files/arithmetic.ax"];
    let tokens: Vec<Vec<Token>> = vec![vec![
        Token::new_blank(TokenType::EOF),
        Token::new(TokenType::Separator, ";"),
        Token::new(TokenType::Integer, "555"),
        Token::new(TokenType::Separator, "."),
        Token::new(TokenType::Identifier, "3"),
        Token::new(TokenType::Operator, "*"),
        Token::new(TokenType::Identifier, "10"),
        Token::new(TokenType::Operator, "+"),
        Token::new(TokenType::Identifier, "id"),
    ]];

    for (i, file) in files.iter().enumerate() {
        let contents = std::fs::read_to_string(file).unwrap();
        let mut lex = Lexer::new(contents).expect("err: lexical analysis failed!");
        lex.print_all();
        let test_tokens = lex.get_ref();
        for j in 0..tokens.len() {
            assert_eq!(tokens[i][j].token_type, test_tokens[j].token_type);
            assert_eq!(tokens[i][j].lexeme, test_tokens[j].lexeme);
        }
    }
}