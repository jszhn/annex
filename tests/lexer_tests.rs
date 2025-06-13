// #[test]
// fn basic_arithmetic_test() {
//     let files: Vec<&str> = vec!["tests/files/arithmetic.ax"];
//     let tokens: Vec<Vec<Token>> = vec![vec![
//         // todo: tidy up
//         Token::new_blank(TokenType::EOF),
//         Token::new_lexeme(TokenType::Separator, ";"),
//         Token::new_lexeme(TokenType::Integer, "555"),
//         Token::new_lexeme(TokenType::Separator, "."),
//         Token::new_lexeme(TokenType::Identifier, "3"),
//         Token::new_lexeme(TokenType::Operator, "*"),
//         Token::new_lexeme(TokenType::Identifier, "10"),
//         Token::new_lexeme(TokenType::Operator, "+"),
//         Token::new_lexeme(TokenType::Identifier, "id"),
//     ]];
//
//     for (i, file) in files.iter().enumerate() {
//         let contents = std::fs::read_to_string(file).unwrap();
//         let mut lex = TokenStream::new(contents).expect("err: lexical analysis failed!");
//         lex.print_all();
//         let test_tokens = lex.get_ref();
//         for j in 0..tokens.len() {
//             assert_eq!(tokens[i][j].typ, test_tokens[j].typ);
//             assert_eq!(tokens[i][j].lexeme, test_tokens[j].lexeme);
//         }
//     }
// }
