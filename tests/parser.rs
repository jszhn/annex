use annex::lexer::TokenStream;
use annex::parse::ParseTree;
use fs_err as fs;

fn check_correct_parsing(input_path: &str, expected_path: &str, test_name: &str) {
    let input_file = fs::read_to_string(input_path);
    let expected_file = fs::read_to_string(expected_path);
    assert!(
        input_file.is_ok() && expected_file.is_ok(),
        "Failed to read input or expected file"
    );
    let input_file = input_file.unwrap();
    let expected_file = expected_file.unwrap();

    let tokens = TokenStream::new(input_file);
    assert!(tokens.is_ok(), "Failed to tokenize input file");
    let tokens = tokens.unwrap();

    let parse_tree = ParseTree::new(tokens);
    if parse_tree.is_err() {
        eprintln!("Error parsing input file: {:?}", parse_tree.as_ref().err());
    }
    assert!(parse_tree.is_ok(), "Failed to parse input file");
    let parse_tree = parse_tree.unwrap();

    let output = parse_tree.print(false);
    let actual_lines: Vec<&str> = output
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    let expected_lines: Vec<&str> = expected_file
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();

    assert_eq!(
        actual_lines, expected_lines,
        "Test: {test_name}\nDifference in parse tree\nExpected:\n{expected_lines:#?}\nActual:\n{actual_lines:#?}",
    );
}

#[test]
/// This test checks the parsing of multiple symbol declarations and a simple function.
fn test_symbol_decls() {
    let test_name = "test_symbol_decls";
    let input_path = "tests/files/process_array.ax";
    let expected_path = "tests/expected/parse/process_array.txt";

    check_correct_parsing(input_path, expected_path, test_name);
}

#[test]
/// This test checks the parsing of a single function with a for loop and arithmetic.
fn test_function_loop() {
    let test_name = "test_function_loop";
    let input_path = "tests/files/more_arithmetic.ax";
    let expected_path = "tests/expected/parse/more_arithmetic.txt";

    check_correct_parsing(input_path, expected_path, test_name);
}

#[test]
/// This test checks a simple if statement.
fn test_max() {
    let test_name = "test_max";
    let input_path = "tests/files/max_i32.ax";
    let expected_path = "tests/expected/parse/max_i32.txt";

    check_correct_parsing(input_path, expected_path, test_name);
}

#[test]
/// This test checks for Pratt parsing of complex expressions.
fn test_expr_parsing() {
    let test_name = "test_expr_parsing";
    let input_path = "tests/files/many_expr.ax";
    let expected_path = "tests/expected/parse/many_expr.txt";

    check_correct_parsing(input_path, expected_path, test_name);
}

#[test]
/// This test checks for more Pratt parsing of complex expressions, including
/// unary operators and precedence.
fn test_expr_parsing2() {
    let test_name = "test_expr_parsing2";
    let input_path = "tests/files/many_expr2.ax";
    let expected_path = "tests/expected/parse/many_expr2.txt";

    check_correct_parsing(input_path, expected_path, test_name);
}

#[test]
/// This test ensures expressions with parentheses are parsed correctly.
fn test_expr_parsing3() {
    let test_name = "test_expr_parsing3";
    let input_path = "tests/files/many_expr3.ax";
    let expected_path = "tests/expected/parse/many_expr2.txt";

    check_correct_parsing(input_path, expected_path, test_name);
}
