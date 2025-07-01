use annex::lexer::TokenStream;
use annex::parse::ParseTree;

fn check_correct_parsing(input_path: &str, expected_path: &str, test_name: &str) {
    let input_file = std::fs::read_to_string(input_path);
    let expected_file = std::fs::read_to_string(expected_path);
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
        "Test: {}\nDifference in parse tree\nExpected:\n{:#?}\nActual:\n{:#?}",
        test_name, expected_lines, actual_lines
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
