use annex::lexer::TokenStream;
use fs_err as fs;

/// Lexical analysis test logic.
fn verify_tokens(source: String, expected_tokens: Vec<&str>, test_name: &str) {
    let tokens = TokenStream::new(source);
    assert!(
        tokens.is_ok(),
        "Lexical analysis failed for test: {}",
        test_name
    );
    let tokens = tokens.unwrap().to_string();
    let actual_tokens = tokens.lines().collect::<Vec<_>>();

    assert_eq!(
        actual_tokens, expected_tokens,
        "Test: {}\nDifference in tokens\nExpected:\n{:#?}\nActual:\n{:#?}",
        test_name, expected_tokens, actual_tokens
    );
}

#[test]
/// Tests a simple arithmetic expression.
/// Also checks for handling of floating-point numbers.
fn test_simple_arithmetic() {
    let test_name = "test_simple_arithmetic";
    let source = "id + 3.555 * 10;".to_string();
    let expected_tokens = vec![
        "identifier: id",
        "operator: +",
        "integer: 3", // checks how lexer handles floating point numbers
        "separator: .",
        "integer: 555",
        "operator: *",
        "integer: 10",
        "separator: ;",
        "eof",
    ];

    verify_tokens(source, expected_tokens, test_name);
}

#[test]
/// Tests an arithmetic expression closer to how the language works.
/// Also checks that bitshift (<<) and comparison (<) operators are not confused.
fn test_more_simple_arithmetic() {
    let test_name = "test_more_simple_arithmetic";
    let source = "var bool x = (y >> 2) < 2;".to_string();
    let expected_tokens = vec![
        "var",
        "type: bool",
        "identifier: x",
        "operator: =",
        "group begin: (",
        "identifier: y",
        "operator: >>",
        "integer: 2",
        "group end: )",
        "operator: <",
        "integer: 2",
        "separator: ;",
        "eof",
    ];

    verify_tokens(source, expected_tokens, test_name);
}

#[test]
/// Tests control flow statements.
fn test_control_loops() {
    let test_name = "test_control_loops";

    let source_path = "tests/files/max_i32.ax";
    let source = fs::read_to_string(source_path);
    assert!(source.is_ok(), "Failed to read file: {}", source_path);
    let source = source.unwrap();

    let expected_path = "tests/expected/lexer/max_i32.txt";
    let expected = fs::read_to_string(expected_path);
    assert!(expected.is_ok(), "Failed to read file: {}", expected_path);
    let expected = expected.unwrap();
    let expected_tokens = expected.lines().collect::<Vec<_>>();

    verify_tokens(source, expected_tokens, test_name);
}

#[test]
/// Tests every other keyword in the language.
fn test_other_keywords() {
    let test_name = "test_other_keywords";
    let source = "const vol fn return if elif else for while continue break;".to_string();
    let expected_tokens = vec![
        "const",
        "vol",
        "fn",
        "return",
        "if",
        "elif",
        "else",
        "for",
        "while",
        "continue",
        "break",
        "separator: ;",
        "eof",
    ];

    verify_tokens(source, expected_tokens, test_name);
}
