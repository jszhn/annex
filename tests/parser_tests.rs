use std::error::Error;
use std::iter::zip;

use annex::lexer::Lexer;
use annex::parse::Parser;

#[test]
fn parse_test() -> Result<(), Box<dyn Error>> {
    let input_paths = vec![
        "tests/files/process_array.ax",
        "tests/files/more_arithmetic.ax",
    ];
    let expected_paths = vec![
        "tests/files/process_array.expected",
        "tests/files/more_arithmetic_parse.expected",
    ];

    for (input, expected) in zip(input_paths, expected_paths) {
        test_parse_file(input, expected)?;
    }
    Ok(())
}

fn test_parse_file(input_path: &str, expected_path: &str) -> Result<(), Box<dyn Error>> {
    let input_file = std::fs::read_to_string(input_path)?;
    let expected_file = std::fs::read_to_string(expected_path)?;

    let tokens = Lexer::new(input_file)?;
    let parse_tree = Parser::new(tokens)?;

    let output = parse_tree.print(false);
    let actual_lines: Vec<&str> = output
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    let expected_lines: Vec<&str> = expected_file
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();

    assert_eq!(actual_lines, expected_lines);

    Ok(())
}