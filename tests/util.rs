#[path = "../src/util/mod.rs"]
mod util;

#[test]
fn load_file() {
    let files: Vec<String> = vec!["tests/files/arithmetic.ax".to_string()];

    for file in files {
        assert_eq!(util::get_file_contents(&file), "8 + 3 * 10;".to_string())
    }
}
