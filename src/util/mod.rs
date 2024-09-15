use std::fs;

pub fn get_file_contents(path: &String) -> String {
    /*
        Reads a file path, returns the file's contents as a string
    */
    let file_contents = fs::read_to_string(path);
    return if let Ok(file_contents) = file_contents {
        file_contents
    } else {
        "".to_string()
    };
}

pub trait OutputHandler {
    fn to_txt(&self, path: String) -> bool;
    fn from_txt(path: String);
}
