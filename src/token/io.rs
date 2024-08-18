use crate::token::CToken;

pub fn print(vec: &Vec<CToken>) {
    for item in vec {
        println!("{}", item.value);
    }
}