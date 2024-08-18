use crate::token::CToken;
use crate::token::types;

pub fn print(vec: &Vec<CToken>) {
    for item in vec {
        println!("{}", item.value);
    }
}