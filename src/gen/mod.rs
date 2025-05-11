// use crate::ast::Ast;
// use crate::gen::util::AsmError;
//
// mod asm;
// mod util;
//
// pub struct Assembly {
//     value: String,
// }
//
// impl Assembly {
//     pub fn new(tree: Ast, emit: bool) -> Result<Assembly, AsmError> {}
// }
//
// //
// // pub struct Assembly {
// //     value: String,
// // }
// //
// // impl Assembly {
// //     pub fn new(tree: Ast) -> Result<Assembly, AsmError> {
// //         generate(tree)
// //     }
// //
// //     pub fn new_str(input: String) -> Assembly {
// //         return Assembly { value: input };
// //     }
// //
// //     pub fn to_file(&self, path: String) -> std::io::Result<()> {
// //         let mut file = fs::File::create(path)?;
// //         file.write_all(self.value.as_str().as_bytes())?;
// //         Ok(())
// //     }
// //
// //     pub fn print(&self) {
// //         println!("{}", self.value);
// //     }
// // }
// //
// // fn generate(tree: Ast) -> Result<Assembly, AsmError> {
// //     let head = tree.get_head_ref();
// //     let mut file = String::new();
// //     asm::file_start(&mut file);
// //
// //     if let ExprType::Cons(val, vec) = &head.expr_type {
// //         if val == "return" {
// //             _ = recurse(&vec[0], &mut file);
// //             return Ok(Assembly::new_str(file));
// //         }
// //     }
// //     eprint!("ERR: not yet supported. sorry!");
// //     Err(AsmError::new(
// //         "Err in code generation: not yet supported. Sorry!",
// //     ))
// // }
// //
// // fn recurse(node: &ExprType, file: &mut String) -> i32 {
// //     if let ExprType::Atom(val) = node {
// //         let (instr, reg) = asm::instr::load_imm(val.parse::<i32>().unwrap());
// //         file.push_str(instr.as_str());
// //         return reg as i32;
// //     } else if let ExprType::Cons(val, vec) = node {
// //         let left = recurse(&vec[0], file);
// //         let right = if vec.len() > 1 {
// //             recurse(&vec[1], file)
// //         } else {
// //             0
// //         };
// //         file.push_str(
// //             match val.as_str() {
// //                 "+" => asm::instr::add(left, right),
// //                 "-" => asm::instr::sub(left, right),
// //                 "*" => asm::instr::mul(left, right),
// //                 "/" => asm::instr::mul(left, right),
// //                 _ => {
// //                     println!("ERR: operator not yet supported. Yikes!");
// //                     std::process::exit(-1);
// //                 }
// //             }
// //             .as_str(),
// //         );
// //         return left;
// //     }
// //     0
// // }
