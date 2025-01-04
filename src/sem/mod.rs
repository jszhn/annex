use crate::ast::{Ast, AstNode};
use crate::sem::err::SemError;
use crate::sem::sym_table::{FuncEntry, SymTable};

mod err;
pub mod sym_table;

impl Ast {
    pub fn sem_analysis(&self) -> Result<(), SemError> {
        fill_func_table(self)
    }
}

/// Fills the global function symbol table.
/// Also performs function declaration-related semantic checks.
fn fill_func_table(tree: &Ast) -> Result<(), SemError> {
    let head = tree.get_head_ref();
    match head {
        AstNode::Block(prog) => {
            let mut func_table: SymTable<FuncEntry> = SymTable::new(0);
            let vec = prog.get_elems_ref();
            for func in vec.iter().filter_map(|node| {
                if let AstNode::Function(func) = node {
                    Some(func)
                } else {
                    None
                }
            }) {
                if func_table.contains(&func.name) {
                    return Err(SemError::RedefFunction(func.name.clone()));
                } else {
                    let entry = FuncEntry::new(func.return_type.clone(), &func.params);
                    func_table.insert(func.name.clone(), entry);
                }
            }
            Ok(())
        }
        _ => Err(SemError::InternalError(
            "Mismatched abstract syntax tree head node type".to_string(),
        )),
    }
}
