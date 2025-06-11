use crate::ast::{Ast, types::AstNode};
use crate::sem::err::SemError;
use crate::sem::sym_table::{FuncEntry, SymTable};

mod err;
pub mod sym_table;
mod analyzer;

pub use err::{SemErrorKind, Location};
pub use analyzer::SemanticAnalyzer;

impl Ast {
    pub fn sem_analysis(&self) -> Result<(), Vec<SemError>> {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(self)
    }
    
    /// Legacy function table filling for compatibility
    pub fn fill_func_table(&self) -> Result<SymTable<FuncEntry>, SemError> {
        fill_func_table(self)
    }
}

/// Fills the global function symbol table.
/// Also performs function declaration-related semantic checks.
fn fill_func_table(tree: &Ast) -> Result<SymTable<FuncEntry>, SemError> {
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
                    return Err(SemError::identifier_redefined(
                        crate::sem::err::Location::unknown(),
                        func.name.clone(),
                    ));
                } else {
                    let entry = FuncEntry::new(func.return_type.clone(), func.params.clone());
                    func_table.insert(func.name.clone(), entry);
                }
            }
            Ok(func_table)
        }
        _ => Err(SemError::internal_error(
            crate::sem::err::Location::unknown(),
            "Mismatched abstract syntax tree head node type".to_string(),
        )),
    }
}
