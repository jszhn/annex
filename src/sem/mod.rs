// use std::collections::HashMap;

use crate::ast::{types::*, Ast};
use crate::sem::err::SemError;
use crate::sem::table::{FuncEntry, FuncTable, ScopedSymTable, SymTable};

mod err;
pub mod table;
mod visitor;

/// Contains semantic analysis methods for the AST.
impl Ast {
    pub fn sem_analysis(&self) -> Result<(), Vec<SemError>> {
        let mut analyser = SemanticAnalyser::new();
        analyser.start(self)
    }
}

/// State object for semantic analysis.
struct SemanticAnalyser {
    errors: Vec<SemError>,
    functions: FuncTable,
    scoped_vars: ScopedSymTable,
    curr_func: Option<FunctionNode>, // todo: should this be a reference?
                                     // expr_types: HashMap<*const AstNode, Type>, // currently unused
}

impl SemanticAnalyser {
    fn new() -> Self {
        Self {
            errors: Vec::new(),
            functions: SymTable::new(0),
            scoped_vars: ScopedSymTable::new(),
            curr_func: None,
            // expr_types: HashMap::new(),
        }
    }

    /// Starts the semantic analysis process on the given AST.
    fn start(&mut self, ast: &Ast) -> Result<(), Vec<SemError>> {
        let result = self.visit_program(ast.get_head_ref());
        if let Err(err) = result {
            self.errors.push(err);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(Default::default())
        }
    }

    fn add_err(&mut self, err: SemError) {
        self.errors.push(err);
    }

    fn visit_program(&mut self, node: &AstNode) -> Result<(), SemError> {
        match node {
            AstNode::Block(program) => {
                self.build_func_table(program)?;

                // second pass for variable/function analysis
                for elem in program.get_elems_ref() {
                    self.visit(elem)?;
                }

                Ok(())
            }
            _ => Err(SemError::internal_error("expected AST BlockNode")), // fatal error
        }
    }

    /// This is a single pass over the program's block node to build the function table.
    fn build_func_table(&mut self, program: &BlockNode) -> Result<(), SemError> {
        program
            .get_elems_ref()
            .iter()
            .filter_map(|node| {
                // filter out non-function nodes
                if let AstNode::Function(func) = node {
                    Some(func)
                } else {
                    None
                }
            })
            .for_each(|func| {
                // check for redefinitions
                if let Some(entry) = self.functions.lookup_mut(&func.name) {
                    entry.mark();
                    self.add_err(SemError::id_redefined(func.name.clone()));
                } else {
                    self.functions.insert(
                        func.name.clone(),
                        FuncEntry::new(func.params.clone(), func.return_type.clone()),
                    );
                }
            });
        Ok(())
    }
}
