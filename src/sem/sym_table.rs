use std::collections::HashMap;

use crate::ast::types::{ParamNode, Type};

/// FIFO stack-based symbol table implementation, for entering and exiting program scopes.
pub struct ScopedSymTable {
    scopes: Vec<SymTable<VarEntry>>,
}

impl ScopedSymTable {
    fn new() -> ScopedSymTable {
        let mut scopes = Vec::new();
        scopes.push(SymTable::new(0));
        ScopedSymTable { scopes }
    }
}

/// Symbol table struct type. Wraps around a hash table.
pub struct SymTable<T> {
    table: HashMap<String, T>,
    pub scope_level: u32,
}

impl<T> SymTable<T> {
    pub fn new(scope_level: u32) -> Self {
        SymTable {
            table: HashMap::new(),
            scope_level,
        }
    }

    pub fn get_ref(&self, key: String) -> Option<&T> {
        self.table.get(&key)
    }

    pub fn contains(&self, key: &String) -> bool {
        let result = self.table.get(key);
        result.is_some()
    }

    pub fn insert(&mut self, key: String, value: T) -> Option<T> {
        self.table.insert(key, value)
    }
}

pub struct VarEntry {
    typ: Type,
    is_array: bool,
}

impl VarEntry {
    pub fn new(typ: Type, is_array: bool) -> VarEntry {
        VarEntry { typ, is_array }
    }
}

pub struct FuncEntry<'a> {
    return_type: Type,
    params: &'a Vec<ParamNode>,
}

impl<'a> FuncEntry<'a> {
    pub fn new(return_type: Type, params: &'a Vec<ParamNode>) -> FuncEntry<'a> {
        FuncEntry {
            return_type,
            params,
        }
    }
}
