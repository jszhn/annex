use std::collections::HashMap;

use crate::ast::types::{ParamNode, Type};

/// FIFO stack-based symbol table implementation, for entering and exiting program scopes.
#[allow(dead_code)]
pub struct ScopedSymTable {
    pub scopes: Vec<SymTable<VarEntry>>,
    num_scopes: u32,
}

impl ScopedSymTable {
    #[allow(dead_code)]
    pub fn new() -> ScopedSymTable {
        let mut scopes = Vec::new();
        scopes.push(SymTable::new(0));
        ScopedSymTable {
            scopes,
            num_scopes: 1,
        }
    }

    /// Enters new scope, with new symbol table.
    #[allow(dead_code)]
    pub fn enter(&mut self) {
        self.scopes.push(SymTable::new(self.num_scopes + 1));
        self.num_scopes += 1;
    }

    /// Exits current scope. Discards current symbol table.
    #[allow(dead_code)]
    pub fn exit(&mut self) {
        if self.num_scopes > 1 {
            self.scopes.pop();
            self.num_scopes -= 1;
        }
    }

    #[allow(dead_code)]
    pub fn lookup(&self, key: &String) -> Option<&VarEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get_ref(key) {
                return Some(entry);
            }
        }
        None
    }

    /// Looks-up the given key in the current scope's symbol table.
    #[allow(dead_code)]
    pub fn lookup_current(&self, key: &String) -> Option<&VarEntry> {
        let scope = self.scopes.last()?;
        if let Some(entry) = scope.get_ref(key) {
            return Some(entry);
        }
        None
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

    #[allow(dead_code)]
    pub fn get_ref(&self, key: &String) -> Option<&T> {
        self.table.get(key)
    }

    pub fn contains(&self, key: &String) -> bool {
        let result = self.table.get(key);
        result.is_some()
    }

    pub fn insert(&mut self, key: String, value: T) -> Option<T> {
        self.table.insert(key, value)
    }
}

#[allow(dead_code)]
pub struct VarEntry {
    typ: Type,
    is_array: bool,
}

impl VarEntry {
    #[allow(dead_code)]
    pub fn new(typ: Type, is_array: bool) -> VarEntry {
        VarEntry { typ, is_array }
    }
}

#[allow(dead_code)]
pub struct FuncEntry<'a> {
    #[allow(dead_code)]
    return_type: Type,
    #[allow(dead_code)]
    params: &'a Vec<ParamNode>,
}

impl<'a> FuncEntry<'a> {
    #[allow(dead_code)]
    pub fn new(return_type: Type, params: &'a Vec<ParamNode>) -> FuncEntry<'a> {
        FuncEntry {
            return_type,
            params,
        }
    }
}
