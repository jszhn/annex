use std::collections::HashMap;

use crate::ast::types::{ParamNode, Type};

/// FIFO stack-based symbol table implementation, for entering and exiting program scopes.
pub struct ScopedSymTable {
    pub scopes: Vec<SymTable<VarEntry>>,
    num_scopes: u32,
}

impl ScopedSymTable {
    pub fn new() -> ScopedSymTable {
        let mut scopes = Vec::new();
        scopes.push(SymTable::new(0));
        ScopedSymTable {
            scopes,
            num_scopes: 1,
        }
    }

    /// Enters new scope, with new symbol table.
    pub fn enter(&mut self) {
        self.scopes.push(SymTable::new(self.num_scopes + 1));
        self.num_scopes += 1;
    }

    /// Exits current scope. Discards current symbol table.
    pub fn exit(&mut self) {
        if self.num_scopes > 1 {
            self.scopes.pop();
            self.num_scopes -= 1;
        }
    }

    pub fn lookup(&self, key: &String) -> Option<&VarEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get_ref(key) {
                return Some(entry);
            }
        }
        None
    }

    /// Looks-up the given key in the current scope's symbol table.
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

pub struct VarEntry {
    _typ: Type,
    _is_array: bool,
}

impl VarEntry {
    pub fn new(_typ: Type, _is_array: bool) -> VarEntry {
        VarEntry { _typ, _is_array }
    }
}

pub struct FuncEntry<'a> {
    _return_type: Type,
    _params: &'a Vec<ParamNode>,
}

impl<'a> FuncEntry<'a> {
    pub fn new(_return_type: Type, _params: &'a Vec<ParamNode>) -> FuncEntry<'a> {
        FuncEntry {
            _return_type,
            _params,
        }
    }
}
