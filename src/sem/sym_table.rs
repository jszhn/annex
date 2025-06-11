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
    
    /// Insert a variable into the current scope
    pub fn insert_current(&mut self, key: String, entry: VarEntry) -> Option<VarEntry> {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(key, entry)
        } else {
            None
        }
    }
    
    /// Check if a variable exists in the current scope only
    pub fn contains_current(&self, key: &String) -> bool {
        if let Some(scope) = self.scopes.last() {
            scope.contains(key)
        } else {
            false
        }
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

#[derive(Debug, Clone, PartialEq)]
pub struct VarEntry {
    pub typ: Type,
    pub is_array: bool,
}

impl VarEntry {
    pub fn new(typ: Type, is_array: bool) -> VarEntry {
        VarEntry { typ, is_array }
    }
    
    pub fn get_type(&self) -> &Type {
        &self.typ
    }
    
    pub fn is_array(&self) -> bool {
        self.is_array
    }
}

#[derive(Debug, Clone)]
pub struct FuncEntry {
    pub return_type: Type,
    pub params: Vec<ParamNode>,
}

impl FuncEntry {
    pub fn new(return_type: Type, params: Vec<ParamNode>) -> FuncEntry {
        FuncEntry {
            return_type,
            params,
        }
    }
    
    pub fn get_return_type(&self) -> &Type {
        &self.return_type
    }
    
    pub fn get_params(&self) -> &Vec<ParamNode> {
        &self.params
    }
    
    pub fn matches_signature(&self, args: &Vec<Type>) -> bool {
        if self.params.len() != args.len() {
            return false;
        }
        
        for (param, arg) in self.params.iter().zip(args.iter()) {
            if param.typ != *arg {
                return false;
            }
        }
        
        true
    }
}
