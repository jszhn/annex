use crate::ast::types::{ParamNode, Type};
use std::collections::HashMap;

/// Symbol table type. Wraps around a hash table.
pub struct SymTable<T> {
    table: HashMap<String, T>,
    pub scope: u32,
}

impl<T> SymTable<T> {
    pub fn new(scope: u32) -> Self {
        Self {
            table: Default::default(),
            scope,
        }
    }

    pub fn contains(&self, key: &String) -> bool {
        self.table.contains_key(key)
    }

    pub fn insert(&mut self, key: String, value: T) -> Option<T> {
        self.table.insert(key, value)
    }

    pub fn lookup(&self, key: &String) -> Option<&T> {
        self.table.get(key)
    }

    pub fn lookup_mut(&mut self, key: &String) -> Option<&mut T> {
        self.table.get_mut(key)
    }
}

/// FIFO stack-based symbol table implementation, for entering and exiting program scopes.
pub struct ScopedSymTable {
    pub scopes: Vec<SymTable<VarEntry>>,
    num_scopes: u32,
}

impl Default for ScopedSymTable {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopedSymTable {
    pub fn new() -> ScopedSymTable {
        let scopes = vec![SymTable::new(0)];
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

    pub fn insert(&mut self, key: String, value: VarEntry) -> Option<VarEntry> {
        let scope = self.scopes.last_mut()?;
        scope.insert(key, value)
    }

    pub fn lookup(&self, key: &String) -> Option<&VarEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.lookup(key) {
                return Some(entry);
            }
        }
        None
    }

    /// Looks-up the given key in the current scope's symbol table.
    pub fn lookup_current(&self, key: &String) -> Option<&VarEntry> {
        let scope = self.scopes.last()?;
        if let Some(entry) = scope.lookup(key) {
            return Some(entry);
        }
        None
    }
}

pub struct VarEntry {
    pub typ: Type,
    pub is_array: bool,
}

impl VarEntry {
    pub fn new(typ: Type, is_array: bool) -> VarEntry {
        VarEntry { typ, is_array }
    }
}

/// Symbol table type for functions.
pub type FuncTable = SymTable<FuncEntry>;

pub struct FuncEntry {
    pub params: Vec<ParamNode>,
    pub return_typ: Type,
    /// todo: This will be an internal marker used to indicate that duplicate function definitions have
    /// been found. We'll continue analysis but will only throw a fatal error if/when this
    /// function is called.
    marked: bool,
}

impl FuncEntry {
    pub fn new(params: Vec<ParamNode>, return_typ: Type) -> FuncEntry {
        FuncEntry {
            return_typ,
            params,
            marked: false,
        }
    }

    pub fn mark(&mut self) {
        self.marked = true;
    }

    pub fn compare_signature(&self, args: &[Option<Type>]) -> bool {
        self.params.len() == args.len()
            && self
                .params
                .iter()
                .zip(args.iter())
                .all(|(param, arg)| arg.as_ref().is_some_and(|t| param.typ == *t))
    }
}
