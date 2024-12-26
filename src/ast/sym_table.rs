use crate::ast::types::DType;
use std::collections::HashMap;

/// Symbol table struct type. Wraps around a hash table.
pub struct SymTable<T> {
    table: HashMap<String, T>,
    pub scope_level: u32,
}

impl<T> SymTable<T> {
    fn get_ref(&self, key: String) -> Option<&T> {
        self.table.get(&key)
    }

    fn contains(&self, key: String) -> bool {
        let result = self.table.get(&key);
        result.is_some()
    }

    fn insert(&mut self, key: String, value: T) -> Option<T> {
        self.table.insert(key, value)
    }
}

pub struct VarEntry {
    _type: DType,
    is_array: bool,
}

impl VarEntry {
    fn new(&self, _type: DType, is_array: bool) -> VarEntry {
        VarEntry { _type, is_array }
    }
}

pub struct FuncEntry {
    return_type: DType,
    arguments: Vec<(String, DType)>,
}

impl FuncEntry {
    fn new(&self, return_type: DType, arguments: Vec<(String, DType)>) -> FuncEntry {
        FuncEntry {
            return_type,
            arguments,
        }
    }
}
