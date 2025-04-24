use std::collections::HashMap;

use super::{
    data_constructors,
    symbol_types::{self, SymTableEntry},
};
pub type ScopeId = usize;
pub struct ScopeArena<'a> {
    scopes: HashMap<ScopeId, Scope<'a>>,
    counter: ScopeId,
}
impl<'a> ScopeArena<'a> {
    pub fn new() -> Self {
        ScopeArena {
            scopes: HashMap::new(),
            counter: 0,
        }
    }

    pub fn new_scope(&mut self, parent_scope: Option<ScopeId>) -> ScopeId {
        self.counter += 1;
        self.scopes.insert(self.counter, Scope::new(parent_scope));
        self.counter
    }
    pub fn add_symbol_to_scope(
        &mut self,
        scope_id: &ScopeId,
        key: String,
        value: symbol_types::SymTableEntry<'a>,
    ) {
        self.scopes
            .get_mut(scope_id)
            .expect("Trying to add to scope that is not in the Arena")
            .add_symbol(key, value);
    }
    pub fn get_value_from_scope(
        &self,
        scope_id: &ScopeId,
        key: &str,
    ) -> Option<&symbol_types::SymTableEntry<'a>> {
        let scope = self
            .scopes
            .get(scope_id)
            .expect("Trying to recieve from scope that is not in the Arena");
        match scope.get_symbol(key) {
            Some(entry) => Some(entry),
            None => scope
                .parent_scope
                .and_then(|parent_id| self.get_value_from_scope(&parent_id, key)),
        }
    }

    pub fn rename_key_in_scope(&mut self, scope_id: &ScopeId, old_key: &str, new_key: String) {
        let value = self
            .scopes
            .get_mut(scope_id)
            .expect("trying to rename item in non-existant scope")
            .remove_symbol(old_key)
            .expect("renamed key out of scope");
        self.add_symbol_to_scope(scope_id, new_key, value);
    }
    //only expected to be used to alter closures when we encounter a new pattern
    pub fn recieve_owned_entry(
        &mut self,
        scope_id: &ScopeId,
        key: &str,
    ) -> Option<SymTableEntry<'a>> {
        let scope = self
            .scopes
            .get(scope_id)
            .expect("Trying to recieve from scope that is not in the Arena");

        match scope.get_symbol(key) {
            Some(_) => self.scopes.get_mut(scope_id).unwrap().remove_symbol(key),
            None => scope
                .parent_scope
                .and_then(|parent_id| self.recieve_owned_entry(&parent_id, key)),
        }
        //self.scopes.get_mut(scope_id).unwrap().remove_symbol(key)
    }
    pub fn remove_scope(&mut self, scope_id: &ScopeId) {
        self.scopes.remove(scope_id);
    }
    pub fn debug_print_in_scope(&self, scope_id: &ScopeId) {
        let scope = self
            .scopes
            .get(scope_id)
            .expect("Trying to debug print from scope that is not in the Arena");

        match scope.parent_scope {
            None => print!("{:?}\n\n\n", scope.symbol_table),
            Some(parent) => {
                print!("{:?}\n\n\n", scope.symbol_table);
                self.debug_print_in_scope(&parent);
            }
        }
    }
}

pub struct Scope<'a> {
    parent_scope: Option<ScopeId>,
    symbol_table: HashMap<String, symbol_types::SymTableEntry<'a>>,
    constructor_symbol_table: HashMap<String, (usize, data_constructors::Constructor)>, //constructor name-> Constructor struct instance
}
impl<'a> Scope<'a> {
    fn new(parent_scope: Option<usize>) -> Self {
        Scope {
            parent_scope,
            symbol_table: HashMap::new(),
            constructor_symbol_table: HashMap::new(),
        }
    }
    fn add_symbol(&mut self, string: String, entry: symbol_types::SymTableEntry<'a>) {
        self.symbol_table.insert(string, entry);
    }
    fn get_symbol(&self, string: &str) -> Option<&symbol_types::SymTableEntry<'a>> {
        self.symbol_table.get(string)
    }
    fn remove_symbol(&mut self, string: &str) -> Option<symbol_types::SymTableEntry<'a>> {
        self.symbol_table.remove(string)
    }
}
