use std::{collections::HashMap, rc::Rc};

use super::CodeGen;

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
        value: SymTableEntry<'a>,
    ) {
        self.scopes
            .get_mut(&scope_id)
            .expect("Trying to add to scope that is not in the Arena")
            .add_symbol(key, value);
    }
    pub fn get_value_from_scope(
        &self,
        scope_id: &ScopeId,
        key: &str,
    ) -> Option<&SymTableEntry<'a>> {
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
    pub fn remove_scope(&mut self, scope_id: &ScopeId) {
        self.scopes.remove(scope_id);
    }
}

#[derive(Clone)]
pub struct Scope<'a> {
    parent_scope: Option<ScopeId>,
    symbol_table: HashMap<String, SymTableEntry<'a>>,
}
impl<'a> Scope<'a> {
    fn new(parent_scope: Option<usize>) -> Self {
        Scope {
            parent_scope: parent_scope,
            symbol_table: HashMap::new(),
        }
    }
    fn add_symbol(&mut self, string: String, entry: SymTableEntry<'a>) {
        self.symbol_table.insert(string, entry);
    }
    fn get_symbol(&self, string: &str) -> Option<&SymTableEntry<'a>> {
        self.symbol_table.get(string)
    }
    fn remove_symbol(&mut self, string: &str) -> Option<SymTableEntry<'a>> {
        self.symbol_table.remove(string)
    }
}
#[derive(Clone)]
pub struct Closure<'a> {
    pub ast: Rc<tree_sitter::Node<'a>>,
    pub patterns: Vec<String>,
    pub scope: ScopeId,
}
impl<'a> Closure<'a> {
    pub fn new(ast: Rc<tree_sitter::Node<'a>>, patterns: Vec<String>, scope: ScopeId) -> Self {
        Closure {
            ast,
            patterns,
            scope,
        }
    }
    pub fn next_pattern(&'a self) -> String {
        self.patterns.get(0).unwrap().clone()
    }

    pub fn execute_ast(self, code_generator: &mut CodeGen<'a>) -> String {
        let mut last_result = None;
        let old_scope = code_generator.scope;
        code_generator.scope = self.scope;
        for expression in tree_to_children(*self.ast).iter() {
            println!("compiling: {}\n", expression);
            last_result = Some(code_generator.recursive_compile(expression));
        }
        code_generator.scope = old_scope;
        last_result.unwrap()
    }
}

#[derive(Clone)]
pub enum PrimPtrs<'a> {
    Basic(inkwell::values::BasicValueEnum<'a>),
    Global(inkwell::values::GlobalValue<'a>),
}

#[derive(Clone)]
pub enum SymTableEntry<'a> {
    Clos(Closure<'a>),
    Prim(PrimPtrs<'a>),
}

impl<'a> SymTableEntry<'a> {
    pub fn get_str(&self) -> Result<&inkwell::values::GlobalValue<'a>, String> {
        match self {
            SymTableEntry::Clos(_) => Err("Expected String".to_string()),
            SymTableEntry::Prim(prim) => match prim {
                PrimPtrs::Global(global_ptr) => Ok(global_ptr),
                _ => Err("Expected String".to_string()),
            },
        }
    }
    pub fn get_int(&self) -> Result<&inkwell::values::IntValue<'a>, String> {
        match self {
            SymTableEntry::Prim(prim) => match prim {
                PrimPtrs::Basic(basic_ptr) => match basic_ptr {
                    inkwell::values::BasicValueEnum::IntValue(int_ptr) => Ok(int_ptr),
                    _ => Err("Expected Int not other basic".to_string()),
                },
                _ => Err("Expected Int".to_string()),
            },
            _ => Err("Expected Int".to_string()),
        }
    }
    pub fn get_ptr(&self) -> Result<&inkwell::values::PointerValue<'a>, String> {
        match self {
            SymTableEntry::Prim(prim) => match prim {
                PrimPtrs::Basic(basic_ptr) => match basic_ptr {
                    inkwell::values::BasicValueEnum::PointerValue(ptr) => Ok(ptr),
                    _ => Err("Expected pointer not other basic".to_string()),
                },
                _ => Err("Expected pointer".to_string()),
            },
            _ => Err("Expected pointer".to_string()),
        }
    }
    pub fn get_closure(&self) -> Result<&Closure<'a>, String> {
        match self {
            SymTableEntry::Clos(closure) => Ok(closure),
            _ => Err("Expected closure".to_string()),
        }
    }
    pub fn global_to_entry(ptr: inkwell::values::GlobalValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Global(ptr))
    }
    pub fn pointer_to_entry(ptr: inkwell::values::PointerValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Basic(
            inkwell::values::BasicValueEnum::PointerValue(ptr),
        ))
    }
    pub fn int_to_entry(ptr: inkwell::values::IntValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Basic(inkwell::values::BasicValueEnum::IntValue(
            ptr,
        )))
    }
    pub fn closure_to_entry(closure: Closure<'a>) -> Self {
        SymTableEntry::Clos(closure)
    }
}

pub fn tree_to_children(code_ast: tree_sitter::Node) -> Vec<tree_sitter::Node> {
    return code_ast.children(&mut code_ast.walk()).collect();
}
