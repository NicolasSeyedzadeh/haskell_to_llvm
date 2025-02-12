use std::collections::HashMap;

use super::CodeGen;

#[derive(Clone)]
pub struct Scope<'a> {
    parent_scope: Option<usize>,
    symbol_table: HashMap<String, SymTableEntry<'a>>,
}
impl<'a> Scope<'a> {
    pub fn new(parent_scope: Option<usize>) -> Self {
        Scope {
            parent_scope: parent_scope,
            symbol_table: HashMap::new(),
        }
    }
    pub fn add_symbol(&mut self, string: String, entry: SymTableEntry<'a>) {
        self.symbol_table.insert(string, entry);
    }
    pub fn get_symbol<'b>(
        &'b self,
        string: &str,
        scopes: &'b Vec<Box<Scope<'a>>>,
    ) -> Option<&'b SymTableEntry<'a>> {
        match self.symbol_table.get(string) {
            Some(entry) => Some(entry),
            None => self
                .parent_scope
                .and_then(|x| scopes[x].get_symbol(string, scopes)),
        }
    }
}
#[derive(Clone)]
pub struct Closure<'a> {
    pub ast: Box<tree_sitter::Node<'a>>,
    pub patterns: Vec<String>,
    pub scope: Scope<'a>,
}
impl<'a> Closure<'a> {
    pub fn new(ast: Box<tree_sitter::Node<'a>>, patterns: Vec<String>, scope: Scope<'a>) -> Self {
        Closure {
            ast,
            patterns,
            scope,
        }
    }
    pub fn next_pattern(&'a self) -> &'a String {
        self.patterns.get(0).unwrap()
    }

    pub fn execute_ast(self, code_generator: &mut CodeGen<'a>) -> String {
        let mut last_result = None;
        for expression in tree_to_children(*self.ast).iter() {
            println!("compiling: {}\n", expression);
            last_result = Some(code_generator.recursive_compile(expression));
        }
        last_result.unwrap()
    }
}

#[derive(Clone)]
enum PrimPtrs<'a> {
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
