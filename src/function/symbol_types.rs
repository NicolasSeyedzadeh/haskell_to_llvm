struct Closure<'a> {
    function_id: i32,
    ast: tree_sitter::Node<'a>,
    args_left: usize,
}

enum PrimPtrs<'a> {
    Basic(inkwell::values::BasicValueEnum<'a>),
    Global(inkwell::values::GlobalValue<'a>),
    Pointer(inkwell::values::PointerValue<'a>),
}

pub enum SymTableEntry<'a> {
    Clos(Closure<'a>),
    Prim(PrimPtrs<'a>),
}
impl<'a> SymTableEntry<'a> {
    pub fn get_str(&self) -> Result<&inkwell::values::GlobalValue<'a>, String> {
        match self {
            SymTableEntry::Clos(_) => Err("Expected String".to_string()),
            SymTableEntry::Prim(prim) => match prim {
                PrimPtrs::Basic(_) => Err("Expected String".to_string()),
                PrimPtrs::Global(global_ptr) => Ok(global_ptr),
                PrimPtrs::Pointer(_) => Err("Expected String".to_string()),
            },
        }
    }

    pub fn global_to_entry(ptr: inkwell::values::GlobalValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Global(ptr))
    }
}
