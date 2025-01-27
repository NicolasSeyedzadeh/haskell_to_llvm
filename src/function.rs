use inkwell::builder;
use inkwell::context;
use inkwell::module;
use std::collections::HashMap;
mod counter;
pub mod symbol_types;

pub enum FunctionBehaviour {
    PutStrLn,
    Defined,
}

fn string_to_func(string: &str) -> FunctionBehaviour {
    match string {
        "putStrLn" => FunctionBehaviour::PutStrLn,
        _ => FunctionBehaviour::Defined,
    }
}
pub struct CodeGen<'ctx> {
    context: &'ctx context::Context,
    pub module: module::Module<'ctx>,
    pub builder: builder::Builder<'ctx>,
    symbol_table: HashMap<String, symbol_types::SymTableEntry<'ctx>>,
    sym_counter: Box<counter::Counter>,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx context::Context,
        module: module::Module<'ctx>,
        builder: builder::Builder<'ctx>,
    ) -> Self {
        CodeGen {
            context,
            module,
            builder,
            symbol_table: HashMap::new(),
            sym_counter: Box::new(counter::Counter::new()),
        }
    }
    fn putstrln_behaviour(&self, arg: String) -> String {
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(0.into());

        // get printf linked from c
        let printf_type = i8_type.fn_type(&[i8_ptr_type.into()], true);
        let printf = self.module.add_function("printf", printf_type, None);

        // Get the string pointer from the symbol table
        let string_value = self
            .symbol_table
            .get(&arg)
            .unwrap()
            .get_str()
            .unwrap()
            .as_pointer_value();

        // Use `build_gep` to convert `[N x i8]*` to `i8*`
        let zero = self.context.i8_type().const_zero();
        let string_ptr = unsafe {
            self.builder
                .build_gep(string_value, &[zero, zero], "string_ptr")
        };

        // Build the call to printf
        self.builder
            .build_call(
                printf,
                &[inkwell::values::BasicMetadataValueEnum::PointerValue(
                    string_ptr.unwrap(),
                )],
                "call_printf",
            )
            .expect("Build call failed on print");

        arg
    }
    pub fn allocate_literal_string(&mut self, string: String) -> String {
        let new_lit = self.context.const_string(string.as_bytes(), false);
        let lit_name = format!("{}{}", "strlit", self.sym_counter.increment());
        let str_global = self.module.add_global(new_lit.get_type(), None, &lit_name);
        str_global.set_initializer(&self.context.const_string(string.as_bytes(), false));
        self.symbol_table.insert(
            lit_name.clone(),
            symbol_types::SymTableEntry::global_to_entry(str_global),
        );
        lit_name
    }
    fn allocate_literal_int(&self, int: i32) -> String {
        int;
        todo!("int literal allocation to implement")
    }
}
pub fn apply(func_string: String, arg: String, code_generator: &CodeGen) -> String {
    match string_to_func(func_string.as_str()) {
        FunctionBehaviour::PutStrLn => code_generator.putstrln_behaviour(arg),
        FunctionBehaviour::Defined => todo!(),
    }
}
