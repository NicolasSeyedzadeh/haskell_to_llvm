use inkwell::builder;
use inkwell::context;
use inkwell::module;
use inkwell::values::FunctionValue;
mod counter;
pub mod symbol_types;

mod class;
mod data_constructors;
mod function_behaviour;
mod recurse;
mod scoping;

pub struct CodeGen<'ctx> {
    context: &'ctx context::Context,
    pub module: module::Module<'ctx>,
    pub builder: builder::Builder<'ctx>,
    pub function: inkwell::values::FunctionValue<'ctx>,
    pub basic_block: inkwell::basic_block::BasicBlock<'ctx>,
    source_code: &'ctx [u8],
    scope: scoping::ScopeId,
    sym_counter: Box<counter::Counter>,
    scopes: scoping::ScopeArena<'ctx>,
    printf: FunctionValue<'ctx>,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx context::Context,
        module: module::Module<'ctx>,
        builder: builder::Builder<'ctx>,
        function: inkwell::values::FunctionValue<'ctx>,
        basic_block: inkwell::basic_block::BasicBlock<'ctx>,
        source_code: &'ctx [u8],
        printf: FunctionValue<'ctx>,
    ) -> Self {
        let mut scopes = scoping::ScopeArena::new();
        let mut cg = CodeGen {
            context,
            module,
            builder,
            function,
            basic_block,
            source_code,
            scope: scopes.new_scope(None),
            sym_counter: Box::new(counter::Counter::new()),
            scopes,
            printf,
        };
        let zero = cg.context.i32_type().const_zero();
        cg.scopes.add_symbol_to_scope(
            &cg.scope,
            "zero".to_string(),
            symbol_types::SymTableEntry::int_to_entry(zero),
        );

        let new_lit = cg.context.const_string("\n".as_bytes(), true);
        let newline = cg.module.add_global(new_lit.get_type(), None, "newline");
        newline.set_initializer(&new_lit);

        let newline_ptr = unsafe {
            cg.builder
                .build_gep(newline.as_pointer_value(), &[zero, zero], "string_ptr")
        }
        .unwrap();

        cg.scopes.add_symbol_to_scope(
            &cg.scope,
            "newline_ptr".to_string(),
            symbol_types::SymTableEntry::pointer_to_entry(newline_ptr),
        );
        cg
    }
}

/*        println!("Printing hashmaps: ");
for k in self.scopes.scopes.get(&parent).unwrap().symbol_table.keys() {
    println!("{}", k);
}  */
