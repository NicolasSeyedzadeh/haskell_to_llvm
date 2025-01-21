use inkwell::builder;
use inkwell::context;
use inkwell::module;

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
    pub context: &'ctx context::Context,
    pub module: module::Module<'ctx>,
    pub builder: builder::Builder<'ctx>,
}
impl CodeGen<'_> {
    fn putstrln_behaviour(&self, arg: String) -> String {
        let str_to_print = self
            .builder
            .build_global_string_ptr((arg + "\n").as_str(), "str_to_print")
            .unwrap();
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(0.into());
        let printf_type = i8_type.fn_type(&[i8_ptr_type.into()], true);
        let printf = self.module.add_function("printf", printf_type, None);
        self.builder
            .build_call(
                printf,
                &[str_to_print.as_pointer_value().into()],
                "call_printf",
            )
            .expect("Build call failed on print");

        "".to_string()
    }
}
pub fn apply(func_string: String, arg: String, code_generator: &CodeGen) -> String {
    match string_to_func(func_string.as_str()) {
        FunctionBehaviour::PutStrLn => code_generator.putstrln_behaviour(arg),
        FunctionBehaviour::Defined => "".to_string(),
    }
}
