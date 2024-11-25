use inkwell::context::Context;
use inkwell::targets::{Target, TargetMachine, TargetTriple};
use inkwell::OptimizationLevel;
use tree_sitter::{Parser, Tree};
fn main() {
    let source_code = "3+5";

    //TODO: make this come from the CLI
    let triple: &str = "";
    let ast = parse_to_ast(source_code).root_node();

    let context = Context::create();
    let module = context.create_module("my_module");

    let target_triple = if triple.is_empty() {
        TargetMachine::get_default_triple()
    } else {
        TargetTriple::create(triple)
    };
    module.set_triple(&target_triple);

    let target = Target::from_triple(&target_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("error ");
    module.set_data_layout(&(target_machine.get_target_data()).get_data_layout());

    // Create a simple main function
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    let return_value = i32_type.const_int(0, false);

    let builder = context.create_builder();
    builder.position_at_end(basic_block);
    let _ = builder.build_return(Some(&return_value));
    module.print_to_stderr();
}

fn parse_to_ast(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_haskell::LANGUAGE.into())
        .expect("Error loading Haskell grammar");

    parser.parse(source_code, None).unwrap()
}
