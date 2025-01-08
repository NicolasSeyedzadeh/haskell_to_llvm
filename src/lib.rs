use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use inkwell::OptimizationLevel;
use tree_sitter::{Parser, Tree};
pub fn compile(source_code: &str) {
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");
    let triple: &str = "";
    let ast = parse_to_ast(source_code);
    println!("{}", ast.root_node());

    //haskell declarations -> declarations -> top_splice -> actual ast
    let code_ast = ast
        .root_node()
        .child(0)
        .unwrap()
        .child(0)
        .unwrap()
        .child(0)
        .unwrap();

    //print!("{}",code_ast);
    let context = Context::create();
    let module = context.create_module("my_module");

    let target_triple = if triple.is_empty() {
        TargetMachine::get_default_triple()
    } else {
        TargetTriple::create(triple)
    };
    let target = Target::from_triple(&target_triple).expect("Failed to get  from triple");
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
    //allocate an int onto the stack
    let stack_loc = builder.build_alloca(i32_type, "name").expect("msg");

    // store the value in location
    let _ = builder.build_store(stack_loc, i32_type.const_int(3, false));

    println!("{}", recursive_build(code_ast, source_code, &builder));

    let _ = builder.build_return(Some(&return_value));
    //module.print_to_stderr();
}

fn parse_to_ast(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_haskell::LANGUAGE.into())
        .expect("Error loading Haskell grammar");

    parser.parse(source_code, None).unwrap()
}

fn get_val_from_node(ast: tree_sitter::Node, code: &str) -> String {
    code[ast.byte_range()].to_string()
}

/*struct ctx{

}
fn recursive_build_wrapper(
    ast: tree_sitter::Node,
    code: &str,
    _builder: &inkwell::builder::Builder)
    -> () {

    }*/

fn recursive_build(
    ast: tree_sitter::Node,
    code: &str,
    _builder: &inkwell::builder::Builder,
) -> String {
    //match top level with string
    match ast.grammar_name() {
        "let_in" => get_val_from_node(ast.child(1).unwrap(), code), //ast. child 0 = "let", child 1 = match expression, child 2 = "in", child 3 = expression
        "local_binds" => "hi".to_string(),
        _ => "Did not find a match".to_string(),
    }
}

//OK current plan:
// - Create Monads and start with IO
// - Add function logic, new struct with function ast and arg counter
// - When function application detected create a native function recogniser
