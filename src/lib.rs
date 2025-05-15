use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use inkwell::OptimizationLevel;
use std::fs;
use tree_sitter::{Parser, Tree};
mod code_gen_def;

//mod types;

pub fn compile(source_path: &str, destination: &str) {
    //read source code
    let source_code = &fs::read_to_string(source_path).unwrap();
    let ast = parse_to_ast(source_code);

    //haskell declarations always the root with no additional information
    let code_ast = ast.root_node().child(0).unwrap();
    let code_ast_split_on_line: Vec<tree_sitter::Node> =
        code_gen_def::symbol_types::tree_to_children(code_ast);

    //inkwell objects for genereating IR
    let context = Context::create();
    let module = context.create_module("my_module");

    //get target machine for module data layout
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");

    let triple: &str = "";
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
            OptimizationLevel::None,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .expect("error ");
    module.set_data_layout(&(target_machine.get_target_data()).get_data_layout());

    //More inkwell setup
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let basic_block: inkwell::basic_block::BasicBlock<'_> =
        context.append_basic_block(function, "entry");
    let return_value = i32_type.const_int(0, false);
    let builder = context.create_builder();
    builder.position_at_end(basic_block);
    // get printf linked from c
    let printf_type = context
        .i32_type()
        .fn_type(&[context.i8_type().ptr_type(0.into()).into()], true);
    let printf = module.add_function("printf", printf_type, None);

    //our code generator object does the work on the AST given all the Inkwell setup
    let code_generator = &mut code_gen_def::CodeGen::new(
        &context,
        module,
        builder,
        function,
        basic_block,
        source_code.as_bytes(),
        printf,
    );
    for expression in code_ast_split_on_line.iter() {
        //println!("compiling: {}\n", expression);
        code_generator.recursive_compile(expression);
    }

    let _ = code_generator.builder.build_return(Some(&return_value));
    code_generator
        .module
        .print_to_file(destination)
        .expect("Problem with printing to file");
}

fn parse_to_ast(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_haskell::LANGUAGE.into())
        .expect("Error loading Haskell grammar");

    parser.parse(source_code, None).unwrap()
}
