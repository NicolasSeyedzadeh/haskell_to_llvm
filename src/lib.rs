use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::fs;
use tree_sitter::{Parser, Tree};
mod function;
mod types;

pub fn compile(source_path: &str) {
    let source_code = &fs::read_to_string(source_path).unwrap();
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");

    types::get_types(source_path);

    let triple: &str = "";
    let ast = parse_to_ast(source_code);
    let sym_table: HashMap<String, function::symbol_types::SymTableEntry> = HashMap::new();

    //haskell declarations always the root with no additional information
    let code_ast = ast.root_node().child(0).unwrap();
    let code_ast_split_on_line: Vec<tree_sitter::Node> = tree_to_children(code_ast);

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

    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);

    let basic_block = context.append_basic_block(function, "entry");
    let return_value = i32_type.const_int(0, false);

    let builder = context.create_builder();
    builder.position_at_end(basic_block);

    /*for expression in code_ast_split_on_line.iter(){
        println!("{}", recursive_compile(&expression, source_code, &builder));
    }*/
    let mut code_generator = function::CodeGen::new(&context, module, builder);
    println!("{}", code_ast_split_on_line[1]);
    println!(
        "{}",
        recursive_compile(
            &code_ast_split_on_line[1],
            source_code.as_bytes(),
            &mut code_generator
        )
    );

    //return ownership
    let builder = code_generator.builder;
    let module = code_generator.module;

    let _ = builder.build_return(Some(&return_value));

    module.print_to_file("out.ll").expect("ouch");
}

fn tree_to_children(code_ast: tree_sitter::Node) -> Vec<tree_sitter::Node> {
    return code_ast.children(&mut code_ast.walk()).collect();
}

fn parse_to_ast(source_code: &str) -> Tree {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_haskell::LANGUAGE.into())
        .expect("Error loading Haskell grammar");

    parser.parse(source_code, None).unwrap()
}

fn recursive_compile(
    ast: &tree_sitter::Node,
    code: &[u8],
    code_generator: &mut function::CodeGen<'_>,
) -> String {
    //match top level with string
    //match on id rather than grammar name
    //println!("{}",ast);
    match ast.grammar_name() {
        "signature" => {
            recursive_compile(
                &ast.child_by_field_name("name").unwrap(),
                code,
                code_generator,
            ) //; get_val_from_node(&ast.child_by_field_name("type").unwrap(), code)
        }
        "bind" => {
            recursive_compile(
                &ast.child_by_field_name("name").unwrap(),
                code,
                code_generator,
            );
            recursive_compile(
                &ast.child_by_field_name("match").unwrap(),
                code,
                code_generator,
            )
        }
        "match" => recursive_compile(
            &ast.child_by_field_name("expression").unwrap(),
            code,
            code_generator,
        ),
        "apply" => function::apply(
            recursive_compile(
                &ast.child_by_field_name("function").unwrap(),
                code,
                code_generator,
            ),
            recursive_compile(
                &ast.child_by_field_name("argument").unwrap(),
                code,
                code_generator,
            ),
            code_generator,
        ),
        "literal" => {
            //select int or str
            let lit = recursive_compile(&ast.child(0).unwrap(), code, code_generator);
            code_generator.allocate_literal_string(lit)
        }
        "string" => {
            let mut chars = ast.utf8_text(code).unwrap().chars();
            chars.next();
            chars.next_back();
            chars.as_str().to_string()
        }
        "variable" => ast.utf8_text(code).unwrap().to_string(),
        _ => "Did not find a match".to_string(),
    }
}
//Tree sitter haskell notes:
//(bind name: (variable) match: (match expression: (apply function: (variable) argument: (literal (string)))))

//            |bind|
//           /      \
//    name  /        \ match
//         /          \
//      |var|        |match|
//                      \
//                       \ express
//                        \
//                       |apply|
//                      /       \
//                  function    argument

//we need a symbol table that associates each variable with a global pointer to the code object, the
//type and the ast with its code generated in it and we pass around the global pointers, this allows
//us to return pointers to values of variable from vars and pointers to literals from literals.
