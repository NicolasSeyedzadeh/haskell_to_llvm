use crate::function::symbol_types;
use crate::function::CodeGen;
use std::rc::Rc;

impl<'ctx> CodeGen<'ctx> {
    pub fn recursive_compile(&mut self, ast: &tree_sitter::Node<'ctx>) -> String {
        //match top level with string
        //match on id rather than grammar name
        match ast.grammar_name() {
            "signature" => "".to_string(),
            "bind" => {
                let bind_name = self.recursive_compile(&ast.child_by_field_name("name").unwrap());
                let bind_value = self.recursive_compile(&ast.child_by_field_name("match").unwrap());
                self.scopes
                    .rename_key_in_scope(&self.scope, &bind_value, bind_name.clone());
                bind_name
            }
            "match" => self.recursive_compile(&ast.child_by_field_name("expression").unwrap()),
            "apply" => {
                let func: String =
                    { self.recursive_compile(&ast.child_by_field_name("function").unwrap()) };
                let arg = { self.recursive_compile(&ast.child_by_field_name("argument").unwrap()) };
                self.apply_fn(func, arg)
            }
            "literal" => {
                //select int or str
                let subtree = &ast.child(0).unwrap();
                let litval = self.recursive_compile(subtree);
                if subtree.grammar_name() == "string" {
                    self.allocate_literal_string(litval)
                } else {
                    self.allocate_literal_int(litval.parse::<i32>().unwrap())
                }
            }
            "string" => {
                let mut chars = ast.utf8_text(self.source_code).unwrap().chars();
                chars.next();
                chars.next_back();
                chars.as_str().to_string()
            }
            "integer" | "variable" | "operator" | "name" => {
                ast.utf8_text(self.source_code).unwrap().to_string()
            }
            "function" => {
                //parse function name
                let function_name =
                    self.recursive_compile(&ast.child_by_field_name("name").unwrap());
                //get the args
                let patterns =
                    symbol_types::tree_to_children(ast.child_by_field_name("patterns").unwrap())
                        .iter()
                        .map(|x| self.recursive_compile(x))
                        .collect();

                //construct the function adding the closure to the scope
                self.construct_fn(
                    function_name,
                    Rc::new(
                        ast.child_by_field_name("match")
                            .unwrap()
                            .child_by_field_name("expression")
                            .unwrap(),
                    ),
                    patterns,
                );

                //return nothing as we have completed the bind
                "".to_string()
            }
            "parens" => self.recursive_compile(&ast.child_by_field_name("expression").unwrap()),
            "infix" => {
                let operator =
                    self.recursive_compile(&ast.child_by_field_name("operator").unwrap());

                let left =
                    self.recursive_compile(&ast.child_by_field_name("left_operand").unwrap());
                let right =
                    self.recursive_compile(&ast.child_by_field_name("right_operand").unwrap());

                self.infix_behaviour(&operator, left, right)
            }
            "data_type" => {
                let id = self.recursive_compile(&ast.child_by_field_name("name").unwrap());
                let patterns: Vec<String> =
                    symbol_types::tree_to_children(ast.child_by_field_name("patterns").unwrap())
                        .iter()
                        .map(|x| self.recursive_compile(x))
                        .collect();

                let type_constructors: Vec<_> = symbol_types::tree_to_children(
                    ast.child_by_field_name("constructors").unwrap(),
                )
                .iter()
                .map(|x| self.recursive_compile(x))
                .collect();

                println!("{:?}", type_constructors);

                "".to_string()
            }
            "data_constructor" => {
                println!("{}", ast.to_string());
                "".to_string()
            }
            _ => panic!("Unimplemented grammar node: {}", ast.grammar_name()),
        }
    }
}
