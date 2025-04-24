use crate::code_gen_def::class::GeneralisedClosure;
use crate::code_gen_def::data_constructors;
use crate::code_gen_def::data_constructors::Constructor;
use crate::code_gen_def::data_constructors::ADT;
use crate::code_gen_def::symbol_types;
use crate::code_gen_def::CodeGen;
use std::collections::HashSet;
use std::iter::zip;
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
                    self.recursive_compile(&ast.child_by_field_name("function").unwrap());
                let arg = Rc::new(ast.child_by_field_name("argument").unwrap());
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

            "function" => {
                self.construct_fn(ast);

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

                let i32_type = self.context.i32_type();

                //placeholder type for possible data payloads
                let new_ptr_type = self.context.struct_type(&[], false).ptr_type(0.into());

                // make new type, tag to decide which constructor it is
                let new_type = self
                    .context
                    .struct_type(&[i32_type.into(), new_ptr_type.into()], false);
                let new_adt = ADT::new(id.clone(), patterns, new_type);

                self.scopes.add_symbol_to_scope(
                    &self.scope,
                    id.clone(),
                    symbol_types::SymTableEntry::adt_def_to_entry(new_adt),
                );

                let constructors: Vec<Constructor> = symbol_types::tree_to_children(
                    ast.child_by_field_name("constructors").unwrap(),
                )
                .iter()
                .enumerate()
                .filter(|(_, x)| x.grammar_name() != "|")
                .map(|(union_number, x)| {
                    data_constructors::Constructor::parse_from_ast(
                        x,
                        self,
                        id.clone(),
                        union_number,
                    )
                })
                .collect();

                constructors.into_iter().for_each(|x| {
                    self.scopes.add_symbol_to_scope(
                        &self.scope,
                        format!("{}+{}", id, x.name),
                        symbol_types::SymTableEntry::adt_constructor_to_entry(x),
                    )
                });

                id
            }
            "class" => {
                //don't need in the current implementation, we dont actuallly store the class we
                //just store the closures that they create
                let id = self.recursive_compile(&ast.child_by_field_name("name").unwrap());
                let patterns: Vec<String> =
                    symbol_types::tree_to_children(ast.child_by_field_name("patterns").unwrap())
                        .iter()
                        .map(|x| self.recursive_compile(x))
                        .collect();

                let decls: Vec<_> = symbol_types::tree_to_children(
                    ast.child_by_field_name("declarations").unwrap(),
                );

                let names = decls
                    .iter()
                    .map(|x| {
                        x.child_by_field_name("name")
                            .unwrap()
                            .utf8_text(self.source_code)
                            .unwrap()
                    })
                    .collect::<Vec<_>>();
                let types = decls
                    .iter()
                    .map(|x| x.child_by_field_name("type").unwrap())
                    .collect::<Vec<_>>();

                for (name, ty) in zip(names, types) {
                    let clos = GeneralisedClosure::new(
                        &ty,
                        patterns.clone(),
                        self.scopes.new_scope(Some(self.scope)),
                        self.source_code,
                    );
                    self.scopes.add_symbol_to_scope(
                        &self.scope,
                        name.to_string(),
                        symbol_types::SymTableEntry::generalised_closure_to_entry(clos),
                    );
                }
                id
            }

            "integer" | "variable" | "operator" | "name" => {
                ast.utf8_text(self.source_code).unwrap().to_string()
            }
            "instance" => {
                panic!("Implemetn instance");
                /*println!("{}", ast);
                let mut closure_set:HashSet<String>=HashSet::new();
                let ADT_key=ast.child_by_field_name("patterns").unwrap().utf8_text(self.source_code).unwrap();
                let
                for decl in
                    symbol_types::tree_to_children(ast.child_by_field_name("declarations").unwrap())
                        .iter()
                {
                    closure_set.insert(self.construct_fn(decl));
                }
                for closure in closure_set{




                }*/
            }

            _ => panic!("Unimplemented grammar node: {}", ast.grammar_name()),
        }
    }
}
