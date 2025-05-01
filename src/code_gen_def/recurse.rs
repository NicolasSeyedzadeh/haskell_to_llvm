use crate::code_gen_def::class::GeneralisedClosure;
use crate::code_gen_def::data_constructors;
use crate::code_gen_def::data_constructors::ADT;
use crate::code_gen_def::symbol_types;
use crate::code_gen_def::CodeGen;
use std::iter::zip;
use std::rc::Rc;

use super::symbol_types::PrimPtrs;

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
                let rec = &ast.child_by_field_name("function").unwrap();
                match rec.kind() {
                    "variable" | "apply" | "parens" => {
                        let func = self.recursive_compile(rec);
                        let arg = Rc::new(ast.child_by_field_name("argument").unwrap());
                        self.apply_fn(func, arg)
                    }

                    "constructor" => {
                        let i32_type = self.context.i32_type();
                        let i8_ptr_type: inkwell::types::PointerType<'_> =
                            self.context.i8_type().ptr_type(0.into()); //void pointer
                        let constructor_key = rec.utf8_text(self.source_code).unwrap();
                        let arg =
                            self.recursive_compile(&ast.child_by_field_name("argument").unwrap());
                        let constructor = self
                            .scopes
                            .get_constructor(&self.scope, constructor_key)
                            .unwrap();
                        let tag = constructor.get_tag();
                        let adt = self
                            .scopes
                            .get_value_from_scope(&self.scope, &constructor.type_loc)
                            .unwrap()
                            .get_adt()
                            .unwrap();
                        let tag_value: inkwell::values::IntValue<'_> =
                            i32_type.const_int(tag, false);

                        let undef = adt.type_llvm.get_undef();
                        //if the argument is an int store it with a pointer
                        let payload_ptr =
                            match self.scopes.get_value_from_scope(&self.scope, &arg).unwrap() {
                                symbol_types::SymTableEntry::Prim(PrimPtrs::Basic(_)) => {
                                    let arg_payload = *self.get_int(arg).unwrap();
                                    let alloca =
                                        self.builder.build_alloca(i32_type, "lit_payload").unwrap();
                                    let _ = self.builder.build_store(alloca, arg_payload);
                                    self.builder
                                        .build_bit_cast(alloca, i8_ptr_type, "payload_cast")
                                        .unwrap()
                                }
                                symbol_types::SymTableEntry::Prim(PrimPtrs::Constructor(_)) => {
                                    let arg_payload = self
                                        .get_constructor_literal(arg, constructor.type_loc.clone())
                                        .unwrap()
                                        .struct_value;
                                    let alloca =
                                        self.builder.build_alloca(i32_type, "lit_payload").unwrap();
                                    let _ = self.builder.build_store(alloca, arg_payload);
                                    self.builder
                                        .build_bit_cast(alloca, i8_ptr_type, "payload_cast")
                                        .unwrap()
                                }
                                _ => panic!("Only ADT and Int supported as ADT field"),
                            };

                        let with_tag = self
                            .builder
                            .build_insert_value(undef, tag_value, 0, "insert_tag")
                            .unwrap()
                            .into_struct_value();
                        let const_struct = self
                            .builder
                            .build_insert_value(with_tag, payload_ptr, 1, "insert_payload")
                            .unwrap()
                            .into_struct_value();

                        let constructor_name =
                            format!("ADT literal {}", self.sym_counter.increment());
                        self.scopes.add_symbol_to_scope(
                            &self.scope,
                            constructor_name.clone(),
                            symbol_types::SymTableEntry::adt_constructor_to_entry(
                                data_constructors::ConstructorLiteral::new(
                                    const_struct,
                                    constructor_key.to_string(),
                                ),
                            ),
                        );

                        constructor_name
                    }
                    _ => panic!("trying to apply something not supported"),
                }
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
                let id = ast
                    .child_by_field_name("name")
                    .unwrap()
                    .utf8_text(self.source_code)
                    .unwrap()
                    .to_string();
                let patterns: Vec<String> =
                    symbol_types::tree_to_children(ast.child_by_field_name("patterns").unwrap())
                        .iter()
                        .map(|x| self.recursive_compile(x))
                        .collect();

                let i32_type = self.context.i32_type();
                //placeholder type for possible data payloads
                let new_ptr_type = self.context.i8_type().ptr_type(0.into());

                // make new type, tag to decide which constructor it is
                let new_type = self
                    .context
                    .struct_type(&[i32_type.into(), new_ptr_type.into()], false);

                let mut constr_name_list = vec![];
                for (union_number, x) in
                    symbol_types::tree_to_children(ast.child_by_field_name("constructors").unwrap())
                        .iter()
                        .filter(|x| x.grammar_name() != "|")
                        .enumerate()
                {
                    let (name, cons) = data_constructors::Constructor::parse_from_ast(
                        x,
                        self,
                        id.clone(),
                        union_number,
                    );
                    self.scopes
                        .register_constructor(&self.scope, name.clone(), cons);
                    constr_name_list.push(name);
                }

                let new_adt = ADT::new(id.clone(), patterns, new_type, constr_name_list);
                self.scopes.add_symbol_to_scope(
                    &self.scope,
                    id.clone(),
                    symbol_types::SymTableEntry::adt_def_to_entry(new_adt),
                );
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
            "name" => {
                if ast.kind() == "constructor" {
                    let i32_type = self.context.i32_type();
                    let i8_ptr_type: inkwell::types::PointerType<'_> =
                        self.context.i8_type().ptr_type(0.into());

                    let constructor_key = ast.utf8_text(self.source_code).unwrap();
                    let constructor = self
                        .scopes
                        .get_constructor(&self.scope, constructor_key)
                        .unwrap();
                    let tag = constructor.get_tag();
                    let adt = self
                        .scopes
                        .get_value_from_scope(&self.scope, &constructor.type_loc)
                        .unwrap()
                        .get_adt()
                        .unwrap();

                    let tag_value: inkwell::values::IntValue<'_> = i32_type.const_int(tag, false);
                    let alloca = self.builder.build_alloca(i32_type, "lit_payload").unwrap();
                    let payload_ptr = self
                        .builder
                        .build_bit_cast(alloca, i8_ptr_type, "payload_cast")
                        .unwrap();

                    let undef = adt.type_llvm.get_undef();
                    let with_tag = self
                        .builder
                        .build_insert_value(undef, tag_value, 0, "insert_tag")
                        .unwrap()
                        .into_struct_value();
                    let const_struct = self
                        .builder
                        .build_insert_value(with_tag, payload_ptr, 1, "insert_payload")
                        .unwrap()
                        .into_struct_value();

                    let constructor_name = format!("ADT literal {}", self.sym_counter.increment());
                    self.scopes.add_symbol_to_scope(
                        &self.scope,
                        constructor_name.clone(),
                        symbol_types::SymTableEntry::adt_constructor_to_entry(
                            data_constructors::ConstructorLiteral::new(
                                const_struct,
                                constructor_key.to_string(),
                            ),
                        ),
                    );

                    constructor_name
                } else {
                    ast.utf8_text(self.source_code).unwrap().to_string()
                }
            }

            "integer" | "variable" | "operator" => {
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
            //for some reason + goes to an "operator" node but - goes to a "-" node
            "-" => "-".to_string(),

            _ => panic!("Unimplemented grammar node: {}", ast.grammar_name()),
        }
    }
}
