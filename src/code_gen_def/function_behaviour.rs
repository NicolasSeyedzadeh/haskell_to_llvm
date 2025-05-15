use crate::code_gen_def::symbol_types;
use crate::code_gen_def::CodeGen;
use inkwell::values::AsValueRef;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;
use std::rc::Rc;

use super::data_constructors;
use super::symbol_types::SymTableEntry;

pub enum FunctionBehaviour {
    PutStrLn,
    Defined,
}
impl FunctionBehaviour {
    pub fn string_to_func(string: &str) -> Self {
        match string {
            "print" => FunctionBehaviour::PutStrLn,
            _ => FunctionBehaviour::Defined,
        }
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn position_at_start(&mut self, block: inkwell::basic_block::BasicBlock) {
        match block.get_last_instruction() {
            None => self.builder.position_at_end(block),
            Some(x) => self.builder.position_before(&x),
        }
    }
    pub fn get_and_eval_indirect(&self, name: &str) -> &SymTableEntry<'ctx> {
        match self.scopes.get_value_from_scope(&self.scope, name).unwrap() {
            symbol_types::SymTableEntry::Indirect(x) => self.get_and_eval_indirect(x),
            x => x,
        }
    }
    //if the value is frozen then eval and replace, else just return
    pub fn get_and_evaluate_from_scope(&mut self, key: &str) {
        if let Some(symbol_types::SymTableEntry::Ast(_)) =
            self.scopes.get_value_from_scope(&self.scope, key)
        {
            //panic!("{}",self.scopes.get_value_from_scope(&self.scope, key).unwrap().get_ast().unwrap());
            let (frozen_entry, found_scope) =
                self.scopes.recieve_owned_entry(&self.scope, key).unwrap();
            let frozen_ast = frozen_entry.get_ast().unwrap();

            let computation_result = self.recursive_compile(&frozen_ast);
            let evaluated = self
                .scopes
                .recieve_owned_entry(&self.scope, &computation_result)
                .unwrap()
                .0;
            self.scopes
                .add_symbol_to_scope(&found_scope, key.to_string(), evaluated.clone());
        };
    }

    fn plus(&mut self, left: String, right: String) -> String {
        //must be int if type checked
        self.get_and_evaluate_from_scope(&left);
        self.get_and_evaluate_from_scope(&right);
        let lhs = self.get_int(&left);

        let rhs = self.get_int(&right);

        let next = self.sym_counter.increment();
        let name = format!("add_result{}", next);

        let intval = self.builder.build_int_add(lhs, rhs, &name).unwrap();

        self.scopes.add_symbol_to_scope(
            &self.scope,
            name.clone(),
            symbol_types::SymTableEntry::int_to_entry(intval),
        );
        name
    }
    fn minus(&mut self, left: String, right: String) -> String {
        //must be int if type checked

        self.get_and_evaluate_from_scope(&left);
        self.get_and_evaluate_from_scope(&right);

        let lhs = self.get_int(&left);

        let rhs = self.get_int(&right);

        let next = self.sym_counter.increment();
        let name = format!("sub_result{}", next);

        let intval = self.builder.build_int_sub(lhs, rhs, &name).unwrap();

        self.scopes.add_symbol_to_scope(
            &self.scope,
            name.clone(),
            symbol_types::SymTableEntry::int_to_entry(intval),
        );
        name
    }
    fn mult(&mut self, left: String, right: String) -> String {
        //must be int if type checked

        self.get_and_evaluate_from_scope(&left);
        self.get_and_evaluate_from_scope(&right);

        let lhs = self.get_int(&left);

        let rhs = self.get_int(&right);

        let next = self.sym_counter.increment();
        let name = format!("mul_result{}", next);

        let intval = self.builder.build_int_mul(lhs, rhs, &name).unwrap();

        self.scopes.add_symbol_to_scope(
            &self.scope,
            name.clone(),
            symbol_types::SymTableEntry::int_to_entry(intval),
        );
        name
    }
    fn div(&mut self, left: String, right: String) -> String {
        //must be int if type checked

        self.get_and_evaluate_from_scope(&left);
        self.get_and_evaluate_from_scope(&right);
        let lhs = self.get_int(&left);
        let rhs = self.get_int(&right);
        let next = self.sym_counter.increment();
        let name = format!("div_result{}", next);

        let intval = self.builder.build_int_signed_div(lhs, rhs, &name).unwrap();

        self.scopes.add_symbol_to_scope(
            &self.scope,
            name.clone(),
            symbol_types::SymTableEntry::int_to_entry(intval),
        );
        name
    }
    pub fn infix_behaviour(&mut self, op: &str, left: String, right: String) -> String {
        match op {
            "+" => self.plus(left, right),
            "-" => self.minus(left, right),
            "*" => self.mult(left, right),
            "/" => self.div(left, right),
            _ => panic!("operand {} not known", op),
        }
    }
    fn defined_behaviour(&mut self, func_name: &str, arg: Rc<tree_sitter::Node<'ctx>>) -> String {
        //the function we want to apply
        //TODO: make this not clone the closure
        //The problem is that we need a reference to the closure to get everything we need from it
        //but holding the reference holds a refernce to the code generator meaning we cant execute
        //it later

        //value needed here
        self.get_and_evaluate_from_scope(func_name);

        let closure_to_apply = self
            .scopes
            .get_value_from_scope(&self.scope, func_name)
            .unwrap()
            .get_closure()
            .unwrap()
            .clone();

        let patterns_left = closure_to_apply.patterns_left();
        let first_pattern = closure_to_apply.next_pattern();
        let returned_name;
        let arg_entry = symbol_types::SymTableEntry::Ast(arg.clone());

        let new_scope = if *closure_to_apply.currently_executing.borrow() {
            self.scopes.new_scope(Some(self.scope))
        } else {
            self.scopes.new_scope(Some(closure_to_apply.scope))
        };

        //if we dont have a complete application, make a new closure with the new scope and patterns
        if patterns_left > 1 {
            let new_closure = closure_to_apply.clone_with_new_patterns(new_scope);

            self.scopes.add_symbol_to_scope(
                &new_scope,
                first_pattern
                    .utf8_text(self.source_code)
                    .unwrap()
                    .to_string(),
                arg_entry,
            );

            //make new closure and add it to old scope
            let next = self.sym_counter.increment();
            returned_name = format!("{}{}", "closurelit", next);
            self.scopes.add_symbol_to_scope(
                &self.scope,
                returned_name.clone(),
                symbol_types::SymTableEntry::closure_to_entry(new_closure),
            );
        }
        //if we have a complete application, determine whether we need the switch and execute the ast
        else {
            //if theres a jump point, execute the arg and use it as a switch key
            returned_name = match closure_to_apply.jump_points {
                Some(_) => {
                    let arg_to_switch_on = self.recursive_compile(&arg);
                    if let None = self
                        .scopes
                        .get_value_from_scope(&new_scope, &arg_to_switch_on)
                    {
                        let val_from_other_scope = self
                            .scopes
                            .recieve_owned_entry(&self.scope, &arg_to_switch_on)
                            .unwrap();
                        self.scopes.add_symbol_to_scope(
                            &new_scope,
                            arg_to_switch_on.clone(),
                            val_from_other_scope.0,
                        );
                    }
                    closure_to_apply
                        .execute_ast(self, new_scope, Some(arg_to_switch_on))
                        .0
                }

                //last arg and no jump points, we just execute with no switch key
                None => {
                    //Allocate arg
                    self.scopes.add_symbol_to_scope(
                        &self.scope,
                        first_pattern
                            .utf8_text(self.source_code)
                            .unwrap()
                            .to_string(),
                        symbol_types::SymTableEntry::Ast(arg),
                    );
                    closure_to_apply.execute_ast(self, new_scope, None).0
                }
            }
        }

        returned_name
    }

    fn putstrln_behaviour(&mut self, arg: &str) {
        let zero = self.get_int("zero");

        // Get the arg pointer from the symbol table
        self.get_and_evaluate_from_scope(arg);
        let val_to_print = self.scopes.get_value_from_scope(&self.scope, arg).unwrap();
        let prim_pointer = match val_to_print {
            symbol_types::SymTableEntry::Prim(prim) => Ok(prim),
            symbol_types::SymTableEntry::Pointer(ptr) => Ok({
                self.unwrap_pointer_to_int(*ptr, arg.to_string());
                self.get_and_eval_indirect(arg).get_prim().unwrap()
            }),
            symbol_types::SymTableEntry::Clos(..) => Err("cannot print closure"),
            symbol_types::SymTableEntry::AdtDef(..) => Err("cannot print ADT definition"),
            symbol_types::SymTableEntry::GenClos(..) => Err("cannot print closure"),
            symbol_types::SymTableEntry::Ast(..) => Err("cannot print frozen"),
            _ => panic!("cannot print this type"),
        }
        .unwrap();

        //A little convoluted but we basically make a format string and BasicMetadataValueEnum out
        //of the type in the entry
        let (format_string, str_to_print) = match prim_pointer {
            symbol_types::PrimPtrs::Global(string_value) => {
                // Use `build_gep` to get pointer to string rather than char array
                let string_ptr = unsafe {
                    self.builder.build_gep(
                        string_value.as_pointer_value(),
                        &[zero, zero],
                        "string_ptr",
                    )
                }
                .unwrap();
                Ok((
                    "%s\n",
                    inkwell::values::BasicMetadataValueEnum::PointerValue(string_ptr),
                ))
            }
            symbol_types::PrimPtrs::Basic(basic) => match basic {
                BasicValueEnum::IntValue(int) => Ok((
                    "%d\n",
                    inkwell::values::BasicMetadataValueEnum::IntValue(*int),
                )),
                _ => Err("Print not supported for other basic values"),
            },
            symbol_types::PrimPtrs::Constructor(_) => panic!("print not supportd on constructor"),
        }
        .unwrap();

        let global_str = self
            .builder
            .build_global_string_ptr(format_string, "fmt")
            .unwrap()
            .as_pointer_value();

        // Build the call to printf(string)
        self.builder
            .build_call(
                self.printf,
                &[global_str.into(), str_to_print],
                "call_printf",
            )
            .expect("Build call failed on print");
    }
    fn allocate_literal_string_wrapped(&mut self, lit_name: String, to_allocate: String) -> String {
        let new_lit = self.context.const_string(to_allocate.as_bytes(), false);
        let str_global = self.module.add_global(new_lit.get_type(), None, &lit_name);
        str_global.set_initializer(&new_lit);
        self.scopes.add_symbol_to_scope(
            &self.scope,
            lit_name.clone(),
            symbol_types::SymTableEntry::global_to_entry(str_global),
        );
        lit_name
    }
    pub fn allocate_literal_string(&mut self, string: String) -> String {
        let next = self.sym_counter.increment();
        self.allocate_literal_string_wrapped(format!("{}{}", "strlit", next), string)
    }
    pub fn allocate_literal_int(&mut self, int: i32) -> String {
        let lit_name = format!("{}{}", "intlit", self.sym_counter.increment());
        let int_ptr = self.context.i32_type().const_int(int as u64, true);
        self.scopes.add_symbol_to_scope(
            &self.scope,
            lit_name.clone(),
            symbol_types::SymTableEntry::int_to_entry(int_ptr),
        );
        lit_name
    }
    pub fn apply_fn(&mut self, func_string: String, arg: Rc<tree_sitter::Node<'ctx>>) -> String {
        match FunctionBehaviour::string_to_func(func_string.as_str()) {
            FunctionBehaviour::PutStrLn => {
                let string_to_print = self.recursive_compile(&arg);
                self.putstrln_behaviour(&string_to_print);
                "zero".to_string()
            }
            FunctionBehaviour::Defined => self.defined_behaviour(&func_string, arg),
        }
    }
    pub fn construct_fn(&mut self, func_ast: &tree_sitter::Node<'ctx>) -> String {
        //parse function name

        let func_name = self.recursive_compile(&func_ast.child_by_field_name("name").unwrap());
        //get the args
        let patterns =
            symbol_types::tree_to_children(func_ast.child_by_field_name("patterns").unwrap())
                .iter()
                .map(|x| Rc::new(*x))
                .collect();

        let closure_ast = Rc::new(
            func_ast
                .child_by_field_name("match")
                .unwrap()
                .child_by_field_name("expression")
                .unwrap(),
        );

        let closure_to_add = symbol_types::ClosureAux::new(closure_ast, patterns);
        let entry_to_add_back;
        //look for existing arm of the closure
        match self.scopes.recieve_owned_entry(&self.scope, &func_name) {
            //if not there add one
            None => {
                entry_to_add_back =
                    symbol_types::SymTableEntry::closure_to_entry(symbol_types::Closure::new(
                        None,
                        closure_to_add,
                        self.scopes.new_scope(Some(self.scope)),
                        func_name.clone(),
                    ));
            }
            Some((closure_list, _)) => {
                let mut closure = closure_list.get_closure_move().unwrap();
                let switching = closure.last_pattern();

                let old_default_closure =
                    closure.replace_default(symbol_types::ClosureUnion::ClosureAux(closure_to_add));

                let arm_value;

                //extract the value we look for when switching
                //TODO: fix this for applied constructor
                if switching.grammar_name() == "literal" {
                    arm_value = switching
                        .utf8_text(self.source_code)
                        .unwrap()
                        .parse::<u64>()
                        .unwrap();
                } else if switching.grammar_name() == "name" {
                    arm_value = self
                        .scopes
                        .get_value_from_scope(
                            &self.scope,
                            switching.utf8_text(self.source_code).unwrap(),
                        )
                        .unwrap()
                        .get_constructor_template()
                        .unwrap()
                        .get_tag();
                } else if switching.grammar_name() == "parens" {
                    arm_value = self
                        .scopes
                        .get_value_from_scope(
                            &self.scope,
                            switching
                                .child_by_field_name("pattern")
                                .unwrap()
                                .child_by_field_name("function")
                                .unwrap()
                                .utf8_text(self.source_code)
                                .unwrap(),
                        )
                        .unwrap()
                        .get_constructor_template()
                        .unwrap()
                        .get_tag();
                } else {
                    panic!("Matching only implemented for integers, datacontructors, or variables, last pattern is a {}",switching.grammar_name())
                }

                //if we already have a jump point then just add a new one
                if let Some(ref mut jump) = closure.jump_points {
                    jump.add_point(arm_value, old_default_closure);
                }
                //make new jump point if second time defined
                else {
                    let mut new_point = symbol_types::JumpPoint::new();
                    new_point.add_point(arm_value, old_default_closure);
                    closure.jump_points = Some(new_point);
                }
                entry_to_add_back = symbol_types::SymTableEntry::closure_to_entry(closure);
            }
        }

        self.scopes
            .add_symbol_to_scope(&self.scope, func_name.clone(), entry_to_add_back);

        func_name
    }

    pub fn get_int(&mut self, name: &str) -> inkwell::values::IntValue<'ctx> {
        self.get_and_evaluate_from_scope(name);
        let int = match self.get_and_eval_indirect(name) {
            symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Basic(
                inkwell::values::BasicValueEnum::IntValue(_),
            )) => "true",
            symbol_types::SymTableEntry::Pointer(_) => "false",
            symbol_types::SymTableEntry::Indirect(key) => &key.clone(),
            _ => panic!("expected integer or pointer"),
        };
        if int == "true" {
            match self.get_and_eval_indirect(name) {
                symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Basic(
                    inkwell::values::BasicValueEnum::IntValue(int_ptr),
                )) => *int_ptr,
                _ => panic!("Expected integer"),
            }
        } else if int == "false" {
            let recieved_ptr = self
                .scopes
                .recieve_owned_entry(&self.scope, &name)
                .unwrap()
                .0;
            match recieved_ptr {
                SymTableEntry::Pointer(ptr) => {
                    self.unwrap_pointer_to_int(ptr, name.to_string());
                    match self
                        .scopes
                        .get_value_from_scope(&self.scope, &name)
                        .unwrap()
                    {
                        symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Basic(
                            inkwell::values::BasicValueEnum::IntValue(intptr),
                        )) => *intptr,
                        _ => panic!("This should never be seen"),
                    }
                }

                _ => panic!("Expected integer"),
            }
        }
        //We are an indirect item
        else {
            self.get_int(int)
        }
    }
    fn unwrap_pointer_to_int(&mut self, ptr: PointerValue<'ctx>, new_name: String) {
        let real_payload_ptr = self
            .builder
            .build_bit_cast(
                ptr,
                self.context.i32_type().ptr_type(0.into()),
                "cast_payload",
            )
            .unwrap()
            .into_pointer_value();
        let loaded_payload = self
            .builder
            .build_load(real_payload_ptr, "load_real_payload")
            .unwrap()
            .into_int_value();
        self.scopes.add_symbol_to_scope(
            &self.scope,
            new_name,
            symbol_types::SymTableEntry::int_to_entry(loaded_payload),
        );
    }
    pub fn get_constructor_literal(
        &mut self,
        name: String,
        expected_type_key: String,
    ) -> Result<&data_constructors::ConstructorLiteral<'ctx>, String> {
        let constr = match self.get_and_eval_indirect(&name) {
            symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Constructor(_)) => true,
            symbol_types::SymTableEntry::Pointer(_) => false,
            _ => panic!("Expected ADT"),
        };
        if constr {
            match self.get_and_eval_indirect(&name) {
                symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Constructor(con)) => {
                    Ok(con)
                }
                _ => panic!("Expected Int"),
            }
        } else {
            let recieved_ptr = self
                .scopes
                .recieve_owned_entry(&self.scope, &name)
                .unwrap()
                .0;
            match recieved_ptr {
                symbol_types::SymTableEntry::Pointer(ptr) => {
                    let expected_type = self
                        .scopes
                        .get_value_from_scope(&self.scope, &expected_type_key)
                        .unwrap()
                        .get_adt()
                        .unwrap();
                    let real_payload_ptr = self
                        .builder
                        .build_bit_cast(
                            ptr,
                            expected_type.type_llvm.ptr_type(0.into()),
                            "cast_payload",
                        )
                        .unwrap()
                        .into_pointer_value();
                    let loaded_payload = self
                        .builder
                        .build_load(real_payload_ptr, "load_real_payload")
                        .unwrap()
                        .into_struct_value();

                    let payload_constr_lit = data_constructors::ConstructorLiteral::new(
                        loaded_payload,
                        expected_type_key.clone(),
                    );
                    self.scopes.add_symbol_to_scope(
                        &self.scope,
                        name.clone(),
                        symbol_types::SymTableEntry::adt_constructor_to_entry(payload_constr_lit),
                    );

                    match self
                        .scopes
                        .get_value_from_scope(&self.scope, &name.clone())
                        .unwrap()
                    {
                        symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Constructor(
                            con,
                        )) => Ok(con),
                        _ => panic!("This should never be seen"),
                    }
                }
                _ => panic!("Expected ptr"),
            }
        }
    }
    pub fn get_constructor_literal_no_pointer_check(
        &self,
        name: &str,
    ) -> Result<&data_constructors::ConstructorLiteral<'ctx>, String> {
        match self.get_and_eval_indirect(name) {
            symbol_types::SymTableEntry::Prim(symbol_types::PrimPtrs::Constructor(con)) => Ok(con),
            _ => panic!("expected constructor"),
        }
    }
}
