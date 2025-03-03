use crate::function::symbol_types;
use crate::function::CodeGen;
use inkwell::values::BasicValueEnum;
use std::rc::Rc;
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
    fn plus(&mut self, left: String, right: String) -> String {
        //must be int if type checked
        let lhs = *self
            .scopes
            .get_value_from_scope(&self.scope, &left)
            .unwrap()
            .get_int()
            .unwrap();

        let rhs = *self
            .scopes
            .get_value_from_scope(&self.scope, &right)
            .unwrap()
            .get_int()
            .unwrap();

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
    pub fn infix_behaviour(&mut self, op: &str, left: String, right: String) -> String {
        match op {
            "+" => self.plus(left, right),
            _ => panic!("operand not known"),
        }
    }
    fn defined_behaviour(&mut self, func_name: &str, arg: &str) -> String {
        //the function we want to apply
        let closure_to_apply = self
            .scopes
            .get_value_from_scope(&self.scope, func_name)
            .unwrap()
            .get_closure()
            .unwrap();
        let next_arg_var_name = closure_to_apply.next_pattern();

        //The value of the arg, We copy the symbol table entry into the new pattern
        let arg_entry = self
            .scopes
            .get_value_from_scope(&self.scope, arg)
            .unwrap()
            .clone();

        //We need to make a new closure with the new scope that contains the arg
        let ast = closure_to_apply.ast.clone();
        let new_patterns = closure_to_apply.patterns.clone()[1..].to_vec();
        let new_scope = self.scopes.new_scope(Some(closure_to_apply.scope));
        let closure_applied = symbol_types::Closure::new(ast, new_patterns, new_scope);
        let returned_name: String;
        //no args left, execute ast with next arg added to scope

        self.scopes
            .add_symbol_to_scope(&new_scope, next_arg_var_name, arg_entry);
        if closure_applied.patterns.is_empty() {
            returned_name = closure_applied.execute_ast(self);
            let returned_val = self
                .scopes
                .get_value_from_scope(&new_scope, &returned_name)
                .unwrap()
                .clone();
            self.scopes
                .add_symbol_to_scope(&self.scope, returned_name.clone(), returned_val);

            self.scopes.remove_scope(&new_scope);
        }
        // if more than one arg left, add the arg to the subs co
        else {
            let next = self.sym_counter.increment();
            returned_name = format!("{}{}", "closurelit", next);
            self.scopes.add_symbol_to_scope(
                &self.scope,
                returned_name.clone(),
                symbol_types::SymTableEntry::closure_to_entry(closure_applied),
            );
        }
        returned_name
    }
    fn putstrln_behaviour(&self, arg: &str) {
        // get printf linked from c
        let printf_type = self
            .context
            .i32_type()
            .fn_type(&[self.context.i8_type().ptr_type(0.into()).into()], true);
        let printf = self.module.add_function("printf", printf_type, None);

        // Get the arg pointer from the symbol table
        let val_to_print = self.scopes.get_value_from_scope(&self.scope, arg).unwrap();
        let prim_pointer = match val_to_print {
            symbol_types::SymTableEntry::Prim(prim) => Ok(prim),
            symbol_types::SymTableEntry::Clos(_) => Err("cannot print closure"),
        }
        .unwrap();
        //A little convoluted but we basically make a format string and BasicMetadataValueEnum out
        //of the type in the entry
        let (format_string, str_to_print) = match prim_pointer {
            symbol_types::PrimPtrs::Global(string_value) => {
                let zero = *self
                    .scopes
                    .get_value_from_scope(&self.scope, "zero")
                    .unwrap()
                    .get_int()
                    .unwrap();
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
        }
        .unwrap();
        let global_str = self
            .builder
            .build_global_string_ptr(&format_string, "fmt")
            .unwrap()
            .as_pointer_value();

        // Build the call to printf(string)
        self.builder
            .build_call(printf, &[global_str.into(), str_to_print], "call_printf")
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
    pub fn apply_fn(&mut self, func_string: String, arg: String) -> String {
        match FunctionBehaviour::string_to_func(func_string.as_str()) {
            FunctionBehaviour::PutStrLn => {
                self.putstrln_behaviour(&arg);
                arg
            }
            FunctionBehaviour::Defined => self.defined_behaviour(&func_string, &arg),
        }
    }
    pub fn construct_fn(
        &mut self,
        func_name: String,
        ast: Rc<tree_sitter::Node<'ctx>>,
        patterns: Vec<String>,
    ) -> String {
        let closure_entry = symbol_types::SymTableEntry::closure_to_entry(
            symbol_types::Closure::new(ast, patterns, self.scopes.new_scope(Some(self.scope))),
        );
        self.scopes
            .add_symbol_to_scope(&self.scope, func_name.clone(), closure_entry);
        func_name
    }
}
