use inkwell::builder;
use inkwell::context;
use inkwell::module;
use inkwell::values::BasicValueEnum;
use std::rc::Rc;
use symbol_types::ScopeArena;
use symbol_types::ScopeId;
use symbol_types::SymTableEntry;
mod counter;
pub mod symbol_types;

mod function_behaviour;
pub struct CodeGen<'ctx> {
    context: &'ctx context::Context,
    pub module: module::Module<'ctx>,
    pub builder: builder::Builder<'ctx>,
    source_code: &'ctx [u8],
    scope: ScopeId,
    sym_counter: Box<counter::Counter>,
    scopes: ScopeArena<'ctx>,
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx context::Context,
        module: module::Module<'ctx>,
        builder: builder::Builder<'ctx>,
        source_code: &'ctx [u8],
    ) -> Self {
        let mut scopes = ScopeArena::new();
        let mut cg = CodeGen {
            context,
            module,
            builder,
            source_code,
            scope: scopes.new_scope(None),
            sym_counter: Box::new(counter::Counter::new()),
            scopes,
        };
        let zero = cg.context.i32_type().const_zero();
        cg.scopes.add_symbol_to_scope(
            &cg.scope,
            "zero".to_string(),
            symbol_types::SymTableEntry::int_to_entry(zero),
        );

        let new_lit = cg.context.const_string("\n".as_bytes(), true);
        let newline = cg.module.add_global(new_lit.get_type(), None, "newline");
        newline.set_initializer(&new_lit);

        let newline_ptr = unsafe {
            cg.builder
                .build_gep(newline.as_pointer_value(), &[zero, zero], "string_ptr")
        }
        .unwrap();

        cg.scopes.add_symbol_to_scope(
            &cg.scope,
            "newline_ptr".to_string(),
            symbol_types::SymTableEntry::pointer_to_entry(newline_ptr),
        );
        cg
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
            SymTableEntry::Prim(prim) => Ok(prim),
            SymTableEntry::Clos(_) => Err("cannot print closure"),
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
        match function_behaviour::FunctionBehaviour::string_to_func(func_string.as_str()) {
            function_behaviour::FunctionBehaviour::PutStrLn => {
                self.putstrln_behaviour(&arg);
                arg
            }
            function_behaviour::FunctionBehaviour::Defined => {
                self.defined_behaviour(&func_string, &arg)
            }
        }
    }
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
    fn infix_behaviour(&mut self, op: &str, left: String, right: String) -> String {
        match op {
            "+" => self.plus(left, right),
            _ => panic!("operand not known"),
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
            "integer" | "variable" | "operator" => {
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
            _ => panic!("Unimplemented grammar node: {}", ast.grammar_name()),
        }
    }
}

/*        println!("Printing hashmaps: ");
for k in self.scopes.scopes.get(&parent).unwrap().symbol_table.keys() {
    println!("{}", k);
}  */
