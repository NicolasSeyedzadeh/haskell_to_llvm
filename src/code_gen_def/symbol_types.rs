use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;
use inkwell::values::IntValue;
use inkwell::values::PhiValue;
use inkwell::values::PointerValue;
use inkwell::values::StructValue;

use super::class;
use super::class::GeneralisedClosure;
use super::data_constructors;
use super::data_constructors::Constructor;
use super::data_constructors::ConstructorLiteral;
use super::data_constructors::ADT;
use super::scoping;
use super::scoping::ScopeId;
use super::CodeGen;
use core::panic;
use std::cell::RefCell;
use std::iter::zip;
use std::mem;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum ClosureUnion<'a> {
    Closure(Closure<'a>),
    ClosureAux(ClosureAux<'a>),
}
impl<'a> ClosureUnion<'a> {
    pub fn next_pattern(&self) -> Rc<tree_sitter::Node<'a>> {
        match self {
            ClosureUnion::Closure(clos) => clos.next_pattern(),
            ClosureUnion::ClosureAux(clos) => clos.next_pattern(),
        }
    }
    pub fn _patterns(&self) -> &Vec<Rc<tree_sitter::Node<'_>>> {
        match self {
            ClosureUnion::Closure(clos) => clos.patterns(),
            ClosureUnion::ClosureAux(clos) => &clos.patterns,
        }
    }
}

#[derive(Clone, Debug)]
pub struct JumpPoint<'a> {
    pub switch_key: Option<String>,
    switches: Vec<u64>,
    possible_asts: Vec<ClosureUnion<'a>>,
}
impl<'a> JumpPoint<'a> {
    pub fn new() -> Self {
        JumpPoint {
            switches: vec![],
            possible_asts: vec![],
            switch_key: None,
        }
    }
    pub fn add_point(&mut self, switch_point: u64, closure: ClosureUnion<'a>) {
        self.switches.push(switch_point);
        self.possible_asts.push(closure);
    }
}
#[derive(Clone, Debug)]
pub enum Argument<'a> {
    Int(IntValue<'a>),
    Const(StructValue<'a>),
}
#[derive(Clone, Debug)]
pub struct Closure<'a> {
    //only allowing one jump point right now
    pub jump_points: Option<JumpPoint<'a>>,
    pub default_closure: Box<ClosureUnion<'a>>,
    pub scope: scoping::ScopeId,
    derivative: String,
    pub currently_executing: RefCell<bool>,
}
impl<'a> Closure<'a> {
    pub fn new(
        jump_points: Option<JumpPoint<'a>>,
        default_closure: ClosureAux<'a>,
        scope: scoping::ScopeId,
        derivative: String,
    ) -> Self {
        Closure {
            jump_points,
            default_closure: Box::new(ClosureUnion::ClosureAux(default_closure)),
            scope,
            currently_executing: RefCell::new(false),
            derivative,
        }
    }
    pub fn set_switch_key(&mut self, new_key: Option<String>) {
        if let Some(ref mut jump) = self.jump_points {
            jump.switch_key = new_key
        } else {
            panic!("Tried to set switch key with no jumps")
        }
    }
    pub fn get_switch_key(&self) -> Option<String> {
        match self.jump_points.as_ref() {
            None => None,
            Some(x) => x.switch_key.clone(),
        }
    }
    pub fn new_with_closure(
        jump_points: Option<JumpPoint<'a>>,
        default_closure: Closure<'a>,
        scope: scoping::ScopeId,
        derivative: String,
    ) -> Self {
        Closure {
            jump_points,
            default_closure: Box::new(ClosureUnion::Closure(default_closure)),
            scope,
            currently_executing: RefCell::new(false),
            derivative,
        }
    }
    pub fn clone_with_new_patterns(&self, new_scope: scoping::ScopeId) -> Closure<'a> {
        let mut clone = self.clone();
        clone.new_patterns();
        clone.scope = new_scope;
        *clone.currently_executing.borrow_mut() = false;
        clone
    }
    pub fn replace_default(&mut self, new_default: ClosureUnion<'a>) -> ClosureUnion<'a> {
        mem::replace(&mut self.default_closure, new_default)
    }
    pub fn new_patterns(&mut self) {
        match self.default_closure.as_mut() {
            ClosureUnion::Closure(nested) => nested.new_patterns(),
            ClosureUnion::ClosureAux(x) => x.pattern_pointer += 1,
        }
    }
    pub fn patterns(&self) -> &Vec<Rc<tree_sitter::Node<'a>>> {
        match self.default_closure.as_ref() {
            ClosureUnion::Closure(nested) => nested.patterns(),
            ClosureUnion::ClosureAux(x) => &x.patterns,
        }
    }
    pub fn patterns_left(&self) -> usize {
        match self.default_closure.as_ref() {
            ClosureUnion::Closure(nested) => nested.patterns_left(),
            ClosureUnion::ClosureAux(x) => x.patterns.len() - x.pattern_pointer,
        }
    }
    pub fn last_pattern(&self) -> Rc<tree_sitter::Node<'a>> {
        match self.default_closure.as_ref() {
            ClosureUnion::Closure(nested) => nested.last_pattern(),
            ClosureUnion::ClosureAux(x) => x.patterns.last().unwrap().clone(),
        }
    }
    pub fn next_pattern(&self) -> Rc<tree_sitter::Node<'a>> {
        match self.default_closure.as_ref() {
            ClosureUnion::Closure(nested) => nested.next_pattern(),
            ClosureUnion::ClosureAux(x) => x.next_pattern(),
        }
    }
    //testing only
    pub fn get_ast(&self) -> Rc<tree_sitter::Node<'a>> {
        match self.default_closure.as_ref() {
            ClosureUnion::Closure(nested) => nested.get_ast(),
            ClosureUnion::ClosureAux(x) => x.ast.clone(),
        }
    }
    pub fn execute_ast(
        &self,
        code_generator: &mut CodeGen<'a>,
        new_scope: ScopeId,
        possible_switch_key: Option<String>,
    ) -> (String, inkwell::basic_block::BasicBlock<'a>) {
        //if we are calling a function that is currently executing
        let old_scope = code_generator.scope;
        code_generator.scope = new_scope;
        let parent_closure = code_generator
            .scopes
            .get_value_from_scope(&code_generator.scope, &self.derivative)
            .unwrap()
            .get_closure()
            .unwrap();
        let patterns: Vec<String> = self
            .patterns()
            .iter()
            .map(|x| parse_arg(**x, code_generator))
            .clone()
            .collect();
        //the value of the item supposed to come from the previous block.
        if *parent_closure.currently_executing.borrow() {
            let name = format!("recursive call {}", code_generator.sym_counter.increment());
            for pattern in &patterns {
                code_generator.get_and_evaluate_from_scope(pattern);
            }
            code_generator.scopes.add_symbol_to_scope(
                &old_scope,
                name.clone(),
                SymTableEntry::Rec(new_scope),
            );
            code_generator.scopes.add_symbol_to_scope(
                &new_scope,
                patterns.last().unwrap().to_string(),
                SymTableEntry::Indirect(possible_switch_key.unwrap()),
            );
            code_generator.scope = old_scope;
            return (name, code_generator.basic_block);
        }
        *parent_closure.currently_executing.borrow_mut() = true;

        //This return basic block is the merge block
        //put scope in temp variable and load the scope of the function we are executing

        //the var we are going to put the result expression in
        let result_value;
        let this_closure_switch_key;
        let key_to_pass;

        match self.get_switch_key() {
            None => {
                this_closure_switch_key = possible_switch_key;
                key_to_pass = None;
            }
            Some(key) => {
                this_closure_switch_key = Some(key);
                key_to_pass = possible_switch_key;
            }
        }
        //if our closure has jumps
        if let Some(jump_point) = &self.jump_points {
            //create the basic blocks
            //One block for each branch (along with the condition for jumping to it)
            let entry_block = code_generator.context.append_basic_block(
                code_generator.function,
                &format!("entry_block{}", code_generator.sym_counter.increment()),
            );
            let previous_block = code_generator.basic_block;
            code_generator
                .builder
                .build_unconditional_branch(entry_block)
                .unwrap();
            code_generator.builder.position_at_end(entry_block);
            code_generator.basic_block = entry_block;

            let blocks: Vec<(
                inkwell::values::IntValue<'a>,
                inkwell::basic_block::BasicBlock<'a>,
            )> = jump_point
                .switches
                .iter()
                .map(|switch| {
                    (
                        code_generator.context.i32_type().const_int(*switch, false),
                        code_generator.context.append_basic_block(
                            code_generator.function,
                            &format!("br_block{}", code_generator.sym_counter.increment()),
                        ),
                    )
                })
                .collect();
            //One default- else case
            let default_block = code_generator.context.append_basic_block(
                code_generator.function,
                &format!("default_block{}", code_generator.sym_counter.increment()),
            );
            //After the execution come back to non-switched execution
            let merge_block = code_generator.context.append_basic_block(
                code_generator.function,
                &format!("merge_block{}", code_generator.sym_counter.increment()),
            );

            //flag for looking into tag

            let (tc, switch_type, constructor) = match code_generator
                .scopes
                .get_value_from_scope(&self.scope, &this_closure_switch_key.clone().unwrap())
                .unwrap()
            {
                SymTableEntry::Prim(PrimPtrs::Constructor(constr)) => (
                    constr.template_key.clone(),
                    constr.get_struct_type(code_generator).as_basic_type_enum(),
                    true,
                ),
                _ => (
                    "".to_string(),
                    code_generator.context.i32_type().as_basic_type_enum(),
                    false,
                ),
            };

            //will hold all entry_block phis, can be none if
            let mut phis = vec![];
            let mut entry_start: Vec<Argument<'_>> = vec![];
            match self.rec_check(&self.derivative, code_generator.source_code) {
                true => {
                    for name in &patterns[..patterns.len() - 1] {
                        let phi_name =
                            &format!("phi_{}_{}", name, code_generator.sym_counter.increment());

                        let pattern_type: BasicTypeEnum;

                        code_generator.get_and_evaluate_from_scope(name);
                        match code_generator.get_and_eval_indirect(name) {
                            SymTableEntry::Prim(prim) => {
                                match prim {
                                    PrimPtrs::Basic(_) => {
                                        pattern_type =
                                            code_generator.context.i32_type().as_basic_type_enum()
                                    }
                                    PrimPtrs::Constructor(cons) => {
                                        pattern_type = cons
                                            .get_struct_type(code_generator)
                                            .as_basic_type_enum()
                                    }
                                    _ => panic!("Did not expect string as pattern"),
                                }

                                entry_start.push(prim.to_argument())
                            }
                            _ => panic!("entry to struct must be int or constructor"),
                        }

                        let phi = code_generator
                            .builder
                            .build_phi(pattern_type, phi_name)
                            .unwrap();

                        code_generator.scopes.add_symbol_to_scope(
                            &code_generator.scope,
                            phi_name.to_string(),
                            SymTableEntry::delegate_phi(pattern_type, phi, tc.clone()),
                        );

                        code_generator.scopes.add_symbol_to_scope(
                            &code_generator.scope,
                            name.to_string(),
                            SymTableEntry::Indirect(phi_name.to_string()),
                        );
                        phis.push(phi);
                    }
                    let name: &String = this_closure_switch_key.as_ref().unwrap();
                    let phi_name =
                        &format!("phi_{}_{}", name, code_generator.sym_counter.increment());

                    let phi = code_generator
                        .builder
                        .build_phi(switch_type, phi_name)
                        .unwrap();

                    code_generator.scopes.add_symbol_to_scope(
                        &code_generator.scope,
                        phi_name.to_string(),
                        SymTableEntry::delegate_phi(switch_type, phi, tc.clone()),
                    );
                    code_generator.get_and_evaluate_from_scope(name);

                    match code_generator
                        .scopes
                        .get_value_from_scope(&code_generator.scope, name)
                        .unwrap()
                    {
                        SymTableEntry::Prim(prim) => entry_start.push(prim.to_argument()),
                        _ => panic!("entry to struct must be int or constructor"),
                    }

                    code_generator.scopes.add_symbol_to_scope(
                        &code_generator.scope,
                        name.to_string(),
                        SymTableEntry::Indirect(phi_name.to_string()),
                    );
                    phis.push(phi);
                }

                false => println!("non-recursive"),
            }

            //get value to switch on:
            // - value itself if switch key is int
            // - tag section of the struct if constructor

            let arg_to_switch = match code_generator
                .scopes
                .get_value_from_scope(&self.scope, &this_closure_switch_key.clone().unwrap())
                .unwrap()
            {
                SymTableEntry::Prim(PrimPtrs::Basic(
                    inkwell::values::BasicValueEnum::IntValue(int),
                )) => match phis.last() {
                    None => int,
                    Some(phi) => &phi.as_basic_value().into_int_value(),
                },

                //TODO: move payload to scope depending on tag
                SymTableEntry::Prim(PrimPtrs::Constructor(constr)) => &code_generator
                    .builder
                    .build_extract_value(
                        match phis.last() {
                            None => constr.struct_value,
                            Some(phi) => phi.as_basic_value().into_struct_value(),
                        },
                        0,
                        "tag",
                    )
                    .unwrap()
                    .into_int_value(),
                _ => panic!("Argument passed not supported"),
            };
            //create switch LLVM IR
            let _ = code_generator
                .builder
                .build_switch(*arg_to_switch, default_block, &blocks);

            //collect blocks only into mut vec
            let mut basic_block_list = blocks
                .iter()
                .map(|(_, x)| *x)
                .collect::<Vec<inkwell::basic_block::BasicBlock<'_>>>();
            let mut result_names_and_blocks: Vec<(String, inkwell::basic_block::BasicBlock)> =
                vec![];

            //generate code for cases
            let def = self.default_closure.as_ref();
            let first_pattern = parse_arg(*def.next_pattern(), code_generator);
            match def {
                ClosureUnion::Closure(clos) => {
                    //for each jump point execute it
                    for (subclosure, new_block) in zip(&jump_point.possible_asts, &basic_block_list)
                    {
                        code_generator.builder.position_at_end(*new_block);
                        code_generator.basic_block = *new_block;
                        let new_scope = code_generator.scopes.new_scope(Some(self.scope));

                        result_names_and_blocks.push(match subclosure {
                            ClosureUnion::Closure(subclosure) => subclosure.execute_ast(
                                code_generator,
                                new_scope,
                                key_to_pass.clone(),
                            ),
                            ClosureUnion::ClosureAux(subclosure) => (
                                code_generator.recursive_compile(&subclosure.ast),
                                *new_block,
                            ),
                        });
                    }

                    //populate default case
                    let new_scope = code_generator.scopes.new_scope(Some(self.scope));

                    code_generator.builder.position_at_end(default_block);
                    code_generator.basic_block = default_block;
                    result_names_and_blocks.push(clos.execute_ast(
                        code_generator,
                        new_scope,
                        key_to_pass,
                    ));
                }
                ClosureUnion::ClosureAux(clos) => {
                    //for each jump point execute it
                    for (subclosure, new_block) in zip(&jump_point.possible_asts, &basic_block_list)
                    {
                        code_generator.builder.position_at_end(*new_block);
                        code_generator.basic_block = *new_block;

                        self.rebind_match_key(
                            constructor,
                            code_generator,
                            &this_closure_switch_key,
                            first_pattern.clone(),
                        );

                        result_names_and_blocks.push(match subclosure {
                            ClosureUnion::Closure(subclosure) => subclosure.execute_ast(
                                code_generator,
                                code_generator.scope,
                                key_to_pass.clone(),
                            ),
                            ClosureUnion::ClosureAux(subclosure) => (
                                code_generator.recursive_compile(&subclosure.ast),
                                *new_block,
                            ),
                        });

                        //populate default case

                        code_generator.builder.position_at_end(default_block);
                        code_generator.basic_block = default_block;

                        self.rebind_match_key(
                            constructor,
                            code_generator,
                            &this_closure_switch_key,
                            first_pattern.clone(),
                        );
                        result_names_and_blocks
                            .push((code_generator.recursive_compile(&clos.ast), default_block));
                    }
                }
            }

            //jump back from default to merge block

            code_generator.builder.position_at_end(merge_block);
            code_generator.basic_block = merge_block;
            basic_block_list.push(default_block);

            code_generator.get_and_evaluate_from_scope(&result_names_and_blocks[0].0);
            //get the first return type (Can't be recursive because it needs a base case)
            match code_generator.get_and_eval_indirect(&result_names_and_blocks[0].0) {
                //if the return type is an integer then we build phi to take back return type and
                //return that phi value
                SymTableEntry::Prim(PrimPtrs::Basic(_)) => {
                    //build phi

                    //get values and finish branch back.
                    let return_value_iter: Vec<(
                        Result<IntValue<'a>, ScopeId>,
                        &inkwell::basic_block::BasicBlock<'_>,
                    )> = result_names_and_blocks
                        .iter()
                        .map(|(result, block)| {
                            (
                                self.end_branch(
                                    code_generator,
                                    block,
                                    &merge_block,
                                    &entry_block,
                                    result.to_string(),
                                ),
                                block,
                            )
                        })
                        .collect();
                    //if there are no recursive calls in the block then we just get the int values.
                    //if there are recursive calls, jump back to the entry and fall
                    let int_values: Vec<(IntValue<'a>, &inkwell::basic_block::BasicBlock<'_>)> =
                        match return_value_iter.iter().any(|(x, _)| x.is_err()) {
                            false => return_value_iter
                                .iter()
                                .map(|(x, y)| (x.unwrap(), *y))
                                .collect(),
                            true => {
                                //go to entry and execute args
                                code_generator.position_at_start(entry_block);

                                code_generator.basic_block = entry_block;

                                //build every argument in the entry.
                                let mut entry_vals = vec![];
                                match this_closure_switch_key {
                                    None => {
                                        for (arg, start) in zip(&patterns, entry_start) {
                                            code_generator.get_and_evaluate_from_scope(arg);
                                            //clone reference I think?
                                            entry_vals.push(vec![(start, &previous_block)]);
                                        }
                                    }
                                    Some(ref key) => {
                                        for (arg, start) in zip(&patterns, entry_start) {
                                            code_generator.get_and_evaluate_from_scope(arg);
                                            entry_vals.push(vec![(start, &previous_block)]);
                                        }
                                        code_generator.get_and_evaluate_from_scope(key);

                                        match code_generator.get_and_eval_indirect(key) {
                                            SymTableEntry::Prim(prim) => entry_vals
                                                .push(vec![(prim.to_argument(), &previous_block)]),

                                            _ => panic!("recursive return should be primitive "),
                                        }
                                    }
                                };

                                //the vals to send back to the merge value (if we only have one base
                                //case this should be 1 val)
                                let mut merge_vals = vec![];

                                //here we want the pairs of patterns, one from entry and one from
                                //the result of the branch.

                                for (x, y) in return_value_iter {
                                    match x {
                                        //if we have a return value add it to the merge phi
                                        Ok(int) => merge_vals.push((int, y)),
                                        //if we don't have a merge value, for every argument build a
                                        //phi for this branch and the entry
                                        Err(scope) => {
                                            for (phi_vec, pattern) in
                                                zip(entry_vals.iter_mut(), patterns.clone())
                                            {
                                                code_generator.scope = scope;
                                                match code_generator.get_and_eval_indirect(&pattern)
                                                {
                                                    SymTableEntry::Prim(prim) => {
                                                        phi_vec.push((prim.to_argument(), y))
                                                    }
                                                    //if we got a payload as a argument it must have
                                                    //the same type as the args ADT, cast to struct
                                                    SymTableEntry::Pointer(payload) => {
                                                        code_generator.builder.position_before(
                                                            &y.get_terminator().unwrap(),
                                                        );
                                                        let casted_payload = code_generator
                                                            .builder
                                                            .build_pointer_cast(
                                                                *payload,
                                                                switch_type.ptr_type(0.into()),
                                                                "cast_to_struct",
                                                            )
                                                            .unwrap();
                                                        let payload_as_struct = code_generator
                                                            .builder
                                                            .build_load(casted_payload, "payload")
                                                            .unwrap()
                                                            .into_struct_value();
                                                        phi_vec.push((
                                                            Argument::Const(payload_as_struct),
                                                            y,
                                                        ))
                                                    }

                                                    _ => panic!(
                                                        "recursive return should be primitive "
                                                    ),
                                                }
                                            }
                                        }
                                    }
                                }
                                code_generator.scope = new_scope;
                                code_generator.position_at_start(entry_block);

                                let basic_values: Vec<_> = entry_vals
                                    .iter()
                                    .map(|phi_input| {
                                        phi_input
                                            .iter()
                                            .map(|(x, y)| {
                                                (
                                                    match x {
                                                        Argument::Int(int) => {
                                                            int.as_basic_value_enum()
                                                        }
                                                        Argument::Const(cons) => {
                                                            cons.as_basic_value_enum()
                                                        }
                                                    },
                                                    **y,
                                                )
                                            })
                                            .collect()
                                    })
                                    .collect();

                                let phi_inputs: Vec<Vec<(&dyn BasicValue<'_>, BasicBlock<'_>)>> =
                                    basic_values
                                        .iter()
                                        .map(
                                            |inner_vec: &Vec<(
                                                BasicValueEnum<'a>,
                                                BasicBlock<'a>,
                                            )>| {
                                                inner_vec
                                                    .iter()
                                                    .map(|(x, y)| (x as &dyn BasicValue<'_>, *y))
                                                    .collect()
                                            },
                                        )
                                        .collect();

                                for (phi_input, phi) in zip(phi_inputs, phis) {
                                    phi.add_incoming(&phi_input);
                                }

                                merge_vals
                            }
                        };

                    code_generator.builder.position_at_end(merge_block);
                    let ref_block_pairs: Vec<(
                        &dyn inkwell::values::BasicValue<'_>,
                        inkwell::basic_block::BasicBlock<'_>,
                    )> = int_values
                        .iter()
                        .map(|(x, y)| (x as &dyn inkwell::values::BasicValue<'_>, **y))
                        .collect();
                    let phi = code_generator
                        .builder
                        .build_phi(
                            code_generator.context.i32_type(),
                            &format!("Result{}", code_generator.sym_counter.increment()),
                        )
                        .unwrap();

                    phi.add_incoming(&ref_block_pairs);

                    result_value =
                        SymTableEntry::int_to_entry(phi.as_basic_value().into_int_value());
                }

                //if we return a closure, build a new closure that switches on the branches from before.
                SymTableEntry::Clos(_) => {
                    let branch_indexes: Vec<String> = (0..basic_block_list.len())
                        .map(|x| code_generator.allocate_literal_int(x as i32))
                        .collect();

                    //Assume we can't recursively return closure

                    //Here we look at the return values that we got and jump to the merge block if
                    //we got a value or jump back to the start if we got a recursive entry
                    let int_values: Vec<inkwell::values::IntValue<'_>> =
                        zip(branch_indexes, &basic_block_list)
                            .map(|(x, block)| {
                                self.end_branch(
                                    code_generator,
                                    block,
                                    &merge_block,
                                    &entry_block,
                                    x,
                                )
                                .unwrap()
                            })
                            .collect();

                    code_generator.builder.position_at_end(merge_block);

                    let result_block_pairs: Vec<(
                        &dyn inkwell::values::BasicValue<'_>,
                        inkwell::basic_block::BasicBlock<'_>,
                    )> = zip(
                        int_values
                            .iter()
                            .map(|x| x as &dyn inkwell::values::BasicValue<'_>),
                        basic_block_list,
                    )
                    .collect();

                    let phi = code_generator
                        .builder
                        .build_phi(
                            code_generator.context.i32_type(),
                            &format!("Result{}", code_generator.sym_counter.increment()),
                        )
                        .unwrap();
                    phi.add_incoming(&result_block_pairs);
                    code_generator.scope = self.scope;

                    //add phi to scope
                    let phi_name = format!("phi{}", code_generator.sym_counter.increment());
                    code_generator.scopes.add_symbol_to_scope(
                        &old_scope,
                        phi_name.clone(),
                        SymTableEntry::Prim(PrimPtrs::Basic(phi.as_basic_value())),
                    );
                    let mut new_jump_points = JumpPoint::new();
                    for (branch_index, (result, _)) in result_names_and_blocks
                        .iter()
                        .enumerate()
                        .take(result_names_and_blocks.len() - 1)
                    {
                        new_jump_points.add_point(
                            branch_index as u64,
                            ClosureUnion::Closure(
                                code_generator
                                    .scopes
                                    .get_value_from_scope(&code_generator.scope, result)
                                    .unwrap()
                                    .clone()
                                    .get_closure_move()
                                    .unwrap(),
                            ),
                        );
                    }
                    let new_default = code_generator
                        .scopes
                        .get_value_from_scope(
                            &code_generator.scope,
                            &result_names_and_blocks[result_names_and_blocks.len() - 1].0,
                        )
                        .unwrap()
                        .clone()
                        .get_closure_move()
                        .unwrap();
                    let der = new_default.derivative.clone();
                    let mut new_closure = Closure::new_with_closure(
                        Some(new_jump_points),
                        new_default,
                        code_generator.scopes.new_scope(Some(self.scope)),
                        der,
                    );
                    new_closure.derivative = self.derivative.clone();
                    new_closure.set_switch_key(Some(phi_name.clone()));

                    result_value = SymTableEntry::closure_to_entry(new_closure);
                }
                _ => panic!("return value not int or closure"),
            }
        }
        //no jumps, just make the default
        else {
            let result_key = match self.default_closure.as_ref() {
                ClosureUnion::ClosureAux(clos) => code_generator.recursive_compile(&clos.ast),
                ClosureUnion::Closure(clos) => {
                    clos.execute_ast(code_generator, new_scope, key_to_pass).0
                }
            };

            result_value = code_generator
                .scopes
                .recieve_owned_entry(&code_generator.scope, &result_key)
                .unwrap()
                .0;
        }

        let parent_closure = code_generator
            .scopes
            .get_value_from_scope(&self.scope, &self.derivative)
            .unwrap()
            .get_closure()
            .unwrap();

        *parent_closure.currently_executing.borrow_mut() = false;
        code_generator.scope = old_scope;

        let return_key = format!("returned{}", code_generator.sym_counter.increment());
        code_generator.scopes.add_symbol_to_scope(
            &code_generator.scope,
            return_key.clone(),
            result_value,
        );

        (return_key, code_generator.basic_block)
    }
    //Build and add to scope values based on the branch
    fn unwrap_field(
        &self,
        constr_key: String,
        code_generator: &mut CodeGen<'a>,
        template_name: String,
    ) {
        //We get the argument literal, then get the ADT, then get the template at the branch we are at
        let constr_template = code_generator
            .scopes
            .get_value_from_scope(&self.scope, &template_name)
            .unwrap()
            .get_constructor_template()
            .unwrap();

        let adt_name = constr_template.type_loc.clone();
        let _ = code_generator
            .get_constructor_literal(constr_key.clone(), adt_name.clone())
            .unwrap();

        let adt = code_generator
            .scopes
            .get_value_from_scope(&self.scope, &adt_name)
            .unwrap()
            .get_adt()
            .unwrap();

        let branch_template = code_generator
            .scopes
            .get_value_from_scope(&code_generator.scope, &template_name)
            .unwrap()
            .get_constructor_template()
            .unwrap();
        let mut entries_to_add: Vec<(String, SymTableEntry)> = vec![];
        for (index, field) in adt.fields.iter().enumerate() {
            if branch_template.fields.iter().any(|x| x == field) {
                let constr_lit = code_generator
                    .get_constructor_literal_no_pointer_check(&constr_key)
                    .unwrap();
                let payload_value = code_generator
                    .builder
                    .build_extract_value(
                        constr_lit.struct_value,
                        (index + 1) as u32,
                        "extract_payload",
                    )
                    .unwrap()
                    .into_pointer_value();

                entries_to_add.push((field.to_string(), SymTableEntry::Pointer(payload_value)));
            }
        }
        for (key, entry) in entries_to_add {
            code_generator
                .scopes
                .add_symbol_to_scope(&code_generator.scope, key, entry);
        }
    }
    fn rec_check(&self, derivative: &str, sc: &[u8]) -> bool {
        Closure::rec_check_wrapped(&self.get_ast(), derivative, sc)
    }
    fn rec_check_wrapped(ast: &tree_sitter::Node<'_>, derivative: &str, sc: &[u8]) -> bool {
        match ast.grammar_name() {
            "function" => Closure::rec_check_wrapped(
                &ast.child_by_field_name("name").unwrap(),
                derivative,
                sc,
            ),
            "apply" => {
                Closure::rec_check_wrapped(
                    &ast.child_by_field_name("function").unwrap(),
                    derivative,
                    sc,
                ) || Closure::rec_check_wrapped(
                    &ast.child_by_field_name("argument").unwrap(),
                    derivative,
                    sc,
                )
            }
            "variable" => ast.utf8_text(sc).unwrap() == derivative,
            "literal" => false,
            "parens" => Closure::rec_check_wrapped(
                &ast.child_by_field_name("expression").unwrap(),
                derivative,
                sc,
            ),
            "infix" => {
                Closure::rec_check_wrapped(
                    &ast.child_by_field_name("left_operand").unwrap(),
                    derivative,
                    sc,
                ) || Closure::rec_check_wrapped(
                    &ast.child_by_field_name("right_operand").unwrap(),
                    derivative,
                    sc,
                )
            }

            _ => panic!(
                "Recursive check found unkown grammar node {}",
                ast.grammar_name()
            ),
        }
    }

    fn rebind_match_key(
        &self,
        constructor: bool,
        code_generator: &mut CodeGen<'a>,
        this_closure_switch_key: &Option<String>,
        template_name: String,
    ) {
        //if its a constructor, unwrap the struct and add fields to scope
        if constructor {
            self.unwrap_field(
                this_closure_switch_key.clone().unwrap(),
                code_generator,
                template_name,
            );
        }
        //else just add our last pattern to scope
        else {
            code_generator.scopes.add_symbol_to_scope(
                &self.scope,
                template_name,
                SymTableEntry::Indirect(this_closure_switch_key.clone().unwrap()),
            );
        }
    }
    fn end_branch(
        &self,
        code_generator: &mut CodeGen<'a>,
        block: &inkwell::basic_block::BasicBlock<'a>,
        merge_block: &inkwell::basic_block::BasicBlock<'a>,
        entry: &inkwell::basic_block::BasicBlock<'a>,
        result_name: String,
    ) -> Result<inkwell::values::IntValue<'a>, ScopeId> {
        code_generator.builder.position_at_end(*block);

        //peak at result, if its a rec_call then build back to entry
        match code_generator
            .scopes
            .get_value_from_scope(&code_generator.scope, &result_name)
            .unwrap()
        {
            SymTableEntry::Rec(scope) => {
                code_generator
                    .builder
                    .build_unconditional_branch(*entry)
                    .expect("unconditional branch build failed");

                Err(*scope)
            }
            _ => {
                let return_val = code_generator.get_int(result_name);
                code_generator
                    .builder
                    .build_unconditional_branch(*merge_block)
                    .expect("unconditional branch build failed");
                Ok(return_val)
            }
        }
    }
}
#[derive(Clone, Debug)]
pub struct ClosureAux<'a> {
    pub ast: Rc<tree_sitter::Node<'a>>,
    pub pattern_pointer: usize,
    pub patterns: Vec<Rc<tree_sitter::Node<'a>>>,
}
impl<'a> ClosureAux<'a> {
    pub fn new(ast: Rc<tree_sitter::Node<'a>>, patterns: Vec<Rc<tree_sitter::Node<'a>>>) -> Self {
        ClosureAux {
            ast,
            pattern_pointer: 0,
            patterns,
        }
    }
    pub fn next_pattern(&self) -> Rc<tree_sitter::Node<'a>> {
        self.patterns[self.pattern_pointer].clone()
    }
}

#[derive(Clone, Debug)]
pub enum PrimPtrs<'a> {
    Basic(inkwell::values::BasicValueEnum<'a>),
    Global(inkwell::values::GlobalValue<'a>),
    Constructor(ConstructorLiteral<'a>),
}
impl<'a> PrimPtrs<'a> {
    pub fn to_argument(&self) -> Argument<'a> {
        match self {
            PrimPtrs::Basic(basic) => Argument::Int(basic.into_int_value()),
            PrimPtrs::Constructor(cons) => Argument::Const(cons.struct_value),
            _ => panic!("Argument should be "),
        }
    }
}

#[derive(Clone, Debug)]
pub enum SymTableEntry<'a> {
    Clos(Closure<'a>),
    AdtDef(ADT<'a>),
    GenClos(class::GeneralisedClosure),
    ConstructorTemplate(data_constructors::Constructor),
    Prim(PrimPtrs<'a>),
    Ast(Rc<tree_sitter::Node<'a>>),
    Pointer(PointerValue<'a>),
    Indirect(String),
    Rec(ScopeId), //scope we get the child items from.
}

impl<'a> SymTableEntry<'a> {
    pub fn _get_str(&self) -> Result<&inkwell::values::GlobalValue<'a>, String> {
        match self {
            SymTableEntry::Prim(prim) => match prim {
                PrimPtrs::Global(global_ptr) => Ok(global_ptr),
                _ => Err("Expected String not other basic".to_string()),
            },
            _ => Err("Expected String".to_string()),
        }
    }

    pub fn get_ast(&self) -> Result<Rc<tree_sitter::Node<'a>>, String> {
        match self {
            SymTableEntry::Ast(ast) => Ok(ast.clone()),
            _ => Err("Expected frozen computation".to_string()),
        }
    }
    pub fn _get_ptr(&self) -> Result<&inkwell::values::PointerValue<'a>, String> {
        match self {
            SymTableEntry::Prim(PrimPtrs::Basic(basic_ptr)) => match basic_ptr {
                inkwell::values::BasicValueEnum::PointerValue(ptr) => Ok(ptr),
                _ => Err("Expected pointer not other basic".to_string()),
            },
            _ => Err("Expected pointer".to_string()),
        }
    }
    pub fn get_adt(&self) -> Result<&ADT<'a>, String> {
        match self {
            SymTableEntry::AdtDef(adt) => Ok(adt),
            _ => Err("Expected ADT".to_string()),
        }
    }
    pub fn get_constructor_template(&self) -> Result<&Constructor, String> {
        match self {
            SymTableEntry::ConstructorTemplate(cons) => Ok(cons),
            _ => Err("Expected Constructor Template".to_string()),
        }
    }

    pub fn get_closure(&self) -> Result<&Closure<'a>, String> {
        match self {
            SymTableEntry::Clos(closure) => Ok(closure),
            _ => Err("Expected closure".to_string()),
        }
    }
    pub fn get_closure_move(self) -> Result<Closure<'a>, String> {
        match self {
            SymTableEntry::Clos(closure) => Ok(closure),
            _ => Err("Expected closure".to_string()),
        }
    }
    pub fn _get_generalised_closure(&self) -> Result<&GeneralisedClosure, String> {
        match self {
            SymTableEntry::GenClos(closure) => Ok(closure),
            _ => Err("Expected closure".to_string()),
        }
    }
    pub fn global_to_entry(ptr: inkwell::values::GlobalValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Global(ptr))
    }
    pub fn pointer_to_entry(ptr: inkwell::values::PointerValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Basic(
            inkwell::values::BasicValueEnum::PointerValue(ptr),
        ))
    }
    pub fn int_to_entry(ptr: inkwell::values::IntValue<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Basic(inkwell::values::BasicValueEnum::IntValue(
            ptr,
        )))
    }
    pub fn closure_to_entry(closure: Closure<'a>) -> Self {
        SymTableEntry::Clos(closure)
    }
    pub fn adt_def_to_entry(adt: ADT<'a>) -> Self {
        SymTableEntry::AdtDef(adt)
    }
    pub fn generalised_closure_to_entry(closure: GeneralisedClosure) -> Self {
        SymTableEntry::GenClos(closure)
    }
    pub fn adt_constructor_to_entry(constructor: ConstructorLiteral<'a>) -> Self {
        SymTableEntry::Prim(PrimPtrs::Constructor(constructor))
    }
    pub fn adt_constructor_template_to_entry(constructor: data_constructors::Constructor) -> Self {
        SymTableEntry::ConstructorTemplate(constructor)
    }
    pub fn delegate_phi(
        ty: BasicTypeEnum,
        phi: PhiValue<'a>,
        template_key: String,
    ) -> SymTableEntry<'a> {
        match ty {
            BasicTypeEnum::IntType(_) => {
                SymTableEntry::int_to_entry(phi.as_basic_value().into_int_value())
            }
            BasicTypeEnum::StructType(_) => SymTableEntry::adt_constructor_to_entry(
                ConstructorLiteral::new(phi.as_basic_value().into_struct_value(), template_key),
            ),
            _ => panic!("Unexpected Type"),
        }
    }
}

pub fn tree_to_children(code_ast: tree_sitter::Node) -> Vec<tree_sitter::Node> {
    return code_ast.children(&mut code_ast.walk()).collect();
}
pub fn parse_arg(ast: tree_sitter::Node, code_gen: &CodeGen) -> String {
    match ast.grammar_name() {
        "name" | "variable" => ast.utf8_text(code_gen.source_code).unwrap().to_string(),
        "parens" => parse_arg(ast.child_by_field_name("pattern").unwrap(), code_gen),
        "apply" => parse_arg(ast.child_by_field_name("function").unwrap(), code_gen),
        _ => panic!("unseen argument format {}", ast.grammar_name()),
    }
}
