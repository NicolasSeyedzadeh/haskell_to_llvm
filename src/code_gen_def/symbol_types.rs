use inkwell::values::IntValue;

use super::class;
use super::class::GeneralisedClosure;
use super::data_constructors::Constructor;
use super::data_constructors::ADT;
use super::scoping;
use super::scoping::ScopeId;
use super::CodeGen;
use std::iter::zip;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub enum ClosureUnion<'a> {
    Closure(Closure<'a>),
    ClosureAux(ClosureAux<'a>),
}

#[derive(Clone, Debug)]
pub enum SwitchType {
    Int,
    Constructor,
}
#[derive(Clone, Debug)]
pub struct JumpPoint<'a> {
    pub switch_type: SwitchType,
    pub switch_key: Option<String>,
    switches: Vec<u64>,
    possible_asts: Vec<ClosureUnion<'a>>,
}
impl<'a> JumpPoint<'a> {
    pub fn new(switch_type: SwitchType) -> Self {
        JumpPoint {
            switch_type,
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
pub struct Closure<'a> {
    //only allowing one jump point right now
    pub jump_points: Option<JumpPoint<'a>>,
    pub default_closure: Box<ClosureUnion<'a>>,
    pub scope: scoping::ScopeId,
}
impl<'a> Closure<'a> {
    pub fn new(
        jump_points: Option<JumpPoint<'a>>,
        default_closure: ClosureAux<'a>,
        scope: scoping::ScopeId,
    ) -> Self {
        Closure {
            jump_points,
            default_closure: Box::new(ClosureUnion::ClosureAux(default_closure)),
            scope,
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
        self.jump_points.as_ref().unwrap().switch_key.clone()
    }
    pub fn new_with_union(
        jump_points: Option<JumpPoint<'a>>,
        default_closure: Box<ClosureUnion<'a>>,
        scope: scoping::ScopeId,
    ) -> Self {
        Closure {
            jump_points,
            default_closure,
            scope,
        }
    }
    pub fn new_with_closure(
        jump_points: Option<JumpPoint<'a>>,
        default_closure: Closure<'a>,
        scope: scoping::ScopeId,
    ) -> Self {
        Closure {
            jump_points,
            default_closure: Box::new(ClosureUnion::Closure(default_closure)),
            scope,
        }
    }
    pub fn clone_with_new_patterns(&self, new_scope: scoping::ScopeId) -> Closure<'a> {
        let mut clone = self.clone();
        clone.new_patterns();
        clone.scope = new_scope;
        clone
    }
    pub fn replace_default(&mut self, new_default: ClosureUnion<'a>) -> ClosureUnion<'a> {
        mem::replace(&mut self.default_closure, new_default)
    }
    pub fn new_patterns(&mut self) {
        match self.default_closure.as_mut() {
            ClosureUnion::Closure(nested) => nested.new_patterns(),
            ClosureUnion::ClosureAux(x) => x.patterns = x.patterns.clone()[1..].to_vec(),
        }
    }
    pub fn patterns_left(&self) -> usize {
        match self.default_closure.as_ref() {
            ClosureUnion::Closure(nested) => nested.patterns_left(),
            ClosureUnion::ClosureAux(x) => x.patterns.len(),
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
    fn get_ast(&self) -> Rc<tree_sitter::Node<'a>> {
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
        //This return basic block is the merge block
        //put scope in temp variable and load the scope of the function we are executing
        let old_scope = code_generator.scope;
        code_generator.scope = new_scope;

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
            //create switch LLVM IR
            //code_generator.scopes.debug_print_in_scope(&self.scope);
            let _ = code_generator.builder.build_switch(
                *code_generator
                    .scopes
                    .get_value_from_scope(&self.scope, &this_closure_switch_key.unwrap())
                    .unwrap()
                    .get_int()
                    .unwrap(),
                default_block,
                &blocks,
            );

            //collect blocks only into mut vec
            let mut basic_block_list = blocks
                .iter()
                .map(|(_, x)| *x)
                .collect::<Vec<inkwell::basic_block::BasicBlock<'_>>>();
            let mut result_names_and_blocks: Vec<(String, inkwell::basic_block::BasicBlock)> =
                vec![];

            //generate code for cases
            match self.default_closure.as_ref() {
                //if we are trying to compile from a returned closure
                ClosureUnion::Closure(clos) => {
                    //for each jump point execute it
                    println!("ahsjdgakjbdkja");

                    println!("{:?}", clos);

                    for (subclosure, new_block) in zip(&jump_point.possible_asts, &basic_block_list)
                    {
                        code_generator.builder.position_at_end(*new_block);
                        code_generator.basic_block = *new_block;
                        result_names_and_blocks.push(match subclosure {
                            ClosureUnion::Closure(subclosure) => subclosure.execute_ast(
                                code_generator,
                                subclosure.scope,
                                key_to_pass.clone(),
                            ),
                            ClosureUnion::ClosureAux(subclosure) => (
                                code_generator.recursive_compile(&subclosure.ast),
                                *new_block,
                            ),
                        });
                        code_generator
                            .builder
                            .build_unconditional_branch(merge_block)
                            .expect("unconditional branch build failed");
                    }

                    //populate default case
                    code_generator.builder.position_at_end(default_block);
                    code_generator.basic_block = default_block;
                    result_names_and_blocks.push(clos.execute_ast(
                        code_generator,
                        clos.scope,
                        key_to_pass,
                    ));
                }
                //if we are trying to compile from a collected closure
                ClosureUnion::ClosureAux(clos) => {
                    //for each jump point execute it
                    for (subclosure, new_block) in zip(&jump_point.possible_asts, &basic_block_list)
                    {
                        code_generator.builder.position_at_end(*new_block);
                        code_generator.basic_block = *new_block;
                        result_names_and_blocks.push(match subclosure {
                            ClosureUnion::Closure(subclosure) => subclosure.execute_ast(
                                code_generator,
                                subclosure.scope,
                                key_to_pass.clone(),
                            ),
                            ClosureUnion::ClosureAux(subclosure) => (
                                code_generator.recursive_compile(&subclosure.ast),
                                *new_block,
                            ),
                        });

                        code_generator
                            .builder
                            .build_unconditional_branch(merge_block)
                            .expect("unconditional branch build failed");
                    }
                    //populate default case
                    code_generator.builder.position_at_end(default_block);
                    code_generator.basic_block = default_block;
                    result_names_and_blocks
                        .push((code_generator.recursive_compile(&clos.ast), default_block));
                }
            }
            //jump back from default to merge block
            code_generator
                .builder
                .build_unconditional_branch(merge_block)
                .expect("unconditional branch build failed");

            code_generator.builder.position_at_end(merge_block);
            code_generator.basic_block = merge_block;
            basic_block_list.push(default_block);

            //get the first return type
            match code_generator
                .scopes
                .get_value_from_scope(&code_generator.scope, &result_names_and_blocks[0].0)
                .unwrap()
            {
                //if the return type is an integer then we build phi to take back return type and
                //return that phi value
                SymTableEntry::Prim(PrimPtrs::Basic(_)) => {
                    //build phi

                    let phi = code_generator
                        .builder
                        .build_phi(
                            code_generator.context.i32_type(),
                            &format!("Result{}", code_generator.sym_counter.increment()),
                        )
                        .unwrap(); //TODO: check whether our use case would ever have a different type
                    let result_block_pairs: Vec<(
                        &dyn inkwell::values::BasicValue<'_>,
                        inkwell::basic_block::BasicBlock<'_>,
                    )> = result_names_and_blocks
                        .iter()
                        .map(|(x, y)| {
                            (
                                code_generator
                                    .scopes
                                    .get_value_from_scope(&code_generator.scope, x)
                                    .unwrap()
                                    .get_int()
                                    .unwrap()
                                    as &dyn inkwell::values::BasicValue,
                                *y,
                            )
                        })
                        .collect();
                    phi.add_incoming(&result_block_pairs);
                    result_value =
                        SymTableEntry::int_to_entry(phi.as_basic_value().into_int_value());
                }

                //if we return a closure, build a new closure that switches on the branches from before.
                SymTableEntry::Clos(_) => {
                    let phi = code_generator
                        .builder
                        .build_phi(
                            code_generator.context.i32_type(),
                            &format!("Result{}", code_generator.sym_counter.increment()),
                        )
                        .unwrap();
                    let branch_indexes: Vec<String> = (0..basic_block_list.len())
                        .map(|x| code_generator.allocate_literal_int(x as i32))
                        .collect();
                    let result_block_pairs: Vec<(
                        &dyn inkwell::values::BasicValue<'_>,
                        inkwell::basic_block::BasicBlock<'_>,
                    )> = zip(
                        branch_indexes.into_iter().map(|x| {
                            code_generator
                                .scopes
                                .get_value_from_scope(&code_generator.scope, &x)
                                .unwrap()
                                .get_int()
                                .unwrap()
                                as &dyn inkwell::values::BasicValue
                        }),
                        basic_block_list,
                    )
                    .collect();
                    phi.add_incoming(&result_block_pairs);
                    //add phi to scope
                    let phi_name = format!("phi{}", code_generator.sym_counter.increment());
                    code_generator.scopes.add_symbol_to_scope(
                        &old_scope,
                        phi_name.clone(),
                        SymTableEntry::Prim(PrimPtrs::Basic(phi.as_basic_value())),
                    );
                    let mut new_jump_points = JumpPoint::new(SwitchType::Int);
                    for branch_index in 0..result_names_and_blocks.len() - 1 {
                        new_jump_points.add_point(
                            branch_index as u64,
                            ClosureUnion::Closure(
                                code_generator
                                    .scopes
                                    .get_value_from_scope(
                                        &code_generator.scope,
                                        &result_names_and_blocks[branch_index].0,
                                    )
                                    .unwrap()
                                    .clone()
                                    .get_closure_move()
                                    .unwrap(),
                            ),
                        );
                    }
                    let mut new_default = code_generator
                        .scopes
                        .get_value_from_scope(
                            &code_generator.scope,
                            &result_names_and_blocks[result_names_and_blocks.len() - 1].0,
                        )
                        .unwrap()
                        .clone()
                        .get_closure_move()
                        .unwrap();
                    let mut new_closure = Closure::new_with_closure(
                        Some(new_jump_points),
                        new_default,
                        code_generator.scopes.new_scope(Some(self.scope)),
                    );
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
                .unwrap();
        }

        code_generator.scope = old_scope;

        let return_key = format!("returned{}", code_generator.sym_counter.increment());
        code_generator.scopes.add_symbol_to_scope(
            &code_generator.scope,
            return_key.clone(),
            result_value,
        );

        (return_key, code_generator.basic_block)
    }
}
#[derive(Clone, Debug)]
pub struct ClosureAux<'a> {
    pub ast: Rc<tree_sitter::Node<'a>>,
    pub patterns: Vec<Rc<tree_sitter::Node<'a>>>,
}
impl<'a> ClosureAux<'a> {
    pub fn new(ast: Rc<tree_sitter::Node<'a>>, patterns: Vec<Rc<tree_sitter::Node<'a>>>) -> Self {
        ClosureAux { ast, patterns }
    }
    pub fn next_pattern(&self) -> Rc<tree_sitter::Node<'a>> {
        self.patterns.first().unwrap().clone()
    }
}

#[derive(Clone, Debug)]
pub enum PrimPtrs<'a> {
    Basic(inkwell::values::BasicValueEnum<'a>),
    Global(inkwell::values::GlobalValue<'a>),
    AdtConst(Constructor),
}
#[derive(Clone, Debug)]
pub enum SymTableEntry<'a> {
    Clos(Closure<'a>),
    AdtDef(ADT<'a>),
    GenClos(class::GeneralisedClosure),
    Prim(PrimPtrs<'a>),
    Ast(Rc<tree_sitter::Node<'a>>),
}

impl<'a> SymTableEntry<'a> {
    pub fn get_str(&self) -> Result<&inkwell::values::GlobalValue<'a>, String> {
        match self {
            SymTableEntry::Prim(prim) => match prim {
                PrimPtrs::Global(global_ptr) => Ok(global_ptr),
                _ => Err("Expected String".to_string()),
            },
            _ => Err("Expected String".to_string()),
        }
    }
    pub fn get_int(&self) -> Result<&inkwell::values::IntValue<'a>, String> {
        match self {
            SymTableEntry::Prim(PrimPtrs::Basic(basic_ptr)) => match basic_ptr {
                inkwell::values::BasicValueEnum::IntValue(int_ptr) => Ok(int_ptr),
                _ => Err("Expected Int not other basic".to_string()),
            },
            _ => Err("Expected Int".to_string()),
        }
    }
    pub fn get_ast(&self) -> Result<Rc<tree_sitter::Node<'a>>, String> {
        match self {
            SymTableEntry::Ast(ast) => Ok(ast.clone()),
            _ => Err("Expected frozen computation".to_string()),
        }
    }
    pub fn get_ptr(&self) -> Result<&inkwell::values::PointerValue<'a>, String> {
        match self {
            SymTableEntry::Prim(PrimPtrs::Basic(basic_ptr)) => match basic_ptr {
                inkwell::values::BasicValueEnum::PointerValue(ptr) => Ok(ptr),
                _ => Err("Expected pointer not other basic".to_string()),
            },
            _ => Err("Expected pointer".to_string()),
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
    pub fn get_generalised_closure(&self) -> Result<&GeneralisedClosure, String> {
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
    pub fn adt_constructor_to_entry(constructor: Constructor) -> Self {
        SymTableEntry::Prim(PrimPtrs::AdtConst(constructor))
    }
}

pub fn tree_to_children(code_ast: tree_sitter::Node) -> Vec<tree_sitter::Node> {
    return code_ast.children(&mut code_ast.walk()).collect();
}
