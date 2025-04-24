use super::scoping;
use std::collections::HashMap;

//generalised in that once we recieve a type we need to know which instance to use, but this info is
//not necessarily known before all arguments are provided
#[derive(Clone, Debug)]
pub struct GeneralisedClosure {
    typing: Vec<usize>, //the numbers here refer to the index of the patterns
    possible_closures: HashMap<String, String>, //string1 here is the ADT' name in it's scope and the second string is the closures or generailised closure's name in the scope
    patterns: Vec<String>,
    scope: scoping::ScopeId,
}

impl GeneralisedClosure {
    pub fn new(
        typing: &tree_sitter::Node,
        patterns: Vec<String>,
        scope: scoping::ScopeId,
        source: &[u8],
    ) -> Self {
        let ty = &mut vec![];
        GeneralisedClosure::parse_type(typing, ty, source, &patterns);
        GeneralisedClosure {
            typing: ty.to_vec(),
            possible_closures: HashMap::new(),
            patterns,
            scope,
        }
    }
    fn register_closure(&mut self, adt: String, closure: String) {
        self.possible_closures.insert(adt, closure);
    }

    fn parse_type(
        node: &tree_sitter::Node,
        inplace: &mut Vec<usize>,
        source: &[u8],
        patterns: &Vec<String>,
    ) {
        match node.grammar_name() {
            "function" => {
                GeneralisedClosure::parse_type(
                    &node.child_by_field_name("parameter").unwrap(),
                    inplace,
                    source,
                    patterns,
                );
                GeneralisedClosure::parse_type(
                    &node.child_by_field_name("result").unwrap(),
                    inplace,
                    source,
                    patterns,
                );
            }
            "apply" => {
                let application = node
                    .child_by_field_name("constructor")
                    .unwrap()
                    .utf8_text(source)
                    .unwrap();
                for pattern_index in 0..patterns.len() {
                    if application == patterns[pattern_index] {
                        inplace.push(pattern_index + 1);
                        break;
                    }
                }
            }
            "variable" => inplace.push(0),
            "parens" => inplace.push(0), //TODO: implement switching on providing a function that has the monadic type

            _ => panic!("Unimplemented type node: {}", node.grammar_name()),
        }
    }
    /*pub register_closure(self,closure){

    }*/
}
