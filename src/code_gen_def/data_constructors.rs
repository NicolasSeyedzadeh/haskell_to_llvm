use super::symbol_types;
use super::CodeGen;

#[derive(Clone, Debug)]
pub struct ADT<'a> {
    name: String,
    fields: Vec<String>,
    type_llvm: inkwell::types::StructType<'a>,
}
#[derive(Clone, Debug)]
pub struct Constructor {
    pub name: String,
    //maybe make this not a vec as it's values are known and not changed at compile time
    fields: Vec<String>,
    type_loc: String, //this should be the key of the relevant ADT in the scope they are both within
    union_number: usize,
}

impl<'a> ADT<'a> {
    pub fn new(
        name: String,
        fields: Vec<String>,
        type_llvm: inkwell::types::StructType<'a>,
    ) -> Self {
        ADT {
            name,
            fields,
            type_llvm,
        }
    }
}

impl Constructor {
    pub fn new(name: String, fields: Vec<String>, type_loc: String, union_number: usize) -> Self {
        Constructor {
            name,
            fields,
            type_loc,
            union_number,
        }
    }
    pub fn parse_from_ast<'a>(
        ast: &tree_sitter::Node<'a>,
        code_generator: &mut CodeGen<'a>,
        type_loc: String,
        union_number: usize,
    ) -> Self {
        let name = code_generator.recursive_compile(&ast.child_by_field_name("name").unwrap());
        let fields_ast = ast.child_by_field_name("field");
        let fields = match fields_ast {
            None => vec![],
            Some(fields_ast) => symbol_types::tree_to_children(fields_ast)
                .iter()
                .map(|x| code_generator.recursive_compile(x))
                .collect(),
        };
        Constructor::new(name, fields, type_loc, union_number)
    }
}
