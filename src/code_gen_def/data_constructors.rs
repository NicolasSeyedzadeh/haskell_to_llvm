use inkwell::values::StructValue;

use super::CodeGen;

#[derive(Clone, Debug)]
pub struct ADT<'a> {
    name: String,
    pub fields: Vec<String>,
    pub type_llvm: inkwell::types::StructType<'a>,
    pub constructor_names: Vec<String>,
}
#[derive(Clone, Debug)]
pub struct Constructor {
    //maybe make this not a vec as it's values are known and not changed at compile time
    pub fields: Vec<String>,
    pub type_loc: String, //this should be the key of the relevant ADT in the scope they are both within
    union_number: usize,
}
#[derive(Clone, Debug)]
pub struct ConstructorLiteral<'a> {
    pub struct_value: StructValue<'a>,
    pub template_key: String,
}

impl<'a> ADT<'a> {
    pub fn new(
        name: String,
        fields: Vec<String>,
        type_llvm: inkwell::types::StructType<'a>,
        constructor_names: Vec<String>,
    ) -> Self {
        ADT {
            name,
            fields,
            type_llvm,
            constructor_names,
        }
    }
}

impl Constructor {
    pub fn new(fields: Vec<String>, type_loc: String, union_number: usize) -> Self {
        Constructor {
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
    ) -> (String, Self) {
        let name = ast
            .child_by_field_name("name")
            .unwrap()
            .utf8_text(code_generator.source_code)
            .unwrap()
            .to_string();
        let fields_ast = ast.child_by_field_name("field");

        let fields = match fields_ast {
            None => vec![],
            //used to have tree_to_children here but the field is duplicated and so we can't easily
            //separate them and the rest of the code doesn't really work with multiple fields for
            //one constructor
            Some(fields_ast) => vec![fields_ast
                .utf8_text(code_generator.source_code)
                .unwrap()
                .to_string()],
        };

        (name, Constructor::new(fields, type_loc, union_number))
    }
    pub fn get_tag(&self) -> u64 {
        self.union_number as u64
    }
}

impl<'a> ConstructorLiteral<'a> {
    pub fn new(struct_value: StructValue<'a>, template_key: String) -> Self {
        ConstructorLiteral {
            struct_value,
            template_key,
        }
    }
}
