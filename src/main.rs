use tree_sitter::{InputEdit, Language, Parser, Point};

fn main() {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_haskell::LANGUAGE.into()).expect("Error loading Haskell grammar");
    let source_code = "sumtorial :: Integer -> Integer\nsumtorial 0 = 0\nsumtorial n = n + sumtorial (n - 1)";
    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();

    let ast_as_str=root_node.to_string();
    println!("{}",ast_as_str);

}
