use haskell_to_llvm::compile;
use std::fs;
fn main() {
    //TODO: make this come from the CLI

    let source_code =
        fs::read_to_string("/home/nicolas/documents/dissertation/haskell_to_llvm/src/test.hs")
            .unwrap();
    println!("{}", source_code);
    compile(&source_code);
}
