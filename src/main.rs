use haskell_to_llvm::compile;
fn main() {
    //TODO: make this come from the CLI
    let path = "src/test.hs";

    compile(path);
}
