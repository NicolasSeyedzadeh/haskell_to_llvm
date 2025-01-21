#[test]
fn test_let() {
    let source_code = "let x = 200 in x";
    haskell_to_llvm::compile(source_code)
}
