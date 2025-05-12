use haskell_to_llvm::compile;
use std::env;
fn main() {
    //get CLI args
    let args: Vec<String> = env::args().collect();

    match args.get(1) {
        //if we dont have 2 args then use the default test path for debugging and leave it in LLVM IR
        None => compile("src/test.hs", "out.ll"),
        // if we got 2 CLI args then they should be the source and destinations so we can compile them and then
        // call llc to compile the IR output
        Some(x) => {
            let dest = x.to_string();
            compile(&args[2], &(dest.clone() + ".ll"));
            std::process::Command::new("llc")
                .arg(dest.clone() + ".ll")
                .arg("-relocation-model=pic")
                .arg("-o")
                .arg(dest.clone() + ".s")
                .output()
                .expect("llc failure");
            std::process::Command::new("gcc")
                .arg(dest.clone() + ".s")
                .arg("-o")
                .arg(dest.clone())
                .output()
                .expect("gcc failure");
        }
    }
}
