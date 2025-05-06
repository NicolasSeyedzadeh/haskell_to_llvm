use haskell_to_llvm::compile;
use std::env;
fn main() {
    //TODO: make this come from the CLI
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        None => compile("src/test.hs", "out.ll"),
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
