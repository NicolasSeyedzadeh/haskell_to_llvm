# Direct Haskell to LLVM IR compiler

A lightweight Rusty compiler that translates a subset of Haskell directly to LLVM IR.

## Usage

After compiling the Rust code with `cargo build --release` at the top level directory, you can then
run the code by invoking `./target/release/haskell_to_llvm {destination path} {source path }`. If
you provide no arguments the code will default to trying to compile the code in src/test.hs to a .ll
file.

## Project Structure

### src/:

main.rs - This is the code that is run when we run invoke the executable, it's main job is to
collect the command line arguments.

lib.rs - This contains the `compile` function that takes in a source and destination path, parses
the source creates the Inkwell objects needed for IR generation puts it into a code_generator object
and then calls the recursive compilation method on the AST.

code_gen_def.rs - This defines the code_generator object that will perform the recursion and holds
global inkwell objects that are required for generation.

types.rs - Incomplete type checker borrowed from GHC, not used in the rest of the project.

code_gen_def/recurse.rs - Defines the process of recursion, and what occurs when an AST structure is
caught.

code_gen_def/function_behaviour.rs - Defines helper functions for the recursive compilation

code_gen_def/scoping.rs - Defines scoping structs in the code

code_gen_def/symbol_types.rs - Defines types that can be held in the scopes, including
closures and their execution methods

code_gen_def/counter.rs - Simple counter definition for unique variable names.
code_gen_def/class.rs - Incomplete type class definition.

code_gen_def/data_constructors.rs - Defines the structs to represent ADTs, their constructors
templates and literals

### evaluation/:

Samplex/ - Contains the Haskell program outlined in the dissertation and their translated versions C
and OCaml versions.

results_plot/ - contains python files that plot the results of the evaluation.

## License

This project is licensed under the MIT License.
