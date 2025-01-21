use regex::Regex;
use std::process::Command;
use std::rc::Rc;

#[derive(Clone)]
enum PrimType {
    Int,
    Str,
    Bool,
    Union,
    Other(String),
}
#[derive(Clone)]
struct FunctionType {
    input: Rc<HType>,
    output: Rc<HType>,
}
#[derive(Clone)]
enum HType {
    Prim(PrimType),
    Func(FunctionType),
}

impl HType {
    fn to_string(&self) -> String {
        match self {
            HType::Prim(p) => "prim: ".to_string() + &p.to_string(),
            HType::Func(f) => "func: ".to_string() + &f.to_string(),
        }
    }
}
impl PrimType {
    fn to_string(&self) -> String {
        match self {
            PrimType::Int => "Int".to_string(),
            PrimType::Str => "Str".to_string(),
            PrimType::Bool => "Bool".to_string(),
            PrimType::Union => "Un".to_string(),
            PrimType::Other(ty) => ty.clone(),
        }
    }
}
impl FunctionType {
    fn to_string(&self) -> String {
        self.input.to_string() + "->" + self.output.to_string().as_str()
    }
}

pub fn get_types(file_name: &str) {
    // Run ghc -c -ddump-types -fforce-recomp
    let output = Command::new("ghc")
        .args(["-c", "-ddump-types", "-fforce-recomp", file_name])
        .output()
        .expect("Failed to execute GHC");

    // Check for errors
    if !output.status.success() {
        eprintln!("Error: {}", String::from_utf8_lossy(&output.stderr));
        return;
    }

    // Capture the output
    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("Raw Output:\n{}", stdout);

    // Parse the output to extract type information
    let type_info = parse_ghc_ddump_types(&stdout);
    for (name, ty) in type_info {
        println!("Name: {}, Type: {}", name, ty.to_string());
    }
}

// Function to parse the output of --ddump-types
fn parse_ghc_ddump_types(output: &str) -> Vec<(String, HType)> {
    let mut results = Vec::new();

    // Regex to match the type declarations
    let re = Regex::new(r"([a-zA-Z_][a-zA-Z0-9_]*) :: (.+)").unwrap();

    for line in output.lines() {
        if let Some(captures) = re.captures(line) {
            let name = captures[1].to_string();
            let ty = split_function(captures[2].to_string());
            results.push((name, ty));
        }
    }

    results
}
//turn a single result into a type by splitting on
fn split_function(ty: String) -> HType {
    let typed_parts = ty
        .split(" -> ")
        .map(single_string_to_htype)
        .collect::<Vec<HType>>();
    if typed_parts.len() == 1 {
        typed_parts[0].clone()
    } else {
        let mut function = HType::Func(FunctionType {
            input: Rc::new(typed_parts[0].clone()),
            output: Rc::new(typed_parts[1].clone()),
        });
        for part in &typed_parts[2..] {
            function = HType::Func(FunctionType {
                input: Rc::new(function),
                output: Rc::new(part.clone()),
            })
        }
        function
    }
}
fn single_string_to_htype(ty: &str) -> HType {
    match ty {
        "Integer" => HType::Prim(PrimType::Int),
        "Boolean" => HType::Prim(PrimType::Bool),
        "String" => HType::Prim(PrimType::Str),
        _ => HType::Prim(PrimType::Other(ty.to_string())),
    }
}
