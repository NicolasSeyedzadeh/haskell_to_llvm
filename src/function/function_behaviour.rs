pub enum FunctionBehaviour {
    PutStrLn,
    Defined,
}
impl FunctionBehaviour {
    pub fn string_to_func(string: &str) -> Self {
        match string {
            "putStrLn" => FunctionBehaviour::PutStrLn,
            _ => FunctionBehaviour::Defined,
        }
    }
}
