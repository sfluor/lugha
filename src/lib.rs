use eval::{exec, EvalError};
use wasm_bindgen::prelude::*;

pub mod eval;
pub mod lexer;
pub mod parser;

// TODO: return errors
#[wasm_bindgen(catch)]
pub fn run(code: String) -> Result<String, EvalError> {
    exec(&code)
}

#[wasm_bindgen]
pub fn get_keywords() -> Vec<String> {
    lexer::KEYWORDS
        .clone()
        .map(|(name, _)| name.to_string())
        .to_vec()
}
