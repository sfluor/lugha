use eval::{exec, parse, EvalError};
use wasm_bindgen::prelude::*;

pub mod eval;
pub mod lexer;
pub mod parser;

impl Into<JsValue> for EvalError {
    fn into(self) -> JsValue {
        // TODO: improve me
        JsValue::from_str(format!("{self:?}").as_str())
    }
}

#[wasm_bindgen(catch)]
pub fn get_ast(code: String) -> Result<String, EvalError> {
    parse(&code)
        .map(|statements| statements
             .iter()
             .map(|statement| format!("{statement:?}\n").to_string())
             .collect()
             )
}

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
