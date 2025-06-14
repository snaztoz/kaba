use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(src: &str) -> Result<JsValue, JsValue> {
    crate::compile(src)
        .map(|out| JsValue::from_str(&out))
        .map_err(|e| JsValue::from_str(&e.message))
}
