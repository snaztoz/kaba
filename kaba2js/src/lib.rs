pub fn compile(src: &str) -> String {
    String::from(src)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = compile("Hello JS!");
        assert_eq!(result, String::from("Hello JS!"));
    }
}
