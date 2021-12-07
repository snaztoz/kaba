use regex::{Captures, Regex};
use std::fs;

pub fn read_input_file(path: &str) -> String {
    let path = format!("{}/src/testdata/{}", env!("CARGO_MANIFEST_DIR"), path);
    let file_content = fs::read_to_string(&path).unwrap();

    // convert newline characters to '\n' in all platforms
    let re = Regex::new("(\r\n|\r)").unwrap();
    let cleaned_content = re.replace_all(&file_content, |_: &Captures| String::from("\n"));

    String::from(cleaned_content)
}
