use pest::Parser;
use pest::{error::Error, iterators::Pairs};
use pest_derive::Parser;

pub fn parse<'a>(src: &'a str) -> Result<Pairs<'a, Rule>, Error<Rule>> {
    KabaParser::parse(Rule::Program, src)
}

#[derive(Parser)]
#[grammar = "kaba.pest"]
struct KabaParser;

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_parsing_program() {
        let src = indoc! {"
            var x = 10;
            var y = 13;

            var temp = x;
            x = y;
            y = temp;

            print(x);
            print(y);
        "};

        let result = parse(src);

        assert!(result.is_ok());
    }
}
