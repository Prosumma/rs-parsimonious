use crate::ext::*;
use crate::parser::*;

pub fn test_char<'a>(
    input: &'a str,
    test: impl FnOnce(char) -> bool,
) -> ParseResult<&'a str, char> {
    let mut chars = input.chars();
    if let Some(ch) = chars.next() {
        if test(ch) {
            ok(chars.as_str(), ch)
        } else {
            err(input, NoMatch)
        }
    } else {
        err(input, EOF)
    }
}

pub fn satisfy_char<'a>(mut test: impl FnMut(char) -> bool) -> impl Parser<&'a str, char> {
    move |input: &'a str| test_char(input, &mut test)
}

pub fn eq_char<'a>(model: char) -> impl Parser<&'a str, char> {
    satisfy_char(move |ch: char| ch == model)
}

pub fn eq_ichar<'a>(model: char) -> impl Parser<&'a str, char> {
    satisfy_char(move |ch: char| ch.eq_ignore_ascii_case(&model))
}

fn test_str<'a, S: ToString>(
    model: S,
    mut test: impl FnMut(&'a str, char) -> ParseResult<&'a str, char>,
) -> impl Parser<&'a str, &'a str> {
    move |input: &'a str| {
        let model = model.to_string();
        let original_input = input;
        let mut input = input;
        for ch in model.chars() {
            match test(input, ch) {
                Ok(ok) => input = ok.input,
                Err(err) => return Err(err),
            }
        }
        ok(input, &original_input[0..model.len()])
    }
}

pub fn eq_str<'a, S: ToString>(model: S) -> impl Parser<&'a str, &'a str> {
    test_str(model, |input: &'a str, ch: char| eq_char(ch).parse(input))
}

pub fn eq_istr<'a, S: ToString>(model: S) -> impl Parser<&'a str, &'a str> {
    test_str(model, |input: &'a str, ch: char| eq_ichar(ch).parse(input))
}

pub fn whitespace<'a>(input: &'a str) -> ParseResult<&'a str, char> {
    test_char(input, char::is_whitespace)
}

pub trait StrInParser<'a, O>: Parser<&'a str, O> {
    fn whitespaced(mut self, required: bool) -> impl Parser<&'a str, O> {
        move |input: &'a str| {
            let parse_whitespace = || {
                let mut input = input;
                if required {
                    let output = whitespace.parse(input)?;
                    input = output.input;
                }
                whitespace.many().parse(input)
            };
            let output = parse_whitespace()?;
            let output = self.parse(output.input)?;
            let whitespace_output = parse_whitespace()?;
            ok(whitespace_output.input, output.output)
        }
    }
    fn braced(self) -> impl Parser<&'a str, O> {
        self.delimited('{', '}')
    }
    fn bracketed(self) -> impl Parser<&'a str, O> {
        self.delimited('[', ']')
    }
    fn parenthesized(self) -> impl Parser<&'a str, O> {
        self.delimited('(', ')')
    }
}

impl<'a> Parser<&'a str, char> for char {
    fn parse(&mut self, input: &'a str) -> ParseResult<&'a str, char> {
        test_char(input, move |ch: char| *self == ch)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eq_str() {
        let mut parser = eq_str("Roxy");
        let mut res = parser.parse("Roxy");
        assert!(res.is_ok());
        assert_eq!(res.unwrap().output, "Roxy");

        res = parser.parse("Foxy");
        assert!(res.is_err());
    }

    #[test]
    fn test_eq_istr() {
        let mut parser = eq_istr("Emily");
        let mut res = parser.parse("emilyx");
        assert!(res.is_ok());
        assert_eq!(res.unwrap().output, "emily");

        res = parser.parse("Jane");
        assert!(res.is_err());
    }
}
