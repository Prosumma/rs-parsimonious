use crate::concat;
use crate::ext::*;
use crate::flatten::*;
use crate::join;
use crate::parser::*;
use crate::string::*;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum JSON {
    Number(String),
    Null,
}

fn digit<'a>(input: &'a str) -> ParseResult<&'a str, char> {
    test_char(input, |ch: char| "0123456789".contains(ch))
}

fn positive_digit<'a>(input: &'a str) -> ParseResult<&'a str, char> {
    test_char(input, |ch: char| "123456789".contains(ch))
}

fn integer<'a>(input: &'a str) -> ParseResult<&'a str, Vec<char>> {
    or(digit.to_vec(), cons(positive_digit, digit.many())).parse(input)
}

fn number<'a>(input: &'a str) -> ParseResult<&'a str, Vec<char>> {
    let fractional = cons('.', integer);
    join!(integer, or(fractional, just(Vec::new))).parse(input)
}

fn jnumber<'a>(input: &'a str) -> ParseResult<&'a str, JSON> {
    number.to_string().map(JSON::Number).parse(input)
}
