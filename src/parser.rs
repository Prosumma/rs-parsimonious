use crate::result::*;
use crate::util::*;

pub trait Parser<I, O, E = ()>: Clone {
    fn parse(&mut self, input: I) -> ParseResult<I, O, E>;
}

impl<I, O, E, F: Clone> Parser<I, O, E> for F
where
    F: FnMut(I) -> ParseResult<I, O, E>,
{
    fn parse(&mut self, input: I) -> ParseResult<I, O, E> {
        self(input)
    }
}

impl<'a, E> Parser<&'a str, char, E> for char {
    fn parse(&mut self, input: &'a str) -> ParseResult<&'a str, char, E> {
        if let Some(c) = input.chars().next() {
            if *self == c {
                ok(next_char_slice(input), c)
            } else {
                err(input, NoMatch)
            }
        } else {
            err(input, EOF)
        }
    }
}

pub fn parse<I, O, E>(input: I, mut parser: impl Parser<I, O, E>) -> ParseResult<I, O, E> {
    parser.parse(input)
}
