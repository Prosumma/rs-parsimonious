#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParseOutput<I, O> {
    pub input: I,
    pub output: O,
}

pub mod error {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Reason {
        NoMatch,
        EOF,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Error<I> {
        pub input: I,
        pub reason: Reason,
        pub irrefutable: bool,
    }
}

pub use error::Reason::*;

pub type ParseError<I> = error::Error<I>;
pub type ParseResult<I, O> = Result<ParseOutput<I, O>, ParseError<I>>;

pub fn ok<I, O>(input: I, output: O) -> ParseResult<I, O> {
    Ok(ParseOutput { input, output })
}

pub fn err<I, O>(input: I, reason: error::Reason) -> ParseResult<I, O> {
    Err(ParseError {
        input,
        reason,
        irrefutable: false,
    })
}

pub trait Parser<I, O>: Sized {
    fn parse(&mut self, input: I) -> ParseResult<I, O>;
}

impl<I, O, F> Parser<I, O> for F
where
    F: FnMut(I) -> ParseResult<I, O>,
{
    fn parse(&mut self, input: I) -> ParseResult<I, O> {
        self(input)
    }
}
