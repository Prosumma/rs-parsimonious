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

#[macro_export]
macro_rules! err {
    ($input:expr, $reason:expr) => {
        ::core::result::Result::Err($crate::parser::ParseError {
            input: $input,
            reason: $reason,
            irrefutable: false,
        })
    };
    ($input:expr, $reason:expr, irrefutable) => {
        ::core::result::Result::Err($crate::parser::ParseError {
            input: $input,
            reason: $reason,
            irrefutable: true,
        })
    };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_err() {
        let input = "Kimmy";
        let res: ParseResult<&str, char> = err!(input, NoMatch, irrefutable);
        let err = res.unwrap_err();
        assert!(err.irrefutable)
    }
}
