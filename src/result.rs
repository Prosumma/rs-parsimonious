#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ParseOutput<I, O> {
    pub input: I,
    pub output: O,
}

impl<I, O> ParseOutput<I, O> {
    pub fn new(input: I, output: O) -> ParseOutput<I, O> {
        ParseOutput { input, output }
    }
    pub fn map<T>(self, f: impl FnOnce(O) -> T + Clone) -> ParseOutput<I, T> {
        ParseOutput {
            input: self.input,
            output: f(self.output),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ParseErrorReason {
    EOF,
    NoMatch,
}

pub use ParseErrorReason::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ParseError<I, E = ()> {
    pub input: I,
    pub reason: ParseErrorReason,
    pub message: Option<String>,
    pub meta: Option<E>,
    pub irrefutable: bool,
}

impl<I, E> ParseError<I, E> {
    pub fn new(input: I, reason: ParseErrorReason) -> ParseError<I, E> {
        ParseError {
            input,
            reason,
            message: None,
            meta: None,
            irrefutable: false,
        }
    }

    pub fn message<S: ToString>(self, message: S) -> ParseError<I, E> {
        let mut new = self;
        new.message = Some(message.to_string());
        new
    }

    pub fn meta(self, meta: E) -> ParseError<I, E> {
        let mut new = self;
        new.meta = Some(meta);
        new
    }
}

pub type ParseResult<I, O, E = ()> = Result<ParseOutput<I, O>, ParseError<I, E>>;

pub trait ExtParseResult<I, O, E = ()> {
    fn err_message<S: ToString>(self, message: S) -> Self;
    fn err_meta(self, meta: E) -> Self;
}

impl<I, O, E> ExtParseResult<I, O, E> for ParseResult<I, O, E> {
    fn err_message<S: ToString>(self, message: S) -> ParseResult<I, O, E> {
        self.map_err(|e| e.message(message))
    }
    fn err_meta(self, meta: E) -> ParseResult<I, O, E> {
        self.map_err(|e| e.meta(meta))
    }
}

pub fn ok<I, O, E>(input: I, output: O) -> ParseResult<I, O, E> {
    Ok(ParseOutput::new(input, output))
}

pub fn err<I, O, E>(input: I, reason: ParseErrorReason) -> ParseResult<I, O, E> {
    Err(ParseError::new(input, reason))
}
