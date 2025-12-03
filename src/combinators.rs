use crate::parser::*;
use crate::result::*;
use crate::util::*;
use std::collections::HashSet;

pub fn just<I, O: Clone, E>(value: O) -> impl Parser<I, O, E> {
    move |input: I| ok(input, value.clone())
}

pub fn just_lazy<I, O, E>(mut make_value: impl FnMut() -> O + Clone) -> impl Parser<I, O, E> {
    move |input: I| ok(input, make_value())
}

pub fn item<'a, I: Clone + 'a, E>(input: &'a [I]) -> ParseResult<&'a [I], I, E> {
    if let Some(i) = input.get(0) {
        ok(&input[1..], i.clone())
    } else {
        err(input, EOF)
    }
}

pub fn item_str<'a, E>(input: &'a str) -> ParseResult<&'a str, char, E> {
    let mut chars = input.chars();
    if let Some(c) = chars.next() {
        ok(chars.as_str(), c)
    } else {
        err(input, EOF)
    }
}

/// This handles a peculiarity of Rust's
/// type system. Imagine we are creating
/// a combinator to handle whitespace, which
/// may be required or optional. A naive
/// implementation looks like this:
///
/// ```rust,compile_fail
/// fn whitespaced(self, required: bool) -> impl Parser<&'a str, O, E> {
///     if required {
///         self.surrounded_by(whitespace.many1())
///     } else {
///         self.surrounded_by(whitespace.many())
///     }
/// }
/// ```
///
/// So, if whitespace must be present, we return one parser
/// and if it must not, we return another.
///
/// But this won't compile. The reason is that the return type
/// of these combinators is `impl Parser<&'a str, O, E>`. Due
/// to monomorphization, each of these is actually a _different
/// type_, so compilation fails.
///
/// `cond` solves this problem by creating a new parser which
/// returns a single type, and running the other parsers within
/// based on the condition.
///
/// ```rust,ignore
/// fn whitespaced(self, required: bool) -> impl Parser<&'a str, O, E> {
///     self.surrounded_by(cond(required, whitespace.many1(), whitespace.many()))
/// }
/// ```
pub fn cond<I, O, E>(
    cond: bool,
    mut first: impl Parser<I, O, E>,
    mut second: impl Parser<I, O, E>,
) -> impl Parser<I, O, E> {
    move |input: I| {
        if cond {
            first.parse(input)
        } else {
            second.parse(input)
        }
    }
}

/// One of the most common combinators.
///
/// Attempts to match `first` and if that fails
/// it attempts to match `second`. If that fails
/// it returns an error.
///
/// If `first` throws an irrefutable error, then
/// `or` returns this immediately. Irrefutable errors
/// cannot be caught.
///
/// If you need to alternate on more than one
/// parser, use the `or!` macro.
pub fn or<I, O, E>(
    mut first: impl Parser<I, O, E>,
    mut second: impl Parser<I, O, E>,
) -> impl Parser<I, O, E>
where
    I: Clone,
{
    move |input: I| match first.parse(input.clone()) {
        ok @ Ok(_) => ok,
        Err(err) if err.irrefutable => Err(err),
        _ => second.parse(input),
    }
}

#[macro_export]
macro_rules! or {
    ($parser:expr $(,)?) => { $parser };
    ($parser:expr, $($rest:expr),+ $(,)?) => {
        or($parser, or!($($rest),+))
    };
}

pub fn tuple<I, F, S, E>(
    mut first: impl Parser<I, F, E>,
    mut second: impl Parser<I, S, E>,
) -> impl Parser<I, (F, S), E> {
    move |input: I| {
        let first_success = first.parse(input)?;
        let second_success = second.parse(first_success.input)?;
        ok(
            second_success.input,
            (first_success.output, second_success.output),
        )
    }
}

pub fn triple<I, F, M, L, E>(
    mut first: impl Parser<I, F, E>,
    mut middle: impl Parser<I, M, E>,
    mut last: impl Parser<I, L, E>,
) -> impl Parser<I, (F, M, L), E> {
    move |input: I| {
        let first_success = first.parse(input)?;
        let middle_success = middle.parse(first_success.input)?;
        let last_success = last.parse(middle_success.input)?;
        ok(
            last_success.input,
            (
                first_success.output,
                middle_success.output,
                last_success.output,
            ),
        )
    }
}

pub fn satisfy<'a, T: Clone + 'a, E>(
    mut test: impl FnMut(&T) -> bool + Clone,
) -> impl Parser<&'a [T], T, E> {
    move |input: &'a [T]| {
        if let Some(t) = input.get(0) {
            if test(t) {
                ok(&input[1..], t.clone())
            } else {
                err(input, NoMatch)
            }
        } else {
            err(input, EOF)
        }
    }
}

pub fn eq<'a, T: PartialEq + Clone + 'a, E>(model: T) -> impl Parser<&'a [T], T, E> {
    satisfy(move |elem| *elem == model)
}

pub trait CharTest {
    fn test(&mut self, c: char, case_insensitive: bool) -> bool;
}

impl CharTest for char {
    fn test(&mut self, c: char, case_insensitive: bool) -> bool {
        if case_insensitive {
            self.to_ascii_lowercase() == c.to_ascii_lowercase()
        } else {
            *self == c
        }
    }
}

impl<F> CharTest for F
where
    F: FnMut(char, bool) -> bool,
{
    fn test(&mut self, c: char, case_insensitive: bool) -> bool {
        self(c, case_insensitive)
    }
}

pub fn char<'a, E>(
    mut test: impl CharTest + Clone,
    case_insensitive: bool,
) -> impl Parser<&'a str, char, E> {
    move |input: &'a str| {
        if let Some(c) = input.chars().next() {
            if test.test(c, case_insensitive) {
                ok(next_char_slice(input), c)
            } else {
                err(input, NoMatch)
            }
        } else {
            err(input, EOF)
        }
    }
}

pub fn ch<'a, E>(mut test: impl FnMut(char) -> bool + Clone) -> impl Parser<&'a str, char, E> {
    char(move |c: char, _: bool| test(c), false)
}

/// Matches a string, optionally with case insensitivity.
pub fn string<'a, E, S: AsRef<str> + Clone>(
    s: S,
    case_insensitive: bool,
) -> impl Parser<&'a str, &'a str, E> {
    move |input: &'a str| {
        let s = s.as_ref();
        let mut s_chars = s.chars();
        let mut input_chars = input.chars();
        loop {
            match (s_chars.next(), input_chars.next()) {
                (None, _) => return ok(&input[s.len()..], &input[..s.len()]),
                (_, None) => return err(input_chars.as_str(), EOF),
                (Some(s_c), Some(input_c)) => {
                    if case_insensitive {
                        if s_c.to_ascii_lowercase() != input_c.to_ascii_lowercase() {
                            return err(input_chars.as_str(), NoMatch);
                        }
                    } else if s_c != input_c {
                        return err(input_chars.as_str(), NoMatch);
                    }
                }
            }
        }
    }
}

pub fn one_of_str<'a, E, S: AsRef<str> + Clone>(
    s: S,
    case_insensitive: bool,
) -> impl Parser<&'a str, char, E> {
    let mut chars: HashSet<char> = s.as_ref().chars().collect();
    if case_insensitive {
        chars = chars.into_iter().map(|c| c.to_ascii_lowercase()).collect();
    }
    move |input: &'a str| {
        if let Some(c) = input.chars().next() {
            let mut cc = c;
            if case_insensitive {
                cc = c.to_ascii_lowercase();
            }
            if chars.contains(&cc) {
                ok(next_char_slice(input), c)
            } else {
                err(input, NoMatch)
            }
        } else {
            err(input, EOF)
        }
    }
}

/// Matches a single character of whitespace.
pub fn whitespace<'a, E>(input: &'a str) -> ParseResult<&'a str, char, E> {
    ch(char::is_whitespace).parse(input)
}

/// Matches EOF when the input type is a slice of some arbitrary type.
pub fn end<'a, Elem, E>(input: &'a [Elem]) -> ParseResult<&'a [Elem], (), E> {
    if input.len() == 0 {
        ok(input, ())
    } else {
        err(input, NoMatch)
    }
}

/// Matches EOF when the input type is a string slice, the most common case.
pub fn end_str<'a, E>(input: &'a str) -> ParseResult<&'a str, (), E> {
    if input.len() == 0 {
        ok(input, ())
    } else {
        err(input, NoMatch)
    }
}

pub fn concat<I, O, E>(
    mut first: impl Parser<I, Vec<O>, E>,
    mut second: impl Parser<I, Vec<O>, E>,
) -> impl Parser<I, Vec<O>, E> {
    move |input: I| {
        let mut result = Vec::new();
        let mut success = first.parse(input)?;
        result.append(&mut success.output);
        let mut success = second.parse(success.input)?;
        result.append(&mut success.output);
        ok(success.input, result)
    }
}

#[macro_export]
macro_rules! concat {
    ($parser:expr $(,)?) => {
        $parser
    };
    ($parser:expr, $($rest:expr),+ $(,)?) => {
        concat($parser, concat!($($rest),+))
    }
}

/// A parser which always fails, propagating the
/// provided error.
///
/// This is best used with alternation and with the
/// more convenient `fail!` macro.
///
/// ```rust,ignore
/// or!(parser1, parser2, fail!(NoMatch, message = "It failed!"))
/// ```
///
/// However, in most cases it's easier to just use the `err_message`
/// combinator:
///
/// ```rust,ignore
/// or!(parser1, parser2).err_message("It failed!")
/// ```
///
/// This is not exactly equivalent. The former creates a new error
/// while the latter adds a message to the current error, but more
/// often than not the latter is what you want.
pub fn fail<I, O, E: Clone, S: ToString + Clone>(
    reason: ParseErrorReason,
    message: Option<S>,
    meta: Option<E>,
) -> impl Parser<I, O, E> {
    move |input: I| {
        let mut err = ParseError::new(input, reason);
        err.message = message.clone().map(|s| s.to_string());
        err.meta = meta.clone();
        Err(err)
    }
}

#[macro_export]
macro_rules! fail {
    ($reason:expr $(,)?) => {
        $crate::combinators::fail($reason, None, None)
    };
    ($reason:expr, message = $message:expr, meta = $meta:expr $(,)?) => {
        $crate::combinators::fail($reason, Some($message), Some($meta))
    };
    ($reason:expr, message = $message:expr $(,)?) => {
        $crate::combinators::fail($reason, Some($message), None)
    };
    ($reason:expr, meta = $meta:expr $(,)?) => {
        $crate::combinators::fail($reason, None, Some($meta))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ext::*;

    #[test]
    fn satisfies() {
        let input = vec![3u32];
        let mut p = satisfy(|i: &u32| *i == 3);
        let result: ParseResult<&[u32], u32> = p.parse(&input);
        let success = result.unwrap();
        assert_eq!(success.output, 3);
    }

    #[test]
    fn eqs() {
        let input = vec![7u8, 2];
        let mut p = eq(7);
        let result: ParseResult<&[u8], u8> = p.parse(&input);
        let success = result.unwrap();
        assert_eq!(success.output, 7);
    }

    #[test]
    fn test_one_of_str() {
        let input = "aeio";
        let parser = one_of_str("Aeiou", true);
        let result: ParseResult<&str, char> = parse(input, parser);
        let success = result.unwrap();
        assert_eq!(success.output, 'a');
    }

    #[test]
    fn test_or() {
        let input = "{b}";
        let choices = or('a', 'b');
        let parser = or(choices.clone().bracketed(), choices.braced());
        let result: ParseResult<&str, char> = parse(input, parser);
        let success = result.unwrap();
        assert_eq!(success.output, 'b');
    }

    #[test]
    fn test_or_irrefutable() {
        let input = "[a,]";
        let choices = or('a', 'b');
        let parser = or(
            choices.clone().bracketed(),
            fail!(NoMatch, message = "We should never get here."),
        );
        let result: ParseResult<&str, char> = parse(input, parser);
        let err = result.unwrap_err();
        assert!(err.irrefutable)
    }

    #[test]
    fn test_string() {
        let input = "Mises";
        let parser = string("Mises", false);
        let result: ParseResult<&str, &str> = parse(input, parser);
        let success = result.unwrap();
        assert_eq!(success.output, "Mises");
        assert_eq!(success.input, "");
    }
}
