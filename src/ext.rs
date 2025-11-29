use crate::combinators::*;
use crate::parser::*;
use crate::result::*;
use crate::util::*;

pub trait ExtParser<I, O, E = ()>: Parser<I, O, E> {
    fn map<T>(mut self, f: impl FnMut(O) -> T + Clone) -> impl Parser<I, T, E> {
        move |input: I| Ok(self.parse(input)?.map(f.clone()))
    }
    fn map_err(
        self,
        f: impl FnMut(ParseError<I, E>) -> ParseError<I, E> + Clone,
    ) -> impl Parser<I, O, E>
    where
        I: Clone,
    {
        move |input: I| self.clone().parse(input.clone()).map_err(f.clone())
    }
    fn void(self) -> impl Parser<I, (), E> {
        self.map(|_| ())
    }
    fn peek(mut self) -> impl Parser<I, O, E>
    where
        I: Clone,
    {
        move |input: I| {
            let result = self.parse(input.clone())?;
            ok(input, result.output)
        }
    }
    fn not(mut self) -> impl Parser<I, (), E>
    where
        I: Clone,
    {
        move |input: I| match self.parse(input.clone()) {
            Ok(_) => err(input, NoMatch),
            Err(_) => ok(input, ()),
        }
    }
    fn to_vec(self) -> impl Parser<I, Vec<O>, E> {
        self.map(|o| vec![o])
    }
    fn maybe(self) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        or(self.to_vec(), just_lazy(Vec::new))
    }
    fn many(mut self) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        move |input: I| {
            let mut input = input;
            let mut result = Vec::new();
            loop {
                match self.parse(input.clone()) {
                    Ok(success) => {
                        result.push(success.output);
                        input = success.input;
                    }
                    Err(_) => break,
                }
            }
            ok(input, result)
        }
    }
    fn many_sep_by<S>(self, sep: impl Parser<I, S, E>) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
        O: Clone,
    {
        move |input: I| {
            let mut result = Vec::new();
            match self.clone().parse(input.clone()) {
                /*
                 * Why do we do this? Because this is `many`,
                 * so it cannot fail. We match 0 or more.
                 */
                Err(_) => ok(input, result),
                Ok(success) => {
                    result.push(success.output);
                    let mut remainder = parse(
                        success.input,
                        (self.clone().preceded_by(sep.clone())).many(),
                    )?;
                    result.append(&mut remainder.output);
                    ok(remainder.input, result)
                }
            }
        }
    }
    fn many1(self) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        concat(self.clone().to_vec(), self.many())
    }
    fn many1_sep_by<S>(self, sep: impl Parser<I, S, E>) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        concat(self.clone().to_vec(), (self.preceded_by(sep)).many())
    }
    fn irrefutable(mut self) -> impl Parser<I, O, E> {
        move |input: I| match self.parse(input) {
            ok @ Ok(_) => ok,
            Err(mut err) => {
                err.irrefutable = true;
                Err(err)
            }
        }
    }
    fn preceded_by<P>(mut self, mut preceder: impl Parser<I, P, E>) -> impl Parser<I, O, E> {
        move |input: I| {
            let success = preceder.parse(input)?;
            self.parse(success.input)
        }
    }
    fn followed_by<F>(mut self, mut follower: impl Parser<I, F, E>) -> impl Parser<I, O, E> {
        move |input: I| {
            let success = self.parse(input)?;
            let follower_success = follower.parse(success.input)?;
            ok(follower_success.input, success.output)
        }
    }
    fn surrounded_by<S>(self, surrounder: impl Parser<I, S, E>) -> impl Parser<I, O, E> {
        self.preceded_by(surrounder.clone()).followed_by(surrounder)
    }
    fn delimited_by<P, F>(
        self,
        preceder: impl Parser<I, P, E>,
        follower: impl Parser<I, F, E>,
    ) -> impl Parser<I, O, E> {
        self.followed_by(follower)
            .irrefutable()
            .preceded_by(preceder)
    }
    fn err_message<S: ToString + Clone>(
        mut self,
        message: S,
        overwrite: bool,
    ) -> impl Parser<I, O, E>
    where
        I: Clone,
    {
        move |input: I| match self.parse(input.clone()) {
            ok @ Ok(_) => ok,
            Err(mut err) => {
                if overwrite || err.message.is_none() {
                    err.message = Some(message.to_string())
                }
                Err(err)
            }
        }
    }
}

impl<I, O, E, P> ExtParser<I, O, E> for P where P: Parser<I, O, E> {}

pub trait StrInParser<'a, O, E = ()>: Parser<&'a str, O, E> {
    fn irrefutable_after(mut self, after: usize) -> impl Parser<&'a str, O, E> {
        move |input: &'a str| match self.parse(input) {
            ok @ Ok(_) => ok,
            Err(mut err) => {
                if !err.irrefutable && err.input.char_offset_from(input) > after {
                    err.irrefutable = true;
                }
                Err(err)
            }
        }
    }
    fn whitespaced(self, required: bool) -> impl Parser<&'a str, O, E> {
        cond(
            required,
            self.clone().surrounded_by(whitespace.many1()),
            self.surrounded_by(whitespace.many()),
        )
    }
    fn bracketed(self) -> impl Parser<&'a str, O, E> {
        self.delimited_by('[', ']')
    }
    fn braced(self) -> impl Parser<&'a str, O, E> {
        self.delimited_by('{', '}')
    }
    fn parethesized(self) -> impl Parser<&'a str, O, E> {
        self.delimited_by('(', ')')
    }
    fn double_quoted(self) -> impl Parser<&'a str, O, E> {
        self.irrefutable().surrounded_by('"')
    }
    fn single_quoted(self) -> impl Parser<&'a str, O, E> {
        self.irrefutable().surrounded_by('\'')
    }
    fn end(self) -> impl Parser<&'a str, O, E> {
        self.followed_by(end_str)
    }
}

impl<'a, O, E, P> StrInParser<'a, O, E> for P where P: Parser<&'a str, O, E> {}

pub trait CharOutParser<I, E = ()>: Parser<I, Vec<char>, E> {
    fn to_string(self) -> impl Parser<I, String, E> {
        self.map(String::from_iter)
    }
}

impl<I, E, P> CharOutParser<I, E> for P where P: Parser<I, Vec<char>, E> {}

pub trait StrOutParser<'a, I, E = ()>: Parser<I, &'a str, E> {
    fn to_string(self) -> impl Parser<I, String, E> {
        self.map(|s| s.to_string())
    }
}

impl<'a, I, E, P> StrOutParser<'a, I, E> for P where P: Parser<I, &'a str, E> {}

pub trait SliceInParser<'a, I: 'a, O, E = ()>: Parser<&'a [I], O, E> {
    fn end(self) -> impl Parser<&'a [I], O, E> {
        self.followed_by(end)
    }
}

impl<'a, I: 'a, O, E, P> SliceInParser<'a, I, O, E> for P where P: Parser<&'a [I], O, E> {}

pub trait VecOutParser<I, O, E = ()>: Parser<I, Vec<O>, E> {
    fn vec_maybe(self) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        or(self, just_lazy(Vec::new))
    }
}

impl<I, O, E, P> VecOutParser<I, O, E> for P where P: Parser<I, Vec<O>, E> {}

#[macro_export]
macro_rules! void {
    ($parser:expr) => {
        $parser.void()
    };
    ($parser:expr, $($rest:expr),+) => {
        $crate::combinators::or($parser.void(), void!($($rest),+))
    };
}

#[macro_export]
macro_rules! peek {
    ($parser:expr, $($rest:expr),*) => {
        $crate::void!($parser, $($rest),*).peek()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_end_str() {
        let input = "aaa";
        let parser = 'a'.many1().end();
        let result: ParseResult<&str, Vec<char>> = parse(input, parser);
        assert!(result.is_ok())
    }

    #[test]
    fn test_braced() {
        let input = "{ aaa}";
        let parser = 'a'.many1().whitespaced(false).braced().end();
        let result: ParseResult<&str, Vec<char>> = parse(input, parser);
        assert!(result.is_ok())
    }

    #[test]
    fn test_irrefutable_at() {
        let input1 = "nolk";
        let input2 = "zulk";
        let parser = string("null", false).irrefutable_after(1);

        let mut result: ParseResult<&str, &str> = parse(input1, parser.clone());
        let mut err = result.unwrap_err();
        assert!(err.irrefutable);

        result = parse(input2, parser);
        err = result.unwrap_err();
        assert!(!err.irrefutable);
    }
}
