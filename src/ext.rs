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
            Ok(fail) => err(fail.input, NoMatch),
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
    fn up_to(mut self, limit: usize) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        move |input: I| {
            let mut input = input;
            let mut result = Vec::new();
            while result.len() < limit {
                match self.parse(input.clone()) {
                    Ok(success) => {
                        result.push(success.output);
                        input = success.input;
                    }
                    Err(err) if err.irrefutable => return Err(err),
                    Err(_) => break,
                }
            }
            ok(input, result)
        }
    }
    /// Matches exactly `count` instances of `self`.
    fn count(mut self, count: usize) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        move |input: I| {
            let mut input = input;
            let mut result = Vec::new();
            while result.len() < count {
                match self.parse(input.clone()) {
                    Ok(success) => {
                        result.push(success.output);
                        input = success.input;
                    }
                    Err(err) => {
                        return Err(err.message(format!(
                            "Expected to match {} times but got {}.",
                            count,
                            result.len()
                        )))
                    }
                }
            }
            ok(input, result)
        }
    }
    fn at_least(self, count: usize) -> impl Parser<I, Vec<O>, E>
    where
        I: Clone,
    {
        concat(self.clone().count(count), self.many())
    }
    /// Matches 0 or more of `self` until an error
    /// or EOF occurs. However, irrefutable errors
    /// are still propagated (as always).
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
                    Err(err) if err.irrefutable => return Err(err),
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
                Err(err) if err.irrefutable => return Err(err),
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
    /// Makes a parser _irrefutable_.
    ///
    /// An irrefutable parser is one whose errors cannot be
    /// caught. Combinators like `or` and `.many()`, which
    /// usually proceed in the case of errors, will still
    /// propagate the error.
    ///
    /// Why? Consider JSON. Imagine we've parsed an opening
    /// brace, `[`. Once we've done so, this **must** be an
    /// array. There is no other possibility and trying them
    /// makes no sense. We're "committed". So if parsing fails,
    /// it throws an irrefutable error so that parsing can
    /// simply halt.
    fn irrefutable(mut self) -> impl Parser<I, O, E> {
        move |input: I| match self.parse(input) {
            ok @ Ok(_) => ok,
            Err(mut err) => {
                err.irrefutable = true;
                Err(err)
            }
        }
    }
    /// Returns `self` when preceded by the given parser, whose
    /// value must be matched but which is then discarded.
    ///
    /// Imagine a grammar like `TAG 347`, in which the keyword `TAG`
    /// is followed by a number. The keyword must be present, but we
    /// are interested only in the number itself:
    ///
    /// ```rust,ignore
    /// one_of_str("0123456789", false)
    ///     .many1()
    ///     .to_string()
    ///     .followed_by(whitespace.many1())
    ///     .preceded_by(string("TAG"))
    /// ```
    ///
    /// When matching `TAG 347`, this gives us the string "347".
    fn preceded_by<P>(mut self, mut preceder: impl Parser<I, P, E>) -> impl Parser<I, O, E> {
        move |input: I| {
            let success = preceder.parse(input)?;
            self.parse(success.input)
        }
    }
    /// Returns `self` followed by the given parser, whose
    /// value must be matched but which is then discarded.
    ///
    /// Imagine a grammar like `TAG 347`, in which the keyword `TAG`
    /// is followed by a number. The keyword must be present, but we
    /// are interested only in the number itself. We must also handle
    /// the whitespace after the keyword `TAG`, but we are not
    /// interested in its value. `followed_by` works beautifully here:
    ///
    /// ```rust,ignore
    /// one_of_str("0123456789", false)
    ///     .many1()
    ///     .to_string()
    ///     .followed_by(whitespace.many1())
    ///     .preceded_by(string("TAG"))
    /// ```
    ///
    /// When matching `TAG 347`, this gives us the string "347".
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
    /// Returns `self` delimited by `preceder` and `follower`.
    ///
    /// Once `preceder` has been matched, the rest of the match
    /// becomes irrefutable. If you don't want this, it's easy
    /// enough to use `followed_by` and `preceded_by` yourself,
    /// but irrefutable is the most common case.
    fn delimited_by<P, F>(
        self,
        preceder: impl Parser<I, P, E>,
        follower: impl Parser<I, F, E>,
    ) -> impl Parser<I, O, E> {
        self.followed_by(follower)
            .irrefutable()
            .preceded_by(preceder)
    }
    /// Attaches an error message to any error which is
    /// thrown, optionally overwriting an existing message.
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
    /// This makes any error irrefutable after a certain number of `char`s.
    ///
    /// # Example
    /// ```rust,ignore
    /// string("null").irrefutable_after(1)
    /// ```
    ///
    /// This means that after `'n'` is matched, the matched string must be `null`
    /// or an irrefutable error occurs, which cannot be caught.
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
        self.surrounded_by(cond(required, whitespace.many1(), whitespace.many()))
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
    fn test_irrefutable_after() {
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
