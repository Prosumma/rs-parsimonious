use crate::combinators::*;
use crate::parser::*;
use crate::result::*;

pub trait ExtParser<I, O, E = ()>: Parser<I, O, E> {
    fn map<T>(mut self, f: impl FnMut(O) -> T + Clone) -> impl Parser<I, T, E> {
        move |input: I| Ok(self.parse(input)?.map(f.clone()))
    }
    fn to_vec(self) -> impl Parser<I, Vec<O>, E> {
        self.map(|o| vec![o])
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
}

impl<I, O, E, P> ExtParser<I, O, E> for P where P: Parser<I, O, E> {}

pub trait StrParser<'a, O, E = ()>: Parser<&'a str, O, E> {
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

impl<'a, O, E, P> StrParser<'a, O, E> for P where P: Parser<&'a str, O, E> {}

pub trait CharParser<I, E = ()>: Parser<I, Vec<char>, E> {
    fn to_string(self) -> impl Parser<I, String, E> {
        self.map(String::from_iter)
    }
}

impl<I, E, P> CharParser<I, E> for P where P: Parser<I, Vec<char>, E> {}

pub trait SliceParser<'a, I: 'a, O, E = ()>: Parser<&'a [I], O, E> {
    fn end(self) -> impl Parser<&'a [I], O, E> {
        self.followed_by(end)
    }
}

impl<'a, I: 'a, O, E, P> SliceParser<'a, I, O, E> for P where P: Parser<&'a [I], O, E> {}

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
}
