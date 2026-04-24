use crate::err;
use crate::parser::*;

pub trait ExtParser<I, O>: Parser<I, O> {
    fn map<N>(mut self, mut f: impl FnMut(O) -> N) -> impl Parser<I, N> {
        move |input: I| {
            let output = self.parse(input)?;
            ok(output.input, f(output.output))
        }
    }
    fn to_vec(self) -> impl Parser<I, Vec<O>> {
        self.map(|o| vec![o])
    }
    fn many(mut self) -> impl Parser<I, Vec<O>>
    where
        I: Clone,
    {
        move |input: I| {
            let mut outputs = Vec::new();
            let mut input = input;
            loop {
                match self.parse(input.clone()) {
                    Ok(ParseOutput {
                        input: next_input,
                        output,
                    }) => {
                        input = next_input;
                        outputs.push(output);
                    }
                    Err(err) if err.irrefutable => return Err(err),
                    _ => break,
                }
            }
            ok(input, outputs)
        }
    }
    fn preceded_by<P>(mut self, mut preceder: impl Parser<I, P>) -> impl Parser<I, O> {
        move |input: I| {
            let preceding_output = preceder.parse(input)?;
            self.parse(preceding_output.input)
        }
    }
    fn followed_by<F>(mut self, mut follower: impl Parser<I, F>) -> impl Parser<I, O> {
        move |input: I| {
            let output = self.parse(input)?;
            let follower_output = follower.parse(output.input)?;
            ok(follower_output.input, output.output)
        }
    }
    fn delimited<P, F>(
        self,
        preceder: impl Parser<I, P>,
        follower: impl Parser<I, F>,
    ) -> impl Parser<I, O> {
        self.preceded_by(preceder).followed_by(follower)
    }
    fn end(self) -> impl Parser<I, O>
    where
        I: Finite,
    {
        self.followed_by(end)
    }
}

impl<I, O, P> ExtParser<I, O> for P where P: Parser<I, O> {}

pub fn just<I, O>(mut make: impl FnMut() -> O) -> impl Parser<I, O> {
    move |input: I| ok(input, make())
}

pub fn concat<I, O>(
    mut first: impl Parser<I, O>,
    mut second: impl Parser<I, O>,
) -> impl Parser<I, Vec<O>> {
    move |input: I| {
        let first_output = first.parse(input)?;
        let second_output = second.parse(first_output.input)?;
        ok(
            second_output.input,
            vec![first_output.output, second_output.output],
        )
    }
}

#[macro_export]
macro_rules! concat {
    ($parser:expr) => { $parser };
    ($parser:expr, $($rest:expr),+ $(,)?) => {
        $crate::ext::concat($parser, concat!($($rest),+))
    }
}

pub fn or<I, O>(mut lhs: impl Parser<I, O>, mut rhs: impl Parser<I, O>) -> impl Parser<I, O> {
    move |input: I| match lhs.parse(input) {
        ok @ Ok(_) => ok,
        Err(err) if err.irrefutable => Err(err),
        Err(err) => rhs.parse(err.input),
    }
}

#[macro_export]
macro_rules! or {
    ($parser:expr) => { $parser };
    ($parser:expr, $($rest:expr),+ $(,)?) => {
        $crate::ext::or($parser, or!($($rest),+))
    }
}

pub fn cond<I, O>(
    condition: bool,
    mut true_parser: impl Parser<I, O>,
    mut false_parser: impl Parser<I, O>,
) -> impl Parser<I, O> {
    move |input: I| {
        if condition {
            true_parser.parse(input)
        } else {
            false_parser.parse(input)
        }
    }
}

pub trait Finite {
    fn at_end(&self) -> bool;
}

impl Finite for &str {
    fn at_end(&self) -> bool {
        self.len() == 0
    }
}

impl<T> Finite for &[T] {
    fn at_end(&self) -> bool {
        self.len() == 0
    }
}

pub fn end<I: Finite>(input: I) -> ParseResult<I, ()> {
    if input.at_end() {
        ok(input, ())
    } else {
        err!(input, NoMatch)
    }
}
