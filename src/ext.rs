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
}

impl<I, O, P> ExtParser<I, O> for P where P: Parser<I, O> {}

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
        concat($parser, $($rest),+)
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
        or($parser, $($rest),+)
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
