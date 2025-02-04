#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParseError {
  NoMatch(usize),
  EndOfInput
}

pub struct ParseContext<'a, I> {
  input: &'a [I],
  pub position: usize
}

impl<'a, I> ParseContext<'a, I> {
  pub fn new(input: &[I]) -> ParseContext<I> {
    ParseContext{input, position: 0}
  }

  pub fn current(&self) -> Option<&I> {
    self.input.get(self.position)
  }

  pub fn no_match<O>(&self) -> Result<O, ParseError> {
    Err(ParseError::NoMatch(self.position))
  }

  pub fn at_end(&self) -> bool {
    self.position >= self.input.len()
  }
}

pub trait Parser<I, O>: Clone {
  fn parse(&self, context: &mut ParseContext<I>) -> Result<O, ParseError>;
}

impl<I, O, F: Clone> Parser<I, O> for F where F: Fn(&mut ParseContext<I>) -> Result<O, ParseError> {
  fn parse(&self, context: &mut ParseContext<I>) -> Result<O, ParseError> {
      self(context)
  }
}

/// The fundamental combinator. Applies `test` to an individual element, returning
/// that element on success and `ParseError::NoMatch` on failure.
pub fn satisfy<I: Clone>(test: impl Fn(&I) -> bool + Clone) -> impl Parser<I, I> {
  move |context: &mut ParseContext<I>| {
    if let Some(i) = context.current() {
      if test(i) {
        let i = i.clone();
        context.position += 1;
        Ok(i)
      } else {
        context.no_match()
      }
    } else {
      Err(ParseError::EndOfInput)
    }
  }
}

pub fn map<I, O, M>(parser: impl Parser<I, O>, f: impl Fn(O) -> M + Clone) -> impl Parser<I, M> {
  move |context: &mut ParseContext<I>| {
    parser.parse(context).map(&f)
  }
}

pub fn or<I, O>(first: impl Parser<I, O>, second: impl Parser<I, O>) -> impl Parser<I, O> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    first.parse(context).or_else(|_| {
      context.position = position;
      second.parse(context)
    })
  }
}

pub fn chains<I, O>(first: impl Parser<I, Vec<O>>, second: impl Parser<I, Vec<O>>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    first.parse(context).and_then(|first_output| {
      second.parse(context).map(|second_output| {
        let mut output = first_output;
        output.extend(second_output);
        output
      })
    })
  }
}

pub fn many<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    while let Ok(output) = parser.parse(context) {
      outputs.push(output);
    }
    Ok(outputs)
  }
}

pub fn first<I, F, S>(first: impl Parser<I, F>, second: impl Parser<I, S>) -> impl Parser<I, F> {
  move |context: &mut ParseContext<I>| {
    first.parse(context).and_then(|output| {
      second.parse(context).map(|_| output)
    })
  }
}

#[macro_export]
macro_rules! first {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    first($parser, first!($($rest),+))
  }
}

pub fn second<I, F, S>(first: impl Parser<I, F>, second: impl Parser<I, S>) -> impl Parser<I, S> {
  move |context: &mut ParseContext<I>| {
    first.parse(context).and_then(|_| second.parse(context))
  }
}

#[macro_export]
macro_rules! last {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    second($parser, last!($($rest),+))
  }
}

pub fn peek<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, ()> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    parser.parse(context).map(|_| {
      context.position = position;
      ()
    })
  }
}

pub fn not<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, ()> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    match parser.parse(context) {
      Ok(_) => {
        context.position = position;
        context.no_match()
      },
      Err(_) => {
        context.position = position;
        Ok(())
      }
    }
  }
}

pub fn end<I>(context: &mut ParseContext<I>) -> Result<(), ParseError> {
  if context.at_end() {
    Ok(())
  } else {
    context.no_match()
  }
}

pub fn any<I: Clone>(context: &mut ParseContext<I>) -> Result<I, ParseError> {
  if let Some(i) = context.current() {
    Ok(i.clone())
  } else {
    Err(ParseError::EndOfInput)
  }
}

pub fn count<I, O>(count: usize, parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    while outputs.len() < count {
      let output = parser.parse(context)?;
      outputs.push(output);
    }
    Ok(outputs)
  }
}

pub fn upto<I, O>(upto: usize, parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    while outputs.len() < upto {
      let position = context.position;
      if let Ok(output) = parser.parse(context) {
        outputs.push(output)
      } else {
        context.position = position;
        break
      }
    }
    Ok(outputs)
  }
}

pub fn just<I, O>(make: impl Fn() -> O + Clone) -> impl Parser<I, O> {
  move |_: &mut ParseContext<I>| {
    return Ok(make())
  }
}
