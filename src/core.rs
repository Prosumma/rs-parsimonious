#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseError {
  /// A `PartialMatch` is always propagated.
  /// 
  /// If we start matching something and then 
  /// discover that it's wrong, that's always
  /// an error.
  /// 
  /// Consider JSON. If we match `{`, we know
  /// we're in an object. If parsing fails,
  /// there's no sense trying to match an array
  /// or any other JSON type. We throw `PartialMatch`
  /// and this bubbles up to the top.
  /// 
  /// `PartialMatch` is chiefly used by the `or`
  /// and `many` combinators. When either of these
  /// see `PartialMatch`, they raise it and bail.
  PartialMatch(usize),
  NoMatch(usize),
  End
}

pub use ParseError::*;

pub struct ParseContext<'a, I> {
  input: &'a [I],
  pub position: usize
}

impl<'a, I> ParseContext<'a, I> {
  pub const fn new(input: &[I]) -> ParseContext<I> {
    ParseContext { input, position: 0 }
  }

  pub fn current(&self) -> Option<&I> {
    self.input.get(self.position)
  }

  pub fn at_end(&self) -> bool {
    self.position >= self.input.len()
  }

  pub fn err_partial_match(&self) -> ParseError {
    PartialMatch(self.position)
  }

  pub fn err_no_match(&self) -> ParseError {
    NoMatch(self.position)
  }

  pub fn throw_partial_match<O>(&self) -> Result<O, ParseError> {
    Err(self.err_partial_match())
  }

  pub fn throw_no_match<O>(&self) -> Result<O, ParseError> {
    Err(self.err_no_match())
  }

  pub fn throw_end<O>(&self) -> Result<O, ParseError> {
    Err(End)
  }
}

pub trait Parser<I, O>: Clone {
  fn parse(&mut self, context: &mut ParseContext<I>) -> Result<O, ParseError>;

  fn map<M>(self, f: impl FnMut(O) -> M + Clone) -> impl Parser<I, M> {
    map(self, f)
  }

  fn partial(self, at: usize) -> impl Parser<I, O> {
    partial(self, at)
  }

  fn or(self, second: impl Parser<I, O>) -> impl Parser<I, O> {
    or(self, second)
  }

  fn many(self) -> impl Parser<I, Vec<O>> {
    many(self)
  }

  fn preceded_by<F>(self, first: impl Parser<I, F>) -> impl Parser<I, O> {
    second(first, self)
  }

  fn followed_by<S>(self, second: impl Parser<I, S>) -> impl Parser<I, O> {
    first(self, second)
  }
}

impl<I, O, F: Clone> Parser<I, O> for F
  where F: FnMut(&mut ParseContext<I>) -> Result<O, ParseError>
{
  fn parse(&mut self, context: &mut ParseContext<I>) -> Result<O, ParseError> {
    self(context)
  }
}

pub fn end<I>(context: &mut ParseContext<I>) -> Result<(), ParseError> {
  if let Some(_) = context.current() {
    context.throw_no_match()
  } else {
    Ok(())
  }
}

pub fn any<I: Clone>(context: &mut ParseContext<I>) -> Result<I, ParseError> {
  if let Some(i) = context.current() {
    let i = i.clone();
    context.position += 1;
    Ok(i)
  } else {
    context.throw_end()
  }
}

pub fn satisfy<I: Clone>(mut test: impl FnMut(&I) -> bool + Clone) -> impl Parser<I, I> {
  move |context: &mut ParseContext<I>| {
    if let Some(i) = context.current() {
      if test(i) {
        let i = i.clone();
        context.position += 1;
        Ok(i)
      } else {
        context.throw_no_match()
      }
    } else {
      context.throw_end()
    }
  }
}

pub fn map<I, O, M>(mut parser: impl Parser<I, O>, mut f: impl FnMut(O) -> M + Clone) -> impl Parser<I, M> {
  move |context: &mut ParseContext<I>| {
    parser.parse(context).map(&mut f)
  }
}

/// Converts a `NoMatch` to a `PartialMatch` if the difference between
/// the starting position and the error position is >= `at`. By far the
/// most common case is to say `partial(parser, 1)`.
/// 
/// This works best for things which are delimited, such as a string starting
/// with a double quote or a JSON object starting with a curly brace. Or anything
/// starting with a sigil of some kind.
/// 
/// It doesn't work so well for things like numbers, because a number matches
/// digits until there are no more digits, and then says "I'm done". It expects
/// the next parser (if any) to pick up after that.
/// 
/// However, it is possible to throw a partial match with a negative number, but
/// only if the minus sign is followed immediately by a non-digit.
pub fn partial<I, O>(mut parser: impl Parser<I, O>, at: usize) -> impl Parser<I, O> {
  move |context: &mut ParseContext<I>| {
    let initial_position = context.position;
    match parser.parse(context) {
      Err(NoMatch(err_position)) if err_position - initial_position >= at => Err(PartialMatch(err_position)),
      other => other
    }
  }
}

pub fn or<I, O>(mut first: impl Parser<I, O>, mut second: impl Parser<I, O>) -> impl Parser<I, O> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    match first.parse(context) {
      Err(NoMatch(_)) => { 
        context.position = position;
        second.parse(context)
      },
      Err(End) => {
        context.position = position;
        second.parse(context)
      },
      other => other
    }
  }
}

pub fn many<I, O>(mut parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    loop {
      let position = context.position;
      match parser.parse(context) {
        Ok(output) => outputs.push(output),
        Err(PartialMatch(err_position)) => return Err(PartialMatch(err_position)),
        _ => {
          context.position = position;
          break
        } 
      }
    }
    Ok(outputs)
  }
}

pub fn count<I, O>(count: usize, mut parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    while outputs.len() < count {
      outputs.push(parser.parse(context)?);
    }
    Ok(outputs)
  }
}

pub fn upto<I, O>(upto: usize, mut parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    while outputs.len() < upto {
      let position = context.position;
      if let Ok(output) = parser.parse(context) {
        outputs.push(output);
      } else {
        context.position = position;
        break
      }
    }
    Ok(outputs)
  }
}

pub fn chains<I, O>(mut first: impl Parser<I, Vec<O>>, mut second: impl Parser<I, Vec<O>>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut first_output = first.parse(context)?;
    let second_output = second.parse(context)?;
    first_output.extend(second_output);
    Ok(first_output)
  }
}

pub fn first<I, F, S>(mut first: impl Parser<I, F>, mut second: impl Parser<I, S>) -> impl Parser<I, F> {
  move |context: &mut ParseContext<I>| {
    let first_output = first.parse(context)?;
    second.parse(context)?;
    Ok(first_output)
  }
}

pub fn second<I, F, S>(mut first: impl Parser<I, F>, mut second: impl Parser<I, S>) -> impl Parser<I, S> {
  move |context: &mut ParseContext<I>| {
    first.parse(context)?;
    second.parse(context)
  }
}

pub fn just<I, O>(mut make: impl FnMut() -> O + Clone) -> impl Parser<I, O> {
  move |_: &mut ParseContext<I>| {
    Ok(make()) 
  }
}

pub fn peek<I, O>(mut parser: impl Parser<I, O>) -> impl Parser<I, ()> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    parser.parse(context).map(|_| {
      context.position = position;
    })
  }
}

pub fn not<I, O>(mut parser: impl Parser<I, O>) -> impl Parser<I, ()> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    if parser.parse(context).is_err() {
      context.position = position;
      Ok(())
    } else {
      context.throw_no_match()
    }
  }
}