#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseError<E = ()> {
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
  End(usize),
  /// An unrecoverable custom error
  Error(E)
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

  pub fn err_end<E>(&self) -> ParseError<E> {
    End(self.position)
  }

  pub fn err_partial_match<E>(&self) -> ParseError<E> {
    PartialMatch(self.position)
  }

  pub fn err_no_match<E>(&self) -> ParseError<E> {
    NoMatch(self.position)
  }

  pub fn throw_end<O, E>(&self) -> Result<O, ParseError<E>> {
    Err(self.err_end())
  }

  pub fn throw_partial_match<O, E>(&self) -> Result<O, ParseError<E>> {
    Err(self.err_partial_match())
  }

  pub fn throw_no_match<O, E>(&self) -> Result<O, ParseError<E>> {
    Err(self.err_no_match())
  }
}

pub trait Parser<I, O, E>: Clone {
  fn parse(&mut self, context: &mut ParseContext<I>) -> Result<O, ParseError<E>>;

  fn map<M>(self, f: impl FnMut(O) -> M + Clone) -> impl Parser<I, M, E> {
    map(self, f)
  }

  fn partial(self, at: usize) -> impl Parser<I, O, E> {
    partial(self, at)
  }

  fn or(self, second: impl Parser<I, O, E>) -> impl Parser<I, O, E> {
    or(self, second)
  }

  fn many(self) -> impl Parser<I, Vec<O>, E> {
    many(self)
  }

  fn preceded_by<F>(self, first: impl Parser<I, F, E>) -> impl Parser<I, O, E> {
    second(first, self)
  }

  fn followed_by<S>(self, second: impl Parser<I, S, E>) -> impl Parser<I, O, E> {
    first(self, second)
  }
}

impl<I, O, F: Clone, E> Parser<I, O, E> for F
  where F: FnMut(&mut ParseContext<I>) -> Result<O, ParseError<E>>
{
  fn parse(&mut self, context: &mut ParseContext<I>) -> Result<O, ParseError<E>> {
    self(context)
  }
}

pub fn end<I, E>(context: &mut ParseContext<I>) -> Result<(), ParseError<E>> {
  if let Some(_) = context.current() {
    context.throw_no_match()
  } else {
    Ok(())
  }
}

pub fn any<I: Clone, E>(context: &mut ParseContext<I>) -> Result<I, ParseError<E>> {
  if let Some(i) = context.current() {
    let i = i.clone();
    context.position += 1;
    Ok(i)
  } else {
    context.throw_end()
  }
}

pub fn satisfy<I: Clone, E>(mut test: impl FnMut(&I) -> bool + Clone) -> impl Parser<I, I, E> {
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

pub fn map<I, O, E, M>(mut parser: impl Parser<I, O, E>, mut f: impl FnMut(O) -> M + Clone) -> impl Parser<I, M, E> {
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
/// There are also tricks that can be done when the matched value must be followed
/// only by a few possible delimeters. (See `json::jnumber` for an example.)
pub fn partial<I, O, E>(mut parser: impl Parser<I, O, E>, at: usize) -> impl Parser<I, O, E> {
  move |context: &mut ParseContext<I>| {
    let initial_position = context.position;
    match parser.parse(context) {
      Err(NoMatch(err_position)) if err_position - initial_position >= at => Err(PartialMatch(err_position)),
      Err(End(err_position)) if err_position - initial_position >= at => Err(PartialMatch(err_position)),
      other => other
    }
  }
}

pub fn or<I, O, E>(mut first: impl Parser<I, O, E>, mut second: impl Parser<I, O, E>) -> impl Parser<I, O, E> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    match first.parse(context) {
      Err(NoMatch(_)) => { 
        context.position = position;
        second.parse(context)
      },
      Err(End(_)) => {
        context.position = position;
        second.parse(context)
      },
      other => other
    }
  }
}

pub fn many<I, O, E>(mut parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    loop {
      let position = context.position;
      match parser.parse(context) {
        Ok(output) => outputs.push(output),
        _ => {
          context.position = position;
          break
        } 
      }
    }
    Ok(outputs)
  }
}

pub fn count<I, O, E>(count: usize, mut parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    while outputs.len() < count {
      outputs.push(parser.parse(context)?);
    }
    Ok(outputs)
  }
}

pub fn upto<I, O, E>(upto: usize, mut parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
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

pub fn chains<I, O, E>(mut first: impl Parser<I, Vec<O>, E>, mut second: impl Parser<I, Vec<O>, E>) -> impl Parser<I, Vec<O>, E> {
  move |context: &mut ParseContext<I>| {
    let mut first_output = first.parse(context)?;
    let second_output = second.parse(context)?;
    first_output.extend(second_output);
    Ok(first_output)
  }
}

pub fn first<I, F, S, E>(mut first: impl Parser<I, F, E>, mut second: impl Parser<I, S, E>) -> impl Parser<I, F, E> {
  move |context: &mut ParseContext<I>| {
    let first_output = first.parse(context)?;
    second.parse(context)?;
    Ok(first_output)
  }
}

pub fn second<I, F, S, E>(mut first: impl Parser<I, F, E>, mut second: impl Parser<I, S, E>) -> impl Parser<I, S, E> {
  move |context: &mut ParseContext<I>| {
    first.parse(context)?;
    second.parse(context)
  }
}

pub fn just<I, O, E>(mut make: impl FnMut() -> O + Clone) -> impl Parser<I, O, E> {
  move |_: &mut ParseContext<I>| {
    Ok(make()) 
  }
}

pub fn peek<I, O, E>(mut parser: impl Parser<I, O, E>) -> impl Parser<I, (), E> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    parser.parse(context).map(|_| {
      context.position = position;
    })
  }
}

pub fn not<I, O, E>(mut parser: impl Parser<I, O, E>) -> impl Parser<I, (), E> {
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
