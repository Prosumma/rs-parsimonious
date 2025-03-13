
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseErrorReason {
  NoMatch,
  End,
}

pub use ParseErrorReason::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ParseError<E = ()> {
  pub reason: ParseErrorReason,
  pub position: usize,
  pub partial: bool,
  pub extra: Option<E>,
}

impl<E> ParseError<E> {
  pub fn new(reason: ParseErrorReason, position: usize, partial: bool, extra: Option<E>) -> ParseError<E> {
    ParseError { reason, position, partial, extra }
  }
}

pub struct ParseContext<'a, I> {
  input: &'a [I],
  pub position: usize,
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

  pub fn err_extra<E>(&self, reason: ParseErrorReason, partial: bool, extra: Option<E>) -> ParseError<E> {
    ParseError::new(reason, self.position, partial, extra)
  }

  pub fn throw_err_extra<O, E>(&self, reason: ParseErrorReason, partial: bool, extra: Option<E>) -> Result<O, ParseError<E>> {
    Err(self.err_extra(reason, partial, extra))
  }

  pub fn err<E>(&self, reason: ParseErrorReason) -> ParseError<E> {
    self.err_extra(reason, false, None)
  }

  pub fn throw_err<O, E>(&self, reason: ParseErrorReason) -> Result<O, ParseError<E>> {
    self.throw_err_extra(reason, false, None)
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
  if context.current().is_some() {
    context.throw_err(NoMatch)
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
    context.throw_err(End)
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
        context.throw_err(NoMatch)
      }
    } else {
      context.throw_err(End)
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
      Err(mut err) if !err.partial && err.position - initial_position >= at => {
        err.partial = true;
        Err(err)
      }
      other =>
        other
    }
  }
}

pub fn or<I, O, E>(mut first: impl Parser<I, O, E>, mut second: impl Parser<I, O, E>) -> impl Parser<I, O, E> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    match first.parse(context) {
      Err(err) if !err.partial => {
        context.position = position;
        second.parse(context)
      },
      other => other,
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
        Err(err) if err.partial => return Err(err),
        _ => {
          context.position = position;
          break
        }, 
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
      match parser.parse(context) {
        Ok(output) => outputs.push(output),
        Err(err) if err.partial => return Err(err),
        _ => {
          context.position = position;
          break
        }, 
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
      context.throw_err(NoMatch)
    }
  }
}
