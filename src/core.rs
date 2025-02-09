
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ParseError {
  NoMatch(usize),
  End
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

  pub fn at_end(&self) -> bool {
    self.position >= self.input.len()
  }

  pub fn err_no_match(&self) -> ParseError {
    ParseError::NoMatch(self.position)
  } 
}

pub trait Parser<I, O>: Clone {
  fn parse(&mut self, context: &mut ParseContext<I>) -> Result<O, ParseError>;

  fn map<M>(self, f: impl FnMut(O) -> M + Clone) -> impl Parser<I, M> {
    map(self, f)
  }

  fn many(self) -> impl Parser<I, Vec<O>> {
    many(self)
  }

  fn or(self, second: impl Parser<I, O>) -> impl Parser<I, O> {
    or(self, second)
  }

  fn peek(self) -> impl Parser<I, ()> {
    peek(self)
  }

  fn followed_by<S>(self, follower: impl Parser<I, S>) -> impl Parser<I, O> {
    first(self, follower)
  }

  fn preceded_by<F>(self, precedent: impl Parser<I, F>) -> impl Parser<I, O> {
    second(precedent, self)
  }

  fn surrounded_by<S>(self, surrounder: impl Parser<I, S>) -> impl Parser<I, O> {
    self.preceded_by(surrounder.clone()).followed_by(surrounder)
  }

  fn end(self) -> impl Parser<I, O> {
    self.followed_by(end)
  }
}

impl<I, O, F: Clone> Parser<I, O> for F
  where F: FnMut(&mut ParseContext<I>) -> Result<O, ParseError>
{
  fn parse(&mut self, context: &mut ParseContext<I>) -> Result<O, ParseError> {
    self(context)
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
        Err(context.err_no_match())
      }
    } else {
      Err(ParseError::End)
    }
  }
}

pub fn map<I, O, M>(mut parser: impl Parser<I, O>, mut f: impl FnMut(O) -> M + Clone) -> impl Parser<I, M> {
  move |context: &mut ParseContext<I>| {
    parser.parse(context).map(&mut f)
  }
}

pub fn chains<I, O>(mut first: impl Parser<I, Vec<O>>, mut second: impl Parser<I, Vec<O>>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    let output = first.parse(context)?;
    outputs.extend(output);
    let output = second.parse(context)?;
    outputs.extend(output);
    Ok(outputs)
  }
}

pub fn many<I, O>(mut parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut outputs = Vec::new();
    let mut position = context.position;
    while let Ok(output) = parser.parse(context) {
      position = context.position;
      outputs.push(output);
    }
    context.position = position;
    Ok(outputs)
  }
}

pub fn or<I, O>(mut first: impl Parser<I, O>, mut second: impl Parser<I, O>) -> impl Parser<I, O> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    first.parse(context).or_else(|_| {
      context.position = position;
      second.parse(context)
    })
  }
}

/// Matches both parsers in order, but returns the value of only the first.
pub fn first<I, O, S>(mut first: impl Parser<I, O>, mut second: impl Parser<I, S>) -> impl Parser<I, O> {
  move |context: &mut ParseContext<I>| {
    let output = first.parse(context)?;
    second.parse(context)?;
    Ok(output)
  }
}

/// Matches both parsers in order, but returns the value of only the second. 
pub fn second<I, O, S>(mut first: impl Parser<I, O>, mut second: impl Parser<I, S>) -> impl Parser<I, S> {
  move |context: &mut ParseContext<I>| {
    first.parse(context)?;
    second.parse(context)
  }
}

pub fn peek<I, O>(mut parser: impl Parser<I, O>) -> impl Parser<I, ()> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    let result = parser.parse(context);
    if result.is_ok() {
      context.position = position;
      Ok(())
    } else {
      Err(context.err_no_match())
    }
  }
}

pub fn not<I, O>(mut parser: impl Parser<I, O>) -> impl Parser<I, ()> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    let result = parser.parse(context);
    if result.is_ok() {
      Err(context.err_no_match())
    } else {
      context.position = position;
      Ok(())
    }
  }
}

pub fn just<I, O>(mut value: impl FnMut() -> O + Clone) -> impl Parser<I, O> {
  move |_: &mut ParseContext<I>| {
    Ok(value())
  }
}

pub fn any<I: Clone>(context: &mut ParseContext<I>) -> Result<I, ParseError> {
  if let Some(i) = context.current() {
    Ok(i.clone())
  } else {
    Err(ParseError::End)
  }
}

pub fn end<I>(context: &mut ParseContext<I>) -> Result<(), ParseError> {
  if context.at_end() {
    Ok(())
  } else {
    Err(context.err_no_match())
  }
}

pub fn flatten<I, O>(parser: impl Parser<I, Vec<Vec<O>>>) -> impl Parser<I, Vec<O>> {
  map(parser, |outputs| {
    let mut output = Vec::new();
    for elem in outputs {
      output.extend(elem);
    }
    output
  })
}

pub trait FlattenParser<I, O>: Parser<I, Vec<Vec<O>>> {
  fn flatten(self) -> impl Parser<I, Vec<O>> {
   flatten(self)
  }
}

impl<I, O, P> FlattenParser<I, O> for P where P: Parser<I, Vec<Vec<O>>> {}

pub trait StringParser<I>: Parser<I, Vec<char>> {
  fn to_string(self) -> impl Parser<I, String> {
    map(self, |chars| chars.into_iter().collect())
  }
}

impl<I, P> StringParser<I> for P where P: Parser<I, Vec<char>> {}
