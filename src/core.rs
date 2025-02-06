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
    if let Ok(output) = parser.parse(context) {
      outputs.push(output);
    }
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
