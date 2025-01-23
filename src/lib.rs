#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParseError {
  NoMatch(usize),
  EndOfInput
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParseOutput<O> {
  pub output: O,
  pub position: usize
}

impl<O> ParseOutput<O> {
  pub fn new(output: O, position: usize) -> ParseOutput<O> {
    ParseOutput{output, position}
  }
  
  pub fn map<M, F>(self, transform: F) -> ParseOutput<M>
    where F: FnOnce(O) -> M
  {
    ParseOutput::new(transform(self.output), self.position)
  }
}

pub type ParseResult<O> = Result<ParseOutput<O>, ParseError>;

macro_rules! ok {
  ($input:expr, $position:expr) => { Ok(ParseOutput::new($input, $position)) }  
}

macro_rules! no {
  ($position:expr) => { Err(ParseError::NoMatch($position)) }
}

pub trait Parser<I, O>: Clone {
  fn parse(&self, input: &[I], position: usize) -> ParseResult<O>;

  fn map<M>(self, transform: impl Fn(O) -> M + Clone) -> impl Parser<I, M> where Self: Sized {
    map(self, transform)
  }

  fn to_vec(self) -> impl Parser<I, Vec<O>> where Self: Sized {
    to_vec(self)
  }

  fn many(self) -> impl Parser<I, Vec<O>> where Self: Sized {
    many(self)
  }

  fn many1(self) -> impl Parser<I, Vec<O>> where Self: Sized {
    many1(self)
  }

  fn or(self, other: impl Parser<I, O>) -> impl Parser<I, O> where Self: Sized {
    or(self, other)
  }

  fn preceded_by<P>(self, other: impl Parser<I, P>) -> impl Parser<I, O> where Self: Sized {
    second(other, self)
  }

  fn preceding<P>(self, other: impl Parser<I, P>) -> impl Parser<I, P> where Self: Sized {
    second(self, other)
  }

  fn followed_by<F>(self, other: impl Parser<I, F>) -> impl Parser<I, O> where Self: Sized {
    first(self, other)
  }

  fn end(self) -> impl Parser<I, O> where Self: Sized {
    self.followed_by(end)
  }
}

impl<I, O, F> Parser<I, O> for F
  where F: Fn(&[I], usize) -> ParseResult<O> + Clone
{
  fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
    self(input, position) 
  }
}

pub trait StringParser<I>: Parser<I, Vec<char>> {
  fn to_string(self) -> impl Parser<I, String> where Self: Sized {
    self.map(|chars| chars.into_iter().collect())
  }
}

impl<I, P> StringParser<I> for P where P: Parser<I, Vec<char>> {}

pub fn satisfy<I: Clone>(test: impl Fn(&I) -> bool + Clone) -> impl Parser<I, I> {
  move |input: &[I], position: usize| {
    input.get(position)
      .filter(|i| test(i))
      .map_or_else(
        || Err(ParseError::EndOfInput),
        |i| ok!(i.clone(), position + 1)
      )
  }
}

pub fn eq<I: Clone + PartialEq>(model: I) -> impl Parser<I, I> {
  satisfy(move |i| *i == model)
}

pub fn eqs<I: Clone + PartialEq>(models: Vec<I>) -> impl Parser<I, Vec<I>> {
  move |input: &[I], position: usize| {
    let mut position = position;
    for model in models.iter() {
      let output = eq(model.clone()).parse(input, position)?;
      position = output.position;
    }
    ok!(models.clone(), position)
  }
}

pub fn eq_str<S: AsRef<str>>(s: S, case_sensitive: bool) -> impl Parser<char, String> {
  let chars: Vec<char> = s.as_ref().chars().collect();
  move |input: &[char], position: usize| {
    let mut outputs = Vec::new();
    let mut position = position;
    for c in chars.iter() {
      let test = |i: &char| {
        if case_sensitive {
          i.to_ascii_lowercase() == c.to_ascii_lowercase()
        } else {
          i == c
        }
      };
      let output = satisfy(test).parse(input, position)?; 
      outputs.push(output.output);
      position = output.position;
    }
    ok!(outputs.into_iter().collect(), position)
  }
}

pub fn string<S: AsRef<str>>(s: S) -> impl Parser<char, String> {
  eq_str(s, false)
}

pub fn istring<S: AsRef<str>>(s: S) -> impl Parser<char, String> {
  eq_str(s, true)
}

pub fn map<I, O, M>(parser: impl Parser<I, O>, transform: impl Fn(O) -> M + Clone) -> impl Parser<I, M> {
  move |input: &[I], position: usize| {
    parser.parse(input, position).map(|output| output.map(&transform))
  }  
}

pub fn to_vec<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  map(parser, |output| vec![output])
}

pub fn chains<I, O>(first: impl Parser<I, Vec<O>>, second: impl Parser<I, Vec<O>>) -> impl Parser<I, Vec<O>> {
  move |input: &[I], position: usize| {
    let mut first_output = first.parse(input, position)?;
    let second_output = second.parse(input, first_output.position)?;
    first_output.output.extend(second_output.output);
    ok!(first_output.output, second_output.position)
  }
}

#[macro_export]
macro_rules! chains {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    chains($parser, chains!($($rest),+))
  }
}

#[macro_export]
macro_rules! chain {
  ($parser:expr) => { $parser.to_vec() };
  ($parser:expr, $($rest:expr),+) => {
    chains($parser.to_vec(), chain!($($rest),+))
  }
}

pub fn or<I, O>(first: impl Parser<I, O>, second: impl Parser<I, O>) -> impl Parser<I, O> {
  move |input: &[I], position: usize| {
    first.parse(input, position).or_else(|_| second.parse(input, position))
  }
}

#[macro_export]
macro_rules! or {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    or($parser, or!($($rest),+))
  }
}

pub fn one_of<I: Clone + PartialEq>(values: Vec<I>) -> impl Parser<I, I> {
  move |input: &[I], position: usize| {
    for value in values.iter() {
      let result = eq(value.clone()).parse(input, position);
      if result.is_ok() {
        return result
      }
    }
    no!(position)
  }
}

pub fn one_of_str<S: AsRef<str>>(s: S) -> impl Parser<char, char> {
  one_of(s.as_ref().chars().collect())
}

pub fn default<I, O>(parser: impl Parser<I, O>, make_default: impl Fn() -> O + Clone) -> impl Parser<I, O> {
  move |input: &[I], position: usize| {
    parser.parse(input, position).or(ok!(make_default(), position))
  }
}

pub fn many<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  move |input: &[I], position: usize| {
    let mut outputs = Vec::new();
    let mut position = position;
    while let Ok(output) = parser.parse(input, position) {
      outputs.push(output.output);
      position = output.position;
    }
    ok!(outputs, position)
  }
}

pub fn many1<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(parser))
}

pub fn end<I>(input: &[I], position: usize) -> ParseResult<()> {
  if input.get(position).is_none() {
    ok!((), position)
  } else {
    no!(position)
  }
}

pub fn first<I, F, S>(first: impl Parser<I, F>, second: impl Parser<I, S>) -> impl Parser<I, F> {
  move |input: &[I], position: usize| {
    let first_output = first.parse(input, position)?;
    let second_output = second.parse(input, first_output.position)?;
    ok!(first_output.output, second_output.position)
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
  move |input: &[I], position: usize| {
    let first_output = first.parse(input, position)?;
    second.parse(input, first_output.position)
  }
}

#[macro_export]
macro_rules! last {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    second($parser, last!($($rest),+))
  }
}

pub fn unref<I: Clone>(test: impl Fn(I) -> bool + Clone) -> impl Fn(&I) -> bool + Clone {
  move |i: &I| test(i.clone())
}

pub fn whitespace(input: &[char], position: usize) -> ParseResult<char> {
  satisfy(unref(char::is_whitespace)).parse(input, position)
}

pub fn parse<I, O>(input: &[I], parser: impl Parser<I, O>) -> ParseResult<O> {
  parser.parse(input, 0)
}

pub fn parse_str<S: AsRef<str>, O>(input: S, parser: impl Parser<char, O>) -> ParseResult<O> {
  parse(&input.as_ref().chars().collect::<Vec<char>>(), parser)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn eq_works() {
    let s = "a";
    let a = eq('a').end().to_vec().to_string();
    let result = parse_str(s, a);
    assert_eq!(result, ok!(s.to_string(), 1))
  }

  #[test]
  fn or_works() {
    let s = "aeiou  ";
    let p = or!(eq('a'), eq('e'), eq('i'), eq('o'), eq('u')) 
      .many()
      .followed_by(whitespace.many())
      .end()
      .to_string();
    let result = parse_str(s, p);
    assert_eq!(result, ok!(s.trim_end().to_string(), s.len()))
  }

  #[test]
  fn one_of_works() {
    let s = "aeiou";
    let p = one_of_str(s).many().followed_by(whitespace.many()).end().to_string();
    let result = parse_str(s, p);
    assert_eq!(result, ok!(s.trim_end().to_string(), s.len()))
  }

  #[test]
  fn istring_works() {
    let s = "DECLARE foo  ";
    let p = last!(
      istring("declare"),
      whitespace.many1(),
      one_of_str("fo").many1()
    );
    let p = p.followed_by(whitespace.many()).end().to_string();
    let result = parse_str(s, p);
    assert_eq!(result, ok!("foo".to_string(), 13))
  }
}
