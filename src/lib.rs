mod core;
pub mod json;

pub use core::*;
use std::ops::RangeInclusive;

pub trait ExtParser<I, O>: Parser<I, O> {
  fn map<M>(self, f: impl Fn(O) -> M + Clone) -> impl Parser<I, M> {
    map(self, f)
  }

  fn to_vec(self) -> impl Parser<I, Vec<O>> {
    to_vec(self)
  }

  fn many(self) -> impl Parser<I, Vec<O>> {
    many(self)
  }

  fn followed_by<S>(self, follower: impl Parser<I, S>) -> impl Parser<I, O> {
    first(self, follower)
  }

  fn preceded_by<F>(self, predecessor: impl Parser<I, F>) -> impl Parser<I, O> {
    second(predecessor, self)
  }

  fn preceding<S>(self, successor: impl Parser<I, S>) -> impl Parser<I, S> {
    second(self, successor)
  }

  fn surrounded_by<S>(self, surrounder: impl Parser<I, S>) -> impl Parser<I, O> {
    self.preceded_by(surrounder.clone()).followed_by(surrounder)
  }

  fn many_sep<S>(self, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
    many_sep(self, sep)
  }

  fn many1_sep<S>(self, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
    many1_sep(self, sep)
  }

  fn end(self) -> impl Parser<I, O> {
    self.followed_by(end)
  }
}

impl<I, O, P> ExtParser<I, O> for P where P: Parser<I, O> {}

pub trait StringParser<I>: Parser<I, Vec<char>> {
  fn to_string(self) -> impl Parser<I, String> {
    map(self, |output| output.into_iter().collect())
  }
}

impl<I, P> StringParser<I> for P where P: Parser<I, Vec<char>> {}

fn encircle<O>(parser: impl Parser<char, O>, start: char, end: char) -> impl Parser<char, O> {
  parser.preceded_by(eq(start)).followed_by(eq(end))
}

pub trait CharParser<O>: Parser<char, O> {
  fn whitespaced(self) -> impl Parser<char, O> {
    self.surrounded_by(whitespace.many())
  }

  fn bracketed(self) -> impl Parser<char, O> {
    encircle(self, '[', ']')
  }

  fn braced(self) -> impl Parser<char, O> {
    encircle(self, '{', '}')
  }

  fn parenthesized(self) -> impl Parser<char, O> {
    encircle(self, '(', ')')
  }

  fn single_quoted(self) -> impl Parser<char, O> {
    self.surrounded_by(eq('\''))
  }

  fn double_quoted(self) -> impl Parser<char, O> {
    self.surrounded_by(eq('"'))
  }
}

impl<O, P> CharParser<O> for P where P: Parser<char, O> {}

pub trait FlattenParser<I, O>: Parser<I, Vec<Vec<O>>> {
  fn flatten(self) -> impl Parser<I, Vec<O>> {
    flatten(self)
  }
}

impl<I, O, P> FlattenParser<I, O> for P where P: Parser<I, Vec<Vec<O>>> {}

#[macro_export]
macro_rules! or {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    or($parser, or!($($rest),+))
  }
}

pub fn eq<I: Clone + PartialEq>(model: I) -> impl Parser<I, I> {
  satisfy(move |i| *i == model)
}

pub fn ch(test: impl Fn(char) -> bool + Clone) -> impl Parser<char, char> {
  satisfy(move |c: &char| test(*c))
}

pub fn eqc(model: char, case_sensitive: bool) -> impl Parser<char, char> {
  ch(move |c| {
    if case_sensitive {
      c == model
    } else {
      c.to_ascii_lowercase() == model.to_ascii_lowercase()
    }
  })
}

pub fn to_vec<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  map(parser, |output| vec![output])
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

pub fn many1<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(parser.clone().to_vec(), many(parser))
}

pub fn many1_sep<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(second(sep, parser)))
}

pub fn many_sep<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
  or(many1_sep(parser, sep), just(Vec::new))
}

pub fn one_of<I: PartialEq + Clone>(choices: Vec<I>) -> impl Parser<I, I> {
  move |context: &mut ParseContext<I>| {
    let position = context.position;
    for choice in &choices {
      let result = eq(choice.clone()).parse(context);
      if result.is_ok() {
        return result
      } 
      context.position = position;
    }
    context.no_match()
  }
}

pub fn flatten<I, O>(parser: impl Parser<I, Vec<Vec<O>>>) -> impl Parser<I, Vec<O>> {
  map(parser, |output| {
    let mut outputs = Vec::new();
    for o in output {
      outputs.extend(o);
    }
    outputs
  })
}

pub fn range<I, O>(range: RangeInclusive<usize>, parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(count(*range.start(), parser.clone()), upto(range.end() - range.start(), parser))
}

pub fn eqs<S: AsRef<str>>(s: S, case_sensitive: bool) -> impl Parser<char, Vec<char>> {
  let chars: Vec<char> = s.as_ref().chars().collect();
  move |context: &mut ParseContext<char>| {
    let mut matched = Vec::new();
    for &c in &chars {
      let m = eqc(c, case_sensitive).parse(context)?;
      matched.push(m);
    }
    Ok(matched)
  }
}

pub fn string<S: AsRef<str>>(s: S) -> impl Parser<char, Vec<char>> {
  eqs(s, true)
}

pub fn istring<S: AsRef<str>>(s: S) -> impl Parser<char, Vec<char>> {
  eqs(s, false)
}

pub fn one_of_str<S: AsRef<str>>(s: S) -> impl Parser<char, char> {
  one_of(s.as_ref().chars().into_iter().collect())
}

pub fn whitespace(context: &mut ParseContext<char>) -> Result<char, ParseError> {
  ch(char::is_whitespace).parse(context)
}

pub fn parse<I, O>(input: &[I], parser: impl Parser<I, O>) -> Result<O, ParseError> {
  let mut context = ParseContext::new(input);
  parser.parse(&mut context)
}

pub fn parse_str<S: AsRef<str>, O>(input: S, parser: impl Parser<char, O>) -> Result<O, ParseError> {
  let chars: Vec<char> = input.as_ref().chars().into_iter().collect();
  parse(&chars, parser)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn ak() {
    let s = "aeiou";
    let p = many1(one_of_str("aeiou")).to_string().end();
    let r = parse_str(s, p);
    assert_eq!(r, Ok(s.to_string()))
  }

  #[test]
  fn ik() {
    let s = "kxt"; 
    let p = chains(
      // This demonstrates that `peek` can match, but does not move the position forward.
      many(one_of_str("kx")).followed_by(peek(eq('t'))),
      eq('t').to_vec()
    ).to_string();
    let r = parse_str(s, p);    
    assert_eq!(r, Ok("kxt".to_string()))
  }

  #[test]
  fn test_count() {
    let s = "dadada";
    let p = count(2, string("da")).flatten().to_string().end();
    let r = parse_str(s, p);
    assert_eq!(r, Err(ParseError::NoMatch(4)))
  }

  #[test]
  fn test_range() {
    let s = "kakakaka";
    let p = range(2..=4, string("ka")).flatten().to_string().end();
    let r = parse_str(s, p);
    assert_eq!(r, Ok(s.to_string()));

    let p = range(2..=3, string("ka")).flatten().to_string().end();
    let r = parse_str(s, p);
    assert_eq!(r, Err(ParseError::NoMatch(6)));
  }
}
