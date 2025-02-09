mod core;
pub mod json;

#[allow(unused_imports)]
pub use core::*;

#[macro_export]
macro_rules! or {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    or($parser, or!($($rest),+))
  };
}

#[macro_export]
macro_rules! chain {
  ($parser:expr) => { to_vec($parser) };
  ($parser:expr, $($rest:expr),+) => {
    chains(to_vec($parser), chain!($($rest),+))
  };
}

pub fn eq<I: Clone + PartialEq>(model: I) -> impl Parser<I, I> {
  satisfy(move |i| *i == model)
}

pub fn eq_char(model: char, case_sensitive: bool) -> impl Parser<char, char> {
  satisfy(move |c: &char| (case_sensitive && *c == model) || (!case_sensitive && c.to_ascii_lowercase() == model.to_ascii_lowercase()))
}

pub fn one_of<I: Clone + PartialEq>(values: Vec<I>) -> impl Parser<I, I> {
  satisfy(move |i| values.contains(i))
}

pub fn one_of_char(chars: Vec<char>, case_sensitive: bool) -> impl Parser<char, char> {
  satisfy(move |c: &char| {
    for ch in &chars {
      if case_sensitive {
        if c == ch {
          return true
        }
      } else if c.to_ascii_lowercase() == ch.to_ascii_lowercase() {
        return true
      }
    }
    false
  })
}

pub fn one_of_str<S: AsRef<str>>(s: S, case_sensitive: bool) -> impl Parser<char, char> {
  one_of_char(s.as_ref().chars().collect(), case_sensitive)
}

pub fn eq_str<S: AsRef<str>>(s: S, case_sensitive: bool) -> impl Parser<char, Vec<char>> {
  let chars: Vec<char> = s.as_ref().chars().collect();
  move |context: &mut ParseContext<char>| {
    let mut string = Vec::new();
    for c in &chars {
      let mut parser = eq_char(*c, case_sensitive);
      string.push(parser.parse(context)?);
    }
    Ok(string)
  }
}

pub fn string<S: AsRef<str>>(s: S) -> impl Parser<char, Vec<char>> {
  eq_str(s, true)
}

pub fn istring<S: AsRef<str>>(s: S) -> impl Parser<char, Vec<char>> {
  eq_str(s, false)
}

pub fn to_vec<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  map(parser, |output| vec![output])
}

pub fn many1<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(parser))
}

pub fn many1_sep<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(second(sep, parser)))
}

pub fn many_sep<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
  or(many1_sep(parser, sep), just(Vec::new))
}

pub trait ExtParser<I, O>: Parser<I, O> {
  fn to_vec(self) -> impl Parser<I, Vec<O>> {
    to_vec(self)
  }

  fn many_sep<S>(self, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
    many_sep(self, sep)
  }

  fn many1(self) -> impl Parser<I, Vec<O>> {
    many1(self)
  }

  fn many1_sep<S>(self, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
    many1_sep(self, sep)
  }
}

impl<I, O, P: Clone> ExtParser<I, O> for P where P: Parser<I, O> {}

pub fn whitespace(context: &mut ParseContext<char>) -> Result<char, ParseError> {
  satisfy(|c: &char| c.is_whitespace()).parse(context)
}

pub trait CharParser<O>: Parser<char, O> {
  fn whitespaced(self) -> impl Parser<char, O> {
    self.surrounded_by(whitespace.many())
  }

  fn single_quoted(self) -> impl Parser<char, O> {
    self.surrounded_by(eq('\''))
  }

  fn double_quoted(self) -> impl Parser<char, O> {
    self.surrounded_by(eq('"'))
  }

  fn delimited_by(self, start: char, end: char) -> impl Parser<char, O> {
    self.preceded_by(eq(start)).followed_by(eq(end))
  }

  fn braced(self) -> impl Parser<char, O> {
    self.delimited_by('{', '}')
  }

  fn bracketed(self) -> impl Parser<char, O> {
    self.delimited_by('[', ']')
  }

  fn parenthesized(self) -> impl Parser<char, O> {
    self.delimited_by('(', ')')
  }
}

impl<O, P> CharParser<O> for P where P: Parser<char, O> {}

pub fn parse<I, O>(input: &[I], mut parser: impl Parser<I, O>) -> Result<O, ParseError> {
  let mut context = ParseContext::new(input);
  parser.parse(&mut context)
}

pub fn parse_str<O, S: AsRef<str>>(input: S, parser: impl Parser<char, O>) -> Result<O, ParseError> {
  let chars: Vec<char> = input.as_ref().chars().into_iter().collect();
  parse(&chars, parser)
}