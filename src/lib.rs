mod core;
pub mod json;

pub use core::*;
use std::ops::RangeInclusive;

#[macro_export]
macro_rules! or {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => { or($parser, or!($($rest),+)) };
}

#[macro_export]
macro_rules! chains {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => { chains($parser, chains!($($rest),+)) };
}

pub fn void<I, O, E>(parser: impl Parser<I, O, E>) -> impl Parser<I, (), E> {
  parser.map(|_| ())
}

#[macro_export]
macro_rules! void {
  ($parser:expr) => { void($parser) };
  ($parser:expr, $($rest:expr),+) => { or(void($parser), void!($($rest),+))}
}

#[macro_export]
macro_rules! peek {
  ($($parser:expr),+) => { peek(void!($($parser),+)) }
}

pub fn ch<E>(mut test: impl FnMut(char) -> bool + Clone) -> impl Parser<char, char, E> {
  satisfy(move |c: &char| test(*c)) 
}

pub fn eq<I: Clone + PartialEq, E>(model: I) -> impl Parser<I, I, E> {
  satisfy(move |i: &I| *i == model)
}

pub fn one_of<I: Clone + PartialEq, E>(choices: Vec<I>) -> impl Parser<I, I, E> {
  satisfy(move |i: &I| choices.contains(i))
}

pub fn one_of_str<S: AsRef<str> + Clone, E>(string: S, case_sensitive: bool) -> impl Parser<char, char, E> {
  let chars: Vec<char> = string.as_ref().chars().map(|c| if case_sensitive { c } else { c.to_ascii_lowercase() }).collect();
  satisfy(move |c: &char| {
    let c = if case_sensitive { *c } else { c.to_ascii_lowercase() };
    chars.contains(&c)
  })
}

pub fn to_vec<I, O, E>(parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
  map(parser, |output| vec![output])
}

pub fn many1<I, O, E>(parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
  chains(to_vec(parser.clone()), many(parser))
}

pub trait ExtParser<I, O, E>: Parser<I, O, E> {
  fn to_vec(self) -> impl Parser<I, Vec<O>, E> {
    to_vec(self)
  }

  fn many_sep<S>(self, sep: impl Parser<I, S, E>) -> impl Parser<I, Vec<O>, E> {
    many_sep(self, sep)
  }

  fn many1(self) -> impl Parser<I, Vec<O>, E> {
    many1(self)
  }

  fn many1_sep<S>(self, sep: impl Parser<I, S, E>) -> impl Parser<I, Vec<O>, E> {
    many1_sep(self, sep)
  }

  fn optional(self) -> impl Parser<I, Vec<O>, E> {
    optional(self)
  }

  fn range(self, r: RangeInclusive<usize>) -> impl Parser<I, Vec<O>, E> {
    range(r, self)
  }

  fn end(self) -> impl Parser<I, O, E> {
    self.followed_by(end)
  }
}

impl<I, O, E, P> ExtParser<I, O, E> for P where P: Parser<I, O, E> {}

pub fn maybe<I, O, E>(parser: impl Parser<I, Vec<O>, E>) -> impl Parser<I, Vec<O>, E> {
  or(parser, just(Vec::new))
}

pub trait VecParser<I, O, E>: Parser<I, Vec<O>, E> {
  fn maybe(self) -> impl Parser<I, Vec<O>, E> {
    maybe(self)
  }

  fn chains(self, second: impl Parser<I, Vec<O>, E>) -> impl Parser<I, Vec<O>, E> {
    chains(self, second)
  }
}

impl<I, O, E, P> VecParser<I, O, E> for P where P: Parser<I, Vec<O>, E> {}

pub fn eqchar<E>(model: char, case_sensitive: bool) -> impl Parser<char, char, E> {
  satisfy(move |c: &char| {
    if case_sensitive {
      *c == model
    } else {
      c.to_ascii_lowercase() == model.to_ascii_lowercase()
    }
  })
}

pub fn eqstr<S: AsRef<str>, E>(model: S, case_sensitive: bool) -> impl Parser<char, Vec<char>, E> {
  let chars: Vec<char> = model.as_ref().chars().collect();
  move |context: &mut ParseContext<char>| {
    let mut output = Vec::new();
    for &model in &chars {
      output.push(eqchar(model, case_sensitive).parse(context)?);
    }
    Ok(output)
  }
}

pub fn string<S: AsRef<str>, E>(model: S) -> impl Parser<char, Vec<char>, E> {
  eqstr(model, true)
}

pub fn flatten<I, O, E>(mut parser: impl Parser<I, Vec<Vec<O>>, E>) -> impl Parser<I, Vec<O>, E> {
  move |context: &mut ParseContext<I>| {
    let mut output = Vec::new();
    let intermediates = parser.parse(context)?;
    for intermediate in intermediates {
      output.extend(intermediate);
    }
    Ok(output)
  }
}

pub trait FlattenParser<I, O, E>: Parser<I, Vec<Vec<O>>, E> {
  fn flatten(self) -> impl Parser<I, Vec<O>, E> {
    flatten(self)
  }
}

impl<I, O, E, P> FlattenParser<I, O, E> for P where P: Parser<I, Vec<Vec<O>>, E> {}

pub fn join<I, E>(mut first: impl Parser<I, String, E>, mut second: impl Parser<I, String, E>) -> impl Parser<I, String, E> {
  move |context: &mut ParseContext<I>| {
    let mut first_string = first.parse(context)?;
    let second_string = second.parse(context)?;
    first_string.push_str(&second_string);
    Ok(first_string)
  }
}

#[macro_export]
macro_rules! join {
  ($parser:expr) => { $parser };
  ($parser:expr, $($rest:expr),+) => {
    join($parser, join!($($rest),+))
  };
}

pub fn to_string<I, E>(parser: impl Parser<I, Vec<char>, E>) -> impl Parser<I, String, E> {
  parser.map(|output| output.into_iter().collect())
}

pub trait StringParser<I, E>: Parser<I, Vec<char>, E> {
  fn to_string(self) -> impl Parser<I, String, E> {
    to_string(self)
  }
}

impl<I, E, P> StringParser<I, E> for P where P: Parser<I, Vec<char>, E> {}

pub fn optional<I, O, E>(parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
  or(parser.to_vec(), just(Vec::new))
}

pub fn many1_sep<I, O, S, E>(parser: impl Parser<I, O, E>, sep: impl Parser<I, S, E>) -> impl Parser<I, Vec<O>, E> {
  chains(to_vec(parser.clone()), many(second(sep, parser)))
}

pub fn many_sep<I, O, S, E>(parser: impl Parser<I, O, E>, sep: impl Parser<I, S, E>) -> impl Parser<I, Vec<O>, E> {
  or(many1_sep(parser, sep), just(Vec::new))
}

pub fn surround<I, O, S, E>(parser: impl Parser<I, O, E>, sep: impl Parser<I, S, E>) -> impl Parser<I, O, E> {
  second(sep.clone(), first(parser, sep))
}

pub fn range<I, O, E>(range: RangeInclusive<usize>, parser: impl Parser<I, O, E>) -> impl Parser<I, Vec<O>, E> {
  chains(
    count(*range.start(), parser.clone()), 
    upto(range.end() - range.start(), parser)
  )
}

pub fn whitespace<E>(context: &mut ParseContext<char>) -> Result<char, ParseError<E>> {
  ch(char::is_whitespace).parse(context)
}

pub fn ascii_digit<E>(context: &mut ParseContext<char>) -> Result<char, ParseError<E>> {
  satisfy(char::is_ascii_digit).parse(context)
}

fn delimit<O, E>(start: char, parser: impl Parser<char, O, E>, end: char) -> impl Parser<char, O, E> {
  parser.preceded_by(eq(start)).followed_by(eq(end))
}

pub trait CharParser<O, E>: Parser<char, O, E> {
  fn surrounded_by<S>(self, sep: impl Parser<char, S, E>) -> impl Parser<char, O, E> {
    surround(self, sep)
  }

  /// Surrounded by *optional* whitespace.
  /// 
  /// For required whitespace, just say
  /// `parser.surrounded_by(whitespace.many1())`.
  fn whitespaced(self) -> impl Parser<char, O, E> {
    surround(self, whitespace.many())
  }

  fn double_quoted(self) -> impl Parser<char, O, E> {
    surround(self, eq('"'))
  }

  fn single_quoted(self) -> impl Parser<char, O, E> {
    surround(self, eq('\''))
  }

  fn parenthesized(self) -> impl Parser<char, O, E> {
    delimit('(', self, ')')
  }

  fn braced(self) -> impl Parser<char, O, E> {
    delimit('{', self, '}')
  }

  fn bracketed(self) -> impl Parser<char, O, E> {
    delimit('[', self, ']')
  }
}

impl<O, E, P> CharParser<O, E> for P where P: Parser<char, O, E> {}

pub fn parse<I, O, E>(input: &[I], mut parser: impl Parser<I, O, E>) -> Result<O, ParseError<E>> {
  let mut context = ParseContext::new(input);
  parser.parse(&mut context)
}

pub fn parse_str<S: AsRef<str>, O, E>(input: S, parser: impl Parser<char, O, E>) -> Result<O, ParseError<E>> {
  let chars: Vec<char> = input.as_ref().chars().collect();
  parse(&chars, parser)
}
