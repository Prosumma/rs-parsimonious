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

pub fn ch(mut test: impl FnMut(char) -> bool + Clone) -> impl Parser<char, char> {
  satisfy(move |c: &char| test(*c)) 
}

pub fn eq<I: Clone + PartialEq>(model: I) -> impl Parser<I, I> {
  satisfy(move |i: &I| *i == model)
}

pub fn one_of<I: Clone + PartialEq>(choices: Vec<I>) -> impl Parser<I, I> {
  satisfy(move |i: &I| choices.contains(i))
}

pub fn one_of_str<S: AsRef<str> + Clone>(string: S, case_sensitive: bool) -> impl Parser<char, char> {
  let chars: Vec<char> = string.as_ref().chars().map(|c| if case_sensitive { c } else { c.to_ascii_lowercase() }).collect();
  satisfy(move |c: &char| {
    let c = if case_sensitive { *c } else { c.to_ascii_lowercase() };
    chars.contains(&c)
  })
}

pub fn to_vec<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  map(parser, |output| vec![output])
}

pub fn many1<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(parser))
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

  fn optional(self) -> impl Parser<I, Vec<O>> {
    optional(self)
  }

  fn range(self, r: RangeInclusive<usize>) -> impl Parser<I, Vec<O>> {
    range(r, self)
  }

  fn end(self) -> impl Parser<I, O> {
    self.followed_by(end)
  }
}

impl<I, O, P> ExtParser<I, O> for P where P: Parser<I, O> {}

pub fn maybe<I, O>(parser: impl Parser<I, Vec<O>>) -> impl Parser<I, Vec<O>> {
  or(parser, just(Vec::new))
}

pub trait VecParser<I, O>: Parser<I, Vec<O>> {
  fn maybe(self) -> impl Parser<I, Vec<O>> {
    maybe(self)
  }

  fn chains(self, second: impl Parser<I, Vec<O>>) -> impl Parser<I, Vec<O>> {
    chains(self, second)
  }
}

impl<I, O, P> VecParser<I, O> for P where P: Parser<I, Vec<O>> {}

pub fn eqchar(model: char, case_sensitive: bool) -> impl Parser<char, char> {
  satisfy(move |c: &char| {
    if case_sensitive {
      *c == model
    } else {
      c.to_ascii_lowercase() == model.to_ascii_lowercase()
    }
  })
}

pub fn eqstr<S: AsRef<str>>(model: S, case_sensitive: bool) -> impl Parser<char, Vec<char>> {
  let chars: Vec<char> = model.as_ref().chars().collect();
  move |context: &mut ParseContext<char>| {
    let mut output = Vec::new();
    for &model in &chars {
      output.push(eqchar(model, case_sensitive).parse(context)?);
    }
    Ok(output)
  }
}

pub fn string<S: AsRef<str>>(model: S) -> impl Parser<char, Vec<char>> {
  eqstr(model, true)
}

pub fn flatten<I, O>(mut parser: impl Parser<I, Vec<Vec<O>>>) -> impl Parser<I, Vec<O>> {
  move |context: &mut ParseContext<I>| {
    let mut output = Vec::new();
    let intermediates = parser.parse(context)?;
    for intermediate in intermediates {
      output.extend(intermediate);
    }
    Ok(output)
  }
}

pub trait FlattenParser<I, O>: Parser<I, Vec<Vec<O>>> {
  fn flatten(self) -> impl Parser<I, Vec<O>> {
    flatten(self)
  }
}

impl<I, O, P> FlattenParser<I, O> for P where P: Parser<I, Vec<Vec<O>>> {}

pub fn join<I>(mut first: impl Parser<I, String>, mut second: impl Parser<I, String>) -> impl Parser<I, String> {
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

pub fn to_string<I>(parser: impl Parser<I, Vec<char>>) -> impl Parser<I, String> {
  parser.map(|output| output.into_iter().collect())
}

pub trait StringParser<I>: Parser<I, Vec<char>> {
  fn to_string(self) -> impl Parser<I, String> {
    to_string(self)
  }
}

impl<I, P> StringParser<I> for P where P: Parser<I, Vec<char>> {}

pub fn optional<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  or(parser.to_vec(), just(Vec::new))
}

pub fn many1_sep<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(second(sep, parser)))
}

pub fn many_sep<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, Vec<O>> {
  or(many1_sep(parser, sep), just(Vec::new))
}

pub fn surround<I, O, S>(parser: impl Parser<I, O>, sep: impl Parser<I, S>) -> impl Parser<I, O> {
  second(sep.clone(), first(parser, sep))
}

pub fn range<I, O>(range: RangeInclusive<usize>, parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(
    count(*range.start(), parser.clone()), 
    upto(range.end() - range.start(), parser)
  )
}

pub fn whitespace(context: &mut ParseContext<char>) -> Result<char, ParseError> {
  ch(char::is_whitespace).parse(context)
}

pub fn ascii_digit(context: &mut ParseContext<char>) -> Result<char, ParseError> {
  satisfy(char::is_ascii_digit).parse(context)
}

fn delimit<O>(start: char, parser: impl Parser<char, O>, end: char) -> impl Parser<char, O> {
  parser.preceded_by(eq(start)).followed_by(eq(end))
}

pub trait CharParser<O>: Parser<char, O> {
  fn surrounded_by<S>(self, sep: impl Parser<char, S>) -> impl Parser<char, O> {
    surround(self, sep)
  }

  /// Surrounded by *optional* whitespace.
  /// 
  /// For required whitespace, just say
  /// `parser.surrounded_by(whitespace.many1())`.
  fn whitespaced(self) -> impl Parser<char, O> {
    surround(self, whitespace.many())
  }

  fn double_quoted(self) -> impl Parser<char, O> {
    surround(self, eq('"'))
  }

  fn single_quoted(self) -> impl Parser<char, O> {
    surround(self, eq('\''))
  }

  fn parenthesized(self) -> impl Parser<char, O> {
    delimit('(', self, ')')
  }

  fn braced(self) -> impl Parser<char, O> {
    delimit('{', self, '}')
  }

  fn bracketed(self) -> impl Parser<char, O> {
    delimit('[', self, ']')
  }
}

impl<O, P> CharParser<O> for P where P: Parser<char, O> {}

pub fn parse<I, O>(input: &[I], mut parser: impl Parser<I, O>) -> Result<O, ParseError> {
  let mut context = ParseContext::new(input);
  parser.parse(&mut context)
}

pub fn parse_str<S: AsRef<str>, O>(input: S, parser: impl Parser<char, O>) -> Result<O, ParseError> {
  let chars: Vec<char> = input.as_ref().chars().collect();
  parse(&chars, parser)
}
