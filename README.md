# Parsimonious

A pure functional parser combinator crate written in Rust.

## Purpose

Don't use this. It works well and is easy to use, but there are much more performant and Rusty parsers out there like [nom](https://docs.rs/nom/latest/nom/).

The purpose of this is to teach myself some of the more advanced features of Rust, like generics, traits, and the semantics of closures.

The core of the design can be seen below. Any function of the form `Fn(&[I], usize) -> ParseResult<O> + Clone` is a `Parser<I, O>`. Combinators like `satisfy` return `impl Parser` to avoid an explosion of complex types, at the risk of monomorphization bloat. `Clone` is required because without this, it's very difficult to reuse a parser.

Can you tell I mostly write Haskell? 😀

```rust
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
}

pub type ParseResult<O> = Result<ParseOutput<O>, ParseError>;

pub trait Parser<I, O>: Clone {
  fn parse(&self, input: &[I], position: usize) -> ParseResult<O>;
}

impl<I, O, F> Parser<I, O> for F
  where F: Fn(&[I], usize) -> ParseResult<O> + Clone
{
  fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
    self(input, position) 
  }
}

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
```
