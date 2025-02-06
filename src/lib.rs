mod core;

#[allow(unused_imports)]
pub use core::*;

#[macro_export]
macro_rules! chain {
  ($parser:expr) => { to_vec($parser) };
  ($parser:expr, $($rest:expr),+) => {
    chains(to_vec($parser), chain!($($rest),+))
  };
}

pub fn to_vec<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  map(parser, |output| vec![output])
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

pub fn many1<I, O>(parser: impl Parser<I, O>) -> impl Parser<I, Vec<O>> {
  chains(to_vec(parser.clone()), many(parser))
}
