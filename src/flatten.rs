pub use crate::ext::*;
pub use crate::parser::Parser;

pub trait FlattenParser<I, O>: Parser<I, Vec<Vec<O>>> {
    fn flatten(self) -> impl Parser<I, Vec<O>> {
        self.map(|vecs| vecs.into_iter().flatten().collect())
    }
}

impl<I, O, P> FlattenParser<I, O> for P where P: Parser<I, Vec<Vec<O>>> {}
