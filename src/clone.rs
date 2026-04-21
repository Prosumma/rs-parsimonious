use crate::ext::*;
use crate::flatten::*;
use crate::parser::{ParseResult, Parser};
use std::cell::RefCell;
use std::rc::Rc;

pub trait CloneParser<I, O>: Parser<I, O> + Clone {
    fn many1(self) -> impl Parser<I, Vec<O>>
    where
        I: Clone,
    {
        concat(self.clone().to_vec(), self.many()).flatten()
    }
}

impl<I, O, P> Parser<I, O> for Rc<RefCell<P>>
where
    P: Parser<I, O>,
{
    fn parse(&mut self, input: I) -> ParseResult<I, O> {
        self.borrow_mut().parse(input)
    }
}

impl<I, O, P> CloneParser<I, O> for Rc<RefCell<P>> where P: Parser<I, O> {}
