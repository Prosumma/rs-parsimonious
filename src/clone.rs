use crate::ext::*;
use crate::parser::{ParseResult, Parser};
use std::cell::RefCell;
use std::rc::Rc;

pub trait CloneParser<I, O>: Parser<I, O> + Clone {
    fn many1(self) -> impl Parser<I, Vec<O>>
    where
        I: Clone,
    {
        cons(self.clone(), self.many())
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::string::*;

    #[test]
    fn test_many1() {
        let parser = eq_istr("BOB");
        let parser = RefCell::new(parser);
        let mut parser = Rc::new(parser).many1();
        let res = parser.parse("bobbobbob");
        assert!(res.is_ok());
        assert_eq!(res.unwrap().output, vec!["bob", "bob", "bob"])
    }
}
