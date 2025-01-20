use std::{marker::PhantomData, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub struct ParseOutput<O> {
    pub output: O,
    pub position: usize
}

impl<O> ParseOutput<O> {
    fn new(output: O, position: usize) -> ParseOutput<O> {
        ParseOutput{output, position}
    }

    fn map<M, T>(self, transform: T) -> ParseOutput<M>
        where T: Fn(O) -> M
    {
        ParseOutput::new(transform(self.output), self.position)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ParseError {
    NoMatch(usize), EndOfInput 
}

pub type ParseResult<O> = Result<ParseOutput<O>, ParseError>;

pub trait Parser<I, O>: Sized {
    fn parse(&self, input: &[I], position: usize) -> ParseResult<O>;

    fn map<M, T>(self, transform: T) -> impl Parser<I, M> + Clone
        where T: Fn(O) -> M
    {
        map(self, transform)
    }

    fn to_vec(self) -> impl Parser<I, Vec<O>> + Clone {
        to_vec(self)
    }

    fn many(self) -> impl Parser<I, Vec<O>> + Clone
        where Self: Clone
    {
        many(self)
    }

    fn many1(self) -> impl Parser<I, Vec<O>> + Clone
        where Self: Clone
    {
        many1(self)
    }

    fn followed_by<N, S>(self, follower: S) -> impl Parser<I, O> + Clone
        where S: Parser<I, N>
    {
        first(self, follower)
    }

    fn preceding<N, S>(self, preceded: S) -> impl Parser<I, N> + Clone
        where S: Parser<I, N>
    {
        second(self, preceded)
    }

    fn preceded_by<P, F>(self, preceder: F) -> impl Parser<I, O> + Clone
        where F: Parser<I, P>
    {
        second(preceder, self)
    }

    fn surrounded_by<S, P>(self, surrounder: P) -> impl Parser<I, O> + Clone
        where P: Parser<I, S> + Clone
    {
        self.preceded_by(surrounder.clone()).followed_by(surrounder)
    }

    fn end_of_input(self) -> impl Parser<I, O> + Clone {
        self.followed_by(end_of_input())
    }
}

pub trait StringParser<I>: Parser<I, Vec<char>> {
    fn to_string(self) -> impl Parser<I, String> + Clone {
        self.map(|output| output.into_iter().collect())
    }
}

impl<I, P> StringParser<I> for P where P: Parser<I, Vec<char>> {}

pub trait FlattenParser<I, O>: Parser<I, Vec<Vec<O>>> {
    fn flatten(self) -> impl Parser<I, Vec<O>> {
        self.map(|output| output.into_iter().flatten().collect())
    }
}

impl<I, O, P> FlattenParser<I, O> for P where P: Parser<I, Vec<Vec<O>>> {}

impl<I, O, P> Parser<I, O> for Rc<P>
    where P: Parser<I, O>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
        self.as_ref().parse(input, position)
    }
}

struct Satisfy<T> {
    test: T
}

impl<I: Clone, T> Parser<I, I> for Satisfy<T>
    where T: Fn(I) -> bool
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<I> {
        if let Some(i) = input.get(position) {
            if (self.test)(i.clone()) {
                Ok(ParseOutput::new(i.clone(), position + 1))
            } else {
                Err(ParseError::NoMatch(position))
            }
        } else {
            Err(ParseError::EndOfInput)
        }
    }
}

pub fn satisfy<I: Clone, T>(test: T) -> impl Parser<I, I> + Clone
    where T: Fn(I) -> bool
{
    Rc::new(Satisfy{test})
}

pub fn eq<I: Clone + PartialEq>(model: I) -> impl Parser<I, I> + Clone {
    satisfy(move |i| i == model)
}

pub fn eqs<I: Clone + PartialEq>(models: Vec<I>) -> impl Parser<I, Vec<I>> + Clone {
    parser(move |input, position| {
        let mut outputs = Vec::new();
        let mut position = position;
        for model in models.iter() {
            let output = eq(model.clone()).parse(input, position)?;
            outputs.push(output.output.clone());
            position = output.position;
        }
        Ok(ParseOutput::new(outputs, position))
    })
}

struct Map<P, T, O> {
    parser: P,
    transform: T,
    _o: PhantomData<O>
}

impl<I, O, M, P, T> Parser<I, M> for Map<P, T, O>
    where P: Parser<I, O>, T: Fn(O) -> M
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<M> {
        self.parser.parse(input, position).map(|output| output.map(&self.transform))
    }
}

pub fn map<I, O, M, P, T>(parser: P, transform: T) -> impl Parser<I, M> + Clone
    where P: Parser<I, O>, T: Fn(O) -> M
{
    Rc::new(Map{parser, transform, _o: PhantomData})
}

pub fn to_vec<I, O, P>(parser: P) -> impl Parser<I, Vec<O>> + Clone
    where P: Parser<I, O>
{
    map(parser, |output| vec![output])
}

struct Parse<P> {
    parse: P
}

impl<I, O, P> Parser<I, O> for Parse<P>
    where P: Fn(&[I], usize) -> ParseResult<O>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
        (self.parse)(input, position)
    }
}

pub fn parser<I, O, P>(parse: P) -> impl Parser<I, O> + Clone
    where P: Fn(&[I], usize) -> ParseResult<O>
{
    Rc::new(Parse{parse})
}

pub fn one_of<I: Clone + PartialEq>(list: Vec<I>) -> impl Parser<I, I> + Clone {
    parser(move |input, position| {
        if let Some(i) = input.get(position) {
            for elem in list.iter() {
                if elem == i {
                    return Ok(ParseOutput::new(i.clone(), position + 1))
                }
            }
            Err(ParseError::NoMatch(position))
        } else {
            Err(ParseError::EndOfInput)
        }
    })
}

#[macro_export]
macro_rules! one_of {
    ($($elem:expr),+) => { one_of(vec![$($elem),+]) }
}

pub fn one_of_str<S: AsRef<str>>(s: S) -> impl Parser<char, char> + Clone {
    one_of(s.as_ref().chars().collect())
}

#[derive(Clone)]
struct Many<P> {
    parser: P 
}

impl<I, O, P> Parser<I, Vec<O>> for Many<P>
    where P: Parser<I, O>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<Vec<O>> {
        let mut outputs = Vec::new();
        let mut position = position;
        while let Ok(result) = self.parser.parse(input, position) {
            outputs.push(result.output);
            position = result.position;
        }
        Ok(ParseOutput::new(outputs, position))
    }
}

pub fn many<I, O, P: Clone>(parser: P) -> impl Parser<I, Vec<O>> + Clone
    where P: Parser<I, O>
{ 
    Many{parser}
}

struct First<F, S, N> {
    first: F,
    second: S,
    _n: PhantomData<N>
}

impl<I, O, N, F, S> Parser<I, O> for First<F, S, N>
    where F: Parser<I, O>, S: Parser<I, N>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
        let first_output = self.first.parse(input, position)?;
        let second_output = self.second.parse(input, first_output.position)?;
        Ok(ParseOutput::new(first_output.output, second_output.position))
    }
}

pub fn first<I, O, N, F, S>(first: F, second: S) -> impl Parser<I, O> + Clone
    where F: Parser<I, O>, S: Parser<I, N>
{
    Rc::new(First{first, second, _n: PhantomData})
}

#[macro_export]
macro_rules! first {
    ($parser:expr) => { $parser };
    ($parser:expr, $($rest:expr),+) => {
        first($parser, first!($($rest),+))
    }
}

struct Second<F, S, O> {
    first: F,
    second: S,
    _o: PhantomData<O>
}

impl<I, O, N, F, S> Parser<I, N> for Second<F, S, O>
    where F: Parser<I, O>, S: Parser<I, N>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<N> {
        let first_output = self.first.parse(input, position)?; 
        self.second.parse(input, first_output.position)
    }
}

pub fn second<I, O, N, F, S>(first: F, second: S) -> impl Parser<I, N> + Clone
    where F: Parser<I, O>, S: Parser<I, N>
{
    Rc::new(Second{first, second, _o: PhantomData})
}

#[macro_export]
macro_rules! last {
    ($parser:expr) => { $parser };
    ($parser:expr, $($rest:expr),+) => {
        second($parser, last!($($rest),+))
    }
}

struct EndOfInput<I> {
    _i: PhantomData<I>
}

impl<I> Parser<I, ()> for EndOfInput<I> {
    fn parse(&self, input: &[I], position: usize) -> ParseResult<()> {
        if input.get(position).is_none() {
            Ok(ParseOutput::new((), position))
        } else {
            Err(ParseError::NoMatch(position))
        }
    }
}

pub fn end_of_input<I>() -> impl Parser<I, ()> + Clone {
    Rc::new(EndOfInput{_i: PhantomData})
}

#[derive(Clone)]
struct Transact<P> {
    parser: P
}

impl<I, O, P> Parser<I, O> for Transact<P>
    where P: Parser<I, O>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
        match self.parser.parse(input, position) {
            Err(ParseError::NoMatch(_)) => Err(ParseError::NoMatch(position)),
            other => other
        }
    }
}

pub fn transact<I, O, P: Clone>(parser: P) -> impl Parser<I, O> + Clone
    where P: Parser<I, O>
{
    Transact{parser}
}

#[derive(Clone)]
struct Chains<F, S> {
    first: F,
    second: S
}

impl<I, O, F, S> Parser<I, Vec<O>> for Chains<F, S>
    where F: Parser<I, Vec<O>>, S: Parser<I, Vec<O>>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<Vec<O>> {
        let mut first_output = self.first.parse(input, position)?;
        let second_output = self.second.parse(input, first_output.position)?;
        first_output.output.extend(second_output.output);
        Ok(ParseOutput::new(first_output.output, second_output.position))
    }
}

pub fn chains<I, O, F: Clone, S: Clone>(first: F, second: S) -> impl Parser<I, Vec<O>> + Clone
    where F: Parser<I, Vec<O>>, S: Parser<I, Vec<O>>
{
    Chains{first, second}
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

pub fn many1<I, O, P: Clone>(parser: P) -> impl Parser<I, Vec<O>> + Clone
    where P: Parser<I, O>
{
    chains(parser.clone().to_vec(), many(parser))
}

#[derive(Clone)]
struct Or<F, S> {
    first: F,
    second: S
}

impl<I, O, F, S> Parser<I, O> for Or<F, S>
    where F: Parser<I, O>, S: Parser<I, O>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<O> {
        let result = self.first.parse(input, position);
        if result.is_ok() {
            result
        } else {
            self.second.parse(input, position)
        }
    }
}

pub fn or<I, O, F: Clone, S: Clone>(first: F, second: S) -> impl Parser<I, O> + Clone
    where F: Parser<I, O>, S: Parser<I, O>
{
    Or{first, second}
}

#[macro_export]
macro_rules! or {
    ($parser:expr) => { $parser };
    ($parser:expr, $($rest:expr),+) => {
        or($parser, or!($($rest),+))
    }
}

struct Void<P, O> {
    parser: P,
    _o: PhantomData<O>
}

impl<I, O, P> Parser<I, ()> for Void<P, O>
    where P: Parser<I, O>
{
    fn parse(&self, input: &[I], position: usize) -> ParseResult<()> {
        let output = self.parser.parse(input, position)?;
        Ok(ParseOutput::new((), output.position))
    }
}

pub fn void<I, O, P>(parser: P) -> impl Parser<I, ()> + Clone
    where P: Parser<I, O>
{
    Rc::new(Void{parser, _o: PhantomData})
}

#[macro_export]
macro_rules! void {
    ($($parsers:expr),+) => { void(first!($($parsers),+)) }
}

pub fn string<S: AsRef<str>>(s: S) -> impl Parser<char, Vec<char>> + Clone {
    eqs(s.as_ref().chars().collect())
}

pub fn parse<I, O>(input: &[I], parser: impl Parser<I, O>) -> ParseResult<O> {
    parser.parse(input, 0)
}

pub fn parse_str<S: AsRef<str>, O>(input: S, parser: impl Parser<char, O>) -> ParseResult<O> {
    let input: Vec<char> = input.as_ref().chars().collect();
    parse(&input, parser)
}

pub fn whitespace() -> impl Parser<char, char> + Clone {
    satisfy(char::is_whitespace)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eq_succeeds() {
        let s = "a";
        let a = eq('a');
        let r = parse_str(s, a);
        assert_eq!(r, Ok(ParseOutput::new('a', s.len())))
    }

    #[test]
    fn eq_fails_successfully() {
        let s = "a";
        let e = eq('e');
        let r = parse_str(s, e);
        assert_eq!(r, Err(ParseError::NoMatch(0))) 
    }

    #[test]
    fn one_of_succeeds() {
        let s = "  o ";
        let aeiou = one_of_str("aeiou").surrounded_by(whitespace().many());
        let r = parse_str(s, aeiou);
        assert_eq!(r, Ok(ParseOutput::new('o', s.len())))
    }

    #[test]
    fn first_succeeds() {
        let s = "e   ";
        let e = eq('e').followed_by(whitespace().many()).end_of_input();
        let r = parse_str(s, e);
        assert_eq!(r, Ok(ParseOutput::new('e', s.len())))
    }

    #[test]
    fn end_of_input_fails_successfully() {
        let s = "x";
        let e = end_of_input::<char>();
        let r = parse_str(s, e);
        assert_eq!(r, Err(ParseError::NoMatch(0)))
    }

    #[test]
    fn or_succeeds() {
        let s = "  oea ";
        let aeo = or!(eq('a'), eq('e'), eq('o'));
        let p = aeo.many().surrounded_by(whitespace().many()).end_of_input().to_string();
        let r = parse_str(s, p);
        assert_eq!(r, Ok(ParseOutput::new("oea".to_string(), s.len())))
    }

    #[test]
    fn void_succeeds() {
        let s = "FOO xyz";
        let p = void!(string("FOO"), whitespace().many1()).preceding(many1(one_of_str("xyz"))).end_of_input().to_string();
        let r = parse_str(s, p);
        assert_eq!(r, Ok(ParseOutput::new("xyz".to_string(), 7)));
    }
}
