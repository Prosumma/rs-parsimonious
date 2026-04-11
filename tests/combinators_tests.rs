use parsimonious::ExtParser;
use parsimonious::*;

#[test]
fn test_just_and_just_lazy() {
    let input = "abc";
    let parser = just(7u8);
    let result: ParseResult<&str, u8> = parse(input, parser);
    let success = result.unwrap();
    assert_eq!(success.output, 7);
    assert_eq!(success.input, "abc");

    use std::cell::Cell;
    use std::rc::Rc;
    let counter = Rc::new(Cell::new(0u8));
    let counter_handle = counter.clone();
    let mut parser = just_lazy(move || {
        let value = counter_handle.get();
        counter_handle.set(value + 1);
        value
    });

    let first: ParseResult<&str, u8> = parser.parse("x");
    let second: ParseResult<&str, u8> = parser.parse("y");
    assert_eq!(first.unwrap().output, 0);
    assert_eq!(second.unwrap().output, 1);
    assert_eq!(counter.get(), 2);
}

#[test]
fn test_item_and_item_str() {
    let input = [1u8, 2u8, 3u8];
    let result: ParseResult<&[u8], u8> = item(&input);
    let success = result.unwrap();
    assert_eq!(success.output, 1);
    assert_eq!(success.input, &input[1..]);

    let err: ParseResult<&[u8], u8> = item(&[]);
    assert_eq!(err.unwrap_err().reason, EOF);

    let input = "xyz";
    let result: ParseResult<&str, char> = item_str(input);
    let success = result.unwrap();
    assert_eq!(success.output, 'x');
    assert_eq!(success.input, "yz");

    let err: ParseResult<&str, char> = item_str("");
    assert_eq!(err.unwrap_err().reason, EOF);
}

#[test]
fn test_cond_and_or() {
    let parser_true = cond(true, 'a', 'b');
    let result: ParseResult<&str, char> = parse("abc", parser_true);
    assert_eq!(result.unwrap().output, 'a');

    let parser_false = cond(false, 'a', 'b');
    let result: ParseResult<&str, char> = parse("bcd", parser_false);
    assert_eq!(result.unwrap().output, 'b');

    let choices = or('x', 'y');
    let parser = or(choices.clone().bracketed(), choices.braced());
    let result: ParseResult<&str, char> = parse("{y}", parser);
    assert_eq!(result.unwrap().output, 'y');
}

#[test]
fn test_string_and_one_of_str() {
    let parser = string("aBc", true);
    let result: ParseResult<&str, &str> = parse("Abc", parser);
    let success = result.unwrap();
    assert_eq!(success.output, "Abc");
    assert_eq!(success.input, "");

    let parser = one_of_str("abc", true);
    let err: ParseResult<&str, char> = parse("XYZ", parser);
    assert_eq!(err.unwrap_err().reason, NoMatch);
}

#[test]
fn test_whitespace_concat_end() {
    let result: ParseResult<&str, char> = whitespace("\nrest");
    let success = result.unwrap();
    assert_eq!(success.output, '\n');
    assert_eq!(success.input, "rest");

    let parser = concat('a'.to_vec(), 'b'.to_vec());
    let result: ParseResult<&str, Vec<char>> = parse("ab", parser);
    let success = result.unwrap();
    assert_eq!(success.output, vec!['a', 'b']);
    assert_eq!(success.input, "");

    let result: ParseResult<&str, ()> = end("remaining");
    assert_eq!(result.unwrap_err().reason, NoMatch);

    let result: ParseResult<&[u8], ()> = end(&[1u8]);
    assert_eq!(result.unwrap_err().reason, NoMatch);
}
