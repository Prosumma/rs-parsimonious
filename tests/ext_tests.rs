use parsimonious::*;
use parsimonious::{ExtParser, StrInParser};

#[test]
fn test_map_void_peek_not() {
    let parser = 'a'.map(|c| c as u8);
    let result: ParseResult<&str, u8> = parse("ab", parser);
    let success = result.unwrap();
    assert_eq!(success.output, b'a');
    assert_eq!(success.input, "b");

    let parser = 'a'.void();
    let result: ParseResult<&str, ()> = parse("ab", parser);
    let success = result.unwrap();
    assert_eq!(success.output, ());
    assert_eq!(success.input, "b");

    let parser = 'a'.peek();
    let result: ParseResult<&str, char> = parse("ab", parser);
    let success = result.unwrap();
    assert_eq!(success.output, 'a');
    assert_eq!(success.input, "ab");

    let parser = 'a'.not();
    let result: ParseResult<&str, ()> = parse("b", parser);
    let success = result.unwrap();
    assert_eq!(success.output, ());
    assert_eq!(success.input, "b");
}

#[test]
fn test_many_and_count_variants() {
    let parser = 'a'.many();
    let result: ParseResult<&str, Vec<char>> = parse("aaab", parser);
    let success = result.unwrap();
    assert_eq!(success.output, vec!['a', 'a', 'a']);
    assert_eq!(success.input, "b");

    let parser = 'a'.many1();
    let result: ParseResult<&str, Vec<char>> = parse("aab", parser);
    let success = result.unwrap();
    assert_eq!(success.output.len(), 2);
    assert_eq!(success.input, "b");

    let parser = 'a'.count(3);
    let result: ParseResult<&str, Vec<char>> = parse("aaab", parser);
    let success = result.unwrap();
    assert_eq!(success.output.len(), 3);
    assert_eq!(success.input, "b");

    let parser = 'a'.count(3);
    let err: ParseResult<&str, Vec<char>> = parse("aa", parser);
    let err = err.unwrap_err();
    assert_eq!(err.message.as_deref(), Some("Expected to match 3 times but got 2."));

    let parser = 'a'.up_to(2);
    let result: ParseResult<&str, Vec<char>> = parse("aaab", parser);
    let success = result.unwrap();
    assert_eq!(success.output.len(), 2);
    assert_eq!(success.input, "ab");

    let parser = 'a'.at_least(2);
    let result: ParseResult<&str, Vec<char>> = parse("aaab", parser);
    let success = result.unwrap();
    assert_eq!(success.output.len(), 3);
    assert_eq!(success.input, "b");
}

#[test]
fn test_separated_and_delimited() {
    let parser = 'a'.many_sep_by(',');
    let result: ParseResult<&str, Vec<char>> = parse("", parser);
    assert!(result.is_ok());
    assert!(result.unwrap().output.is_empty());

    let parser = 'a'.many1_sep_by(',');
    let result: ParseResult<&str, Vec<char>> = parse("a,a", parser);
    let success = result.unwrap();
    assert_eq!(success.output, vec!['a', 'a']);
    assert_eq!(success.input, "");

    let parser = 'a'.delimited_by('[', ']');
    let result: ParseResult<&str, char> = parse("[a", parser);
    let err = result.unwrap_err();
    assert!(err.irrefutable);
}

#[test]
fn test_err_message_overwrite() {
    let parser = fail(NoMatch, Some("old"), None).err_message("new", false);
    let result: ParseResult<&str, ()> = parse("", parser);
    let err = result.unwrap_err();
    assert_eq!(err.message.as_deref(), Some("old"));

    let parser = fail(NoMatch, Some("old"), None).err_message("new", true);
    let result: ParseResult<&str, ()> = parse("", parser);
    let err = result.unwrap_err();
    assert_eq!(err.message.as_deref(), Some("new"));
}

#[test]
fn test_whitespace_helpers() {
    let parser = 'a'.whitespaced(false).end();
    let result: ParseResult<&str, char> = parse(" a ", parser);
    let success = result.unwrap();
    assert_eq!(success.output, 'a');
    assert_eq!(success.input, "");
}
