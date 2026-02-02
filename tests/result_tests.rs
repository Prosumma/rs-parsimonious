use parsimonious::*;

#[test]
fn test_parse_output_map() {
    let output = ParseOutput::new("rest", 2).map(|v| v + 1);
    assert_eq!(output.output, 3);
    assert_eq!(output.input, "rest");
}

#[test]
fn test_parse_error_message_meta() {
    let err: ParseError<&str, u32> = ParseError::new("input", NoMatch)
        .message("bad input")
        .meta(7);
    assert_eq!(err.reason, NoMatch);
    assert_eq!(err.message.as_deref(), Some("bad input"));
    assert_eq!(err.meta, Some(7));
}

#[test]
fn test_ext_parse_result_helpers() {
    let result: ParseResult<&str, u8> = err("input", EOF);
    let err_with_message = result.err_message("stop").unwrap_err();
    assert_eq!(err_with_message.message.as_deref(), Some("stop"));

    let result: ParseResult<&str, u8, u16> = err("input", NoMatch);
    let err_with_meta = result.err_meta(42).unwrap_err();
    assert_eq!(err_with_meta.meta, Some(42));
}
