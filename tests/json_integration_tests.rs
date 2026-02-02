use parsimonious::json::*;
use parsimonious::*;
use std::{fs, path::PathBuf};

fn load_test_json() -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test.json");
    fs::read_to_string(path).unwrap()
}

#[test]
fn test_parse_test_json() {
    let s = load_test_json();
    let result: ParseResult<&str, JSON> = parse(&s, json);
    let j = result.unwrap().output;

    assert_eq!(j["numbers"]["integer_positive"], JSON::Number("42".to_owned()));
    assert_eq!(j["numbers"]["integer_negative"], JSON::Number("-100".to_owned()));
    assert_eq!(j["numbers"]["zero"], JSON::Number("0".to_owned()));
    assert_eq!(j["numbers"]["float_simple"], JSON::Number("3.14159".to_owned()));
    assert_eq!(
        j["numbers"]["scientific_notation_negative_exponent"],
        JSON::Number("9.87e-3".to_owned())
    );
    assert_eq!(j["booleans_and_null"]["boolean_true"], JSON::Bool(true));
    assert_eq!(j["booleans_and_null"]["boolean_false"], JSON::Bool(false));
    assert_eq!(j["booleans_and_null"]["null_value"], JSON::Null);

    match &j["arrays"]["array_of_numbers"] {
        JSON::Array(items) => assert_eq!(items.len(), 5),
        other => panic!("expected array, got {:?}", other),
    }

    match &j["objects"]["object_with_array_of_objects"]["users"] {
        JSON::Array(users) => {
            assert_eq!(users.len(), 2);
            match &users[0] {
                JSON::Object(first) => {
                    assert_eq!(first["name"], JSON::String("Alice".to_owned()));
                    assert_eq!(first["active"], JSON::Bool(true));
                }
                other => panic!("expected object, got {:?}", other),
            }
        }
        other => panic!("expected array, got {:?}", other),
    }
}

#[test]
fn test_json_string_and_escapes() {
    let input = "\"line\\nend\"";
    let result: ParseResult<&str, JSON> = parse(input, json_string);
    let success = result.unwrap();
    assert_eq!(success.output, JSON::String("line\nend".to_owned()));
    assert_eq!(success.input, "");

    let input = "\"\\u00A9\"";
    let result: ParseResult<&str, JSON> = parse(input, json_string);
    let success = result.unwrap();
    assert_eq!(success.output, JSON::String("\u{00A9}".to_owned()));
}

#[test]
fn test_json_string_invalid_escape() {
    let input = "\"\\q\"";
    let result: ParseResult<&str, String> = quoted_string(input);
    let err = result.unwrap_err();
    assert_eq!(err.message.as_deref(), Some("Invalid escape character: q"));
}

#[test]
fn test_json_number_requires_terminator() {
    let input = "12x";
    let result: ParseResult<&str, JSON> = parse(input, json_number);
    let err = result.unwrap_err();
    assert_eq!(err.reason, NoMatch);
    assert!(err.irrefutable);
}

#[test]
fn test_json_array_and_object_errors() {
    let input = "[]";
    let result: ParseResult<&str, JSON> = parse(input, json_array);
    let success = result.unwrap();
    assert_eq!(success.output, JSON::Array(Vec::new()));

    let input = "{\"a\": 1";
    let result: ParseResult<&str, JSON> = parse(input, json_object);
    let err = result.unwrap_err();
    assert!(err.irrefutable);
}

#[test]
fn test_json_try_from_string_and_index() {
    let json = JSON::String("value".to_owned());
    let s: String = json.clone().try_into().unwrap();
    assert_eq!(s, "value");

    let json = JSON::Bool(true);
    let err: Result<String, _> = json.try_into();
    assert!(err.is_err());

    let json = JSON::Bool(true);
    assert_eq!(json["missing"], JSON::Null);
}

#[test]
fn test_json_invalid_numbers() {
    let invalid_inputs = ["+1", "1e", "1e+", "1e-", "1E+", "01", "-01"];
    for input in invalid_inputs {
        let result: ParseResult<&str, JSON> = parse(input, json_number);
        assert!(result.is_err(), "expected error for {}", input);
    }

    let result: ParseResult<&str, JSON> = parse("+1", json_number);
    let err = result.unwrap_err();
    assert_eq!(err.reason, NoMatch);
}

#[test]
fn test_json_trailing_commas_and_missing_values() {
    let result: ParseResult<&str, JSON> = parse("[1,]", json_array);
    let err = result.unwrap_err();
    assert!(err.irrefutable);

    let result: ParseResult<&str, JSON> = parse("{\"a\":}", json_object);
    let err = result.unwrap_err();
    assert!(err.irrefutable);
}

#[test]
fn test_json_unterminated_strings_and_invalid_unicode() {
    let result: ParseResult<&str, JSON> = parse("\"unterminated", json_string);
    let err = result.unwrap_err();
    assert_eq!(err.reason, EOF);

    let result: ParseResult<&str, JSON> = parse("\"\\uD83D\"", json_string);
    let err = result.unwrap_err();
    assert_eq!(err.reason, NoMatch);

    let result: ParseResult<&str, JSON> = parse("\"\\u12G4\"", json_string);
    let err = result.unwrap_err();
    assert_eq!(err.reason, NoMatch);
}

#[test]
fn test_json_rejects_control_chars_in_string() {
    let input = "\"a\x01b\"";
    let result: ParseResult<&str, String> = quoted_string(input);
    let err = result.unwrap_err();
    assert_eq!(
        err.message.as_deref(),
        Some("Invalid control character in string.")
    );
    assert_eq!(err.reason, NoMatch);
}
