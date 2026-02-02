use parsimonious::json::*;
use parsimonious::*;
use std::{fs, path::PathBuf};

#[test]
fn test_number() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("test.json");
    let s = fs::read_to_string(path).unwrap();
    let result: ParseResult<&str, JSON> = parse(&s, json);
    let j = result.unwrap().output;
    assert_eq!(
        j["numbers"]["integer_positive"],
        JSON::Number("42".to_owned())
    )
}
