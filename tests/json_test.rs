#![allow(unused_imports)]

use parsimonious::*;
use parsimonious::json::*;
use std::fs;
use std::path::Path;

#[test]
fn json_sanity_check() {
  let path = Path::new("tests/JSON.json");
  let contents = fs::read_to_string(path).expect("JSON file is missing.");
  let output = parse_str(contents, json.end());
  println!("{:?}", output);
  assert!(output.is_ok())
}