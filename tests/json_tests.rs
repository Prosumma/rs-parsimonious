#![allow(unused_imports)]

use parsimonious::*;
use parsimonious::json::*;
use std::collections::HashMap;

#[test]
fn test_jstring() {
  let s = "\"watusi\"";
  let p = json.end();
  let result = parse_str(s, p);
  assert_eq!(result, ok!("watusi".into(), 8))
}

#[test]
fn test_jarray() {
  let s = "[\"watusi\", \"foo\" , [  \"crazy\"] ]";
  let p = json.end(); 
  let result = parse_str(s, p);
  assert_eq!(result, ok!(jarray!("watusi", "foo", jarray!("crazy")), s.len()))
}

#[test]
fn test_jobject() {
  let s = "  { \"array\": [\"something\" ,\"else\" ] } ";
  let p = json.end();
  let result = parse_str(s, p);
  assert_eq!(result, ok!(jobject!("array" => jarray!("something", "else")), s.len()))
}

#[test]
fn test_jnull() {
  let s = " [null, \"null\\\"\"] ";
  let p = json.surrounded_by(whitespace.many()).end();
  let result = parse_str(s,p);
  assert_eq!(result, ok!(vec![None, Some("null\"")].into(), s.len()))
}
