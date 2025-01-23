#[allow(unused_imports)]
use crate::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum JSON {
  String(String),
  Array(Vec<Box<JSON>>),
  Object(HashMap<String, Box<JSON>>),
  Null
}

impl From<&str> for JSON {
  fn from(value: &str) -> Self {
      JSON::String(value.to_string())
  }
}

impl<J: Into<JSON>> From<Option<J>> for JSON {
  fn from(value: Option<J>) -> Self {
    value.map_or_else(|| JSON::Null, |value| value.into())
  }
}

impl<J: Into<JSON>> From<Vec<J>> for JSON {
  fn from(values: Vec<J>) -> Self {
    let mut boxes: Vec<Box<JSON>> = Vec::new();
    for value in values {
      boxes.push(Box::new(value.into()));
    }
    JSON::Array(boxes)
  }
}

#[macro_export]
macro_rules! jarray {
  ($($value:expr),*) => {{
    let mut boxes: Vec<Box<JSON>> = Vec::new();
    $(
      boxes.push(Box::new($value.into()));
    )*
    JSON::Array(boxes)
  }}
}

impl<J: Into<JSON>> From<HashMap<String, J>> for JSON {
  fn from(values: HashMap<String, J>) -> Self {
      let mut keyed_boxes: HashMap<String, Box<JSON>> = HashMap::new();
      for (key, value) in values {
        keyed_boxes.insert(key, Box::new(value.into()));
      }
      JSON::Object(keyed_boxes)
  }
}

#[macro_export]
macro_rules! jobject {
  ($($key:expr => $value:expr),*) => {{
    let mut keyed_boxes: HashMap<String, Box<JSON>> = HashMap::new();
    $(
      keyed_boxes.insert($key.to_string(), Box::new($value.into()));
    )*
    JSON::Object(keyed_boxes)
  }}
}

fn json_string_body(input: &[char], position: usize) -> ParseResult<String> {
  let mut chars: Vec<char> = Vec::new();
  let mut position = position;
  let mut escaping = false;
  loop {
    if let Some(&c) = input.get(position) {
      if escaping {
        match c {
          '"' | '\\' => chars.push(c),
          // TODO: Add other possibilities
          _ => return Err(ParseError::NoMatch(position))
        }
        escaping = false
      } else if c == '\\' {
        escaping = true
      } else if c == '"' {
        break 
      } else {
        chars.push(c);
      }
      position += 1;
    } else {
      return Err(ParseError::EndOfInput)
    }
  }
  ok!(chars.into_iter().collect(), position)
}

fn quoted_string(input: &[char], position: usize) -> ParseResult<String> {
  json_string_body.preceded_by(eq('"')).followed_by(eq('"')).parse(input, position)
}

pub fn jstring(input: &[char], position: usize) -> ParseResult<JSON> {
  quoted_string.map(JSON::String).parse(input, position)
}

pub fn json(input: &[char], position: usize) -> ParseResult<JSON> {
  or!(jstring, jobject, jarray, jnull).surrounded_by(whitespace.many()).parse(input, position)
}

fn comma(input: &[char], position: usize) -> ParseResult<char> {
  eq(',').surrounded_by(whitespace.many()).parse(input, position)
}

pub fn jarray(input: &[char], position: usize) -> ParseResult<JSON> {
  let elems = json.many_sep(comma).surrounded_by(whitespace.many());
  let parser = elems.preceded_by(eq('[')).followed_by(eq(']'));
  parser.map(|elems| elems.into()).parse(input, position)
}

fn jassignment(input: &[char], position: usize) -> ParseResult<(String, JSON)> {
  let colon = eq(':').surrounded_by(whitespace.many());
  let key = quoted_string.followed_by(colon);
  let key_output = key.parse(input, position)?;
  let json_output = json.parse(input, key_output.position)?;
  ok!((key_output.output, json_output.output), json_output.position)
}

pub fn jobject(input: &[char], position: usize) -> ParseResult<JSON> {
  let parser = jassignment
    .many_sep(comma)
    .surrounded_by(whitespace.many())
    .preceded_by(eq('{'))
    .followed_by(eq('}'));
  let assignment_output = parser.parse(input, position)?;
  let mut hashmap: HashMap<String, Box<JSON>> = HashMap::new();
  for (key, value) in assignment_output.output {
    hashmap.insert(key, Box::new(value)); 
  }
  ok!(JSON::Object(hashmap), assignment_output.position)
}

pub fn jnull(input: &[char], position: usize) -> ParseResult<JSON> {
  string("null").map(|_| JSON::Null).parse(input, position)
}
