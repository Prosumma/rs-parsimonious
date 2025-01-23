use crate::{ParseOutput,Parser,ParseError,ParseResult,eq,many_sep,whitespace,first,void,or};
use std::{collections::HashMap, ffi::FromBytesUntilNulError};

impl From<&str> for JSON {
  fn from(value: &str) -> Self {
      JSON::String(value.to_string())
  }
}

impl<J: Into<JSON> + Clone> From<&[J]> for JSON {
  fn from(value: &[J]) -> Self {
    let mut output = Vec::new();
    for j in value.iter() {
      output.push(j.clone().into());
    }
    JSON::Array(Box::new(output))
  }
}

macro_rules! jarray {
  ($($elem:expr),*) => {{
    let mut elems: Vec<JSON> = Vec::new();
    $(
      elems.push($elem.into());
    )*
    JSON::Array(Box::new(elems))
  }}
}

macro_rules! jobject {
  ($($key:expr => $value:expr),*) => {{
    let mut map: HashMap<String, JSON> = HashMap::new();
    $(
      map.insert($key.to_string(), $value.into());
    )*
    JSON::Object(Box::new(map))
  }}
}

#[derive(Debug, PartialEq, Clone)]
pub enum JSON {
  String(String),
  Number(String),
  Array(Box<Vec<JSON>>),
  Object(Box<HashMap<String, JSON>>)
}

fn escaped_string(input: &[char], position: usize) -> ParseResult<String> {
    let mut characters = Vec::new();
    let mut position = position;
    let mut escaping = false;
    while let Some(&c) = input.get(position) {
      if escaping {
        match c {
          '\\' => characters.push(c),
          '"' => characters.push(c),
          // TODO: Add the others
          _ => return Err(ParseError::NoMatch(position))
        }
        escaping = false;
      } else if c == '\\' {
        escaping = true;
        continue
      } else if c == '"' {
        let s: String = characters.into_iter().collect();
        return Ok(ParseOutput::new(s, position))
      } else {
        characters.push(c);        
      }
      position += 1
    }
    Err(ParseError::EndOfInput)
}

pub fn quoted_string(input: &[char], position: usize) -> ParseResult<String> {
  escaped_string.surrounded_by('"').parse(input, position)
}

pub fn jstring(input: &[char], position: usize) -> ParseResult<JSON> {
  quoted_string.map(JSON::String).parse(input, position)
}

fn json(input: &[char], position: usize) -> ParseResult<JSON> {
  or!(jstring, jarray, jobject).surrounded_by(whitespace.many()).parse(input, position)
}

fn array_body() -> impl Parser<char, Vec<JSON>> + Clone {
  many_sep(json, ','.surrounded_by(whitespace.many()))
}

pub fn jarray(input: &[char], position: usize) -> ParseResult<JSON> {
  let p = array_body()
    .preceded_by(void!('[', whitespace.many()))
    .followed_by(void!(whitespace.many(), ']'));
  let result = p.parse(input, position)?;
  Ok(ParseOutput::new(JSON::Array(Box::new(result.output)), result.position))
}

pub fn jassignment(input: &[char], position: usize) -> ParseResult<(String, JSON)> {
  let key_parser = quoted_string.followed_by(void!(whitespace.many(), ':', whitespace.many()));
  let key_output = key_parser.parse(input, position)?;
  let json_output = json(input, key_output.position)?;
  Ok(ParseOutput::new((key_output.output, json_output.output), json_output.position))
}

pub fn jobject(input: &[char], position: usize) -> ParseResult<JSON> {
  let p = many_sep(jassignment, ','.surrounded_by(whitespace.many()))
    .preceded_by('{'.followed_by(whitespace.many()))
    .followed_by('}'.preceded_by(whitespace.many()));
  let output = p.parse(input, position)?;
  let mut result = HashMap::new();
  for (key, j) in output.output.into_iter() {
    result.insert(key, j);
  }
  Ok(ParseOutput::new(JSON::Object(Box::new(result)), output.position))
}

#[cfg(test)]
mod tests {
  use crate::parse_str;
  use super::*;

  #[test]
  fn jquote_succeeds_with_simple_string() {
    let s = "\"foo\"";
    let r = parse_str(s, jstring.end_of_input());
    assert_eq!(r, Ok(ParseOutput::new(JSON::String("foo".to_string()), 5)))
  }
  
  #[test]
  fn jarray_works() {
    let s = "[\"ok\",[\"maybe\", \"never\"]  ]";
    let r = parse_str(s, jarray);
    let expected = jarray!("ok", jarray!("maybe", "never"));
    assert_eq!(r, Ok(ParseOutput::new(expected, 27)))
  }
  
  #[test]
  fn jobject_works() {
    let s = "{\"x\": \"bob\", \"y\": [\"fred\", \"joe\"]  }";
    let r = parse_str(s, json.surrounded_by(whitespace.many()));
    let expected = jobject!("x" => "bob", "y" => jarray!("fred", "joe"));
    assert_eq!(r, Ok(ParseOutput::new(expected, 36)));
    println!("{:?}", r);
  }
}