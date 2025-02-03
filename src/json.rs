use crate::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum JSON {
  String(String),
  Number(String),
  Bool(bool),
  Array(Box<Vec<JSON>>),
  Object(Box<HashMap<String, JSON>>),
  Null
}

impl JSON {
  pub fn empty_array() -> JSON {
    JSON::Array(Box::new(Vec::new()))
  }

  pub fn empty_object() -> JSON {
    JSON::Object(Box::new(HashMap::new()))
  }
}

impl From<&str> for JSON {
  fn from(value: &str) -> JSON {
      JSON::String(value.into())
  }
}

impl From<bool> for JSON {
  fn from(value: bool) -> JSON {
    JSON::Bool(value)
  }
}

#[macro_export]
macro_rules! jarray {
  ($($value:expr),*) => {{
    #[allow(unused_mut)]
    let mut values: Vec<JSON> = Vec::new();
    $(
      values.push($value.into());
    )*
    JSON::Array(Box::new(values))
  }}
}

#[macro_export]
macro_rules! jobject {
  ($($key:expr => $value:expr),*) => {{
    #[allow(unused_mut)]
    let mut values: HashMap<String, JSON> = HashMap::new(); 
    $(
      values.insert($key.to_string(), $value.into());
    )*
    JSON::Object(Box::new(values))
  }}
}

fn unquoted_string(context: &mut ParseContext<char>) -> Result<String, ParseError> {
  let mut s = String::new();
  let mut escaping = false;
  loop {
    if let Some(&c) = context.current() {
      if escaping {
        match c {
          '"' => s.push(c),
          '\\' => s.push(c),
          // TODO: The rest of the escape sequences recognized in JSON.
          _ => return context.no_match() 
        }
        escaping = false
      } else if c == '\\' {
        escaping = true
      } else if c == '"' {
        break
      } else {
        s.push(c)
      }
      context.position += 1;
    } else {
      return Err(ParseError::EndOfInput)
    }
  }
  Ok(s)
}

fn quoted_string(context: &mut ParseContext<char>) -> Result<String, ParseError> {
  unquoted_string.double_quoted().parse(context)
}

fn jstring(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  quoted_string.map(JSON::String).parse(context)
}

fn jbool(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  let jtrue = string("true").map(|_| true.into());
  let jfalse = string("false").map(|_| false.into()); 
  or(jtrue, jfalse).parse(context)
}

fn jarray(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  json.many_sep(eq(',')).whitespaced().bracketed().map(|output| JSON::Array(Box::new(output))).parse(context)
}

fn jassign(context: &mut ParseContext<char>) -> Result<(String, JSON), ParseError> {
  let key = quoted_string.whitespaced().parse(context)?;
  eq(':').parse(context)?;
  let value = json.parse(context)?;
  Ok((key, value))
}

fn jobject(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  let assignments = jassign.many_sep(eq(',')).whitespaced().braced().parse(context)?;
  let mut object = HashMap::new();
  for (key, value) in assignments {
    object.insert(key, value);
  }
  Ok(JSON::Object(Box::new(object)))
}

fn jnull(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  string("null").map(|_| JSON::Null).parse(context)
}

pub fn json(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  or!(jstring, jbool, jnull, jarray, jobject).whitespaced().parse(context)
}

#[cfg(test)]
mod tests {
  use crate::*;
  use super::*;

  #[test]
  fn json_parses() {
    let s = r#"{"foo": "bar", "x": ["a", {}, true, null]}"#;
    let j = parse_str(s, json.end());
    let expected = jobject!{"foo" => "bar", "x" => jarray!["a", jobject!{}, true, JSON::Null]};
    assert_eq!(j, Ok(expected)) 
  }
}
