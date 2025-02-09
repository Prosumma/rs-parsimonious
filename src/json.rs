use crate::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum JSON {
  String(String),
  Array(Box<Vec<JSON>>),
  Object(Box<HashMap<String, JSON>>),
  Null
}

fn unquoted_string(context: &mut ParseContext<char>) -> Result<String, ParseError> {
  let mut output = String::new();
  let mut escaping = false;
  while let Some(&c) = context.current() {
    if escaping {
      match c {
        '\\' => output.push(c),
        '"'  => output.push(c),
        'n'  => output.push('\n'),
        't'  => output.push('\t'),
        _    => return Err(context.err_no_match())
      }
      escaping = false
    } else if c == '"' {
      break
    } else if c == '\\' {
      escaping = true;
    } else {
      output.push(c);
    }
    context.position += 1;
  }
  // When parsing a string, we should never
  // be at_end, because an unquoted string
  // by itself never occurs in JSON. It is
  // always surrounded by quotes.
  if context.at_end() {
    Err(ParseError::End)
  } else {
    Ok(output)
  }
}

fn quoted_string(context: &mut ParseContext<char>) -> Result<String, ParseError> {
  unquoted_string.double_quoted().parse(context)
}

fn jstring(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  quoted_string.map(JSON::String).parse(context)
}

fn jarray(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  json
    .many_sep(eq(',').whitespaced())
    .whitespaced()
    .bracketed()
    .map(|js| JSON::Array(Box::new(js)))
    .parse(context)
}

fn jassign(context: &mut ParseContext<char>) -> Result<(String, JSON), ParseError> {
  let key = quoted_string.parse(context)?;
  eq(':').whitespaced().parse(context)?;
  let j = json.parse(context)?;
  Ok((key, j))
}

fn jobject(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  let pairs = jassign
    .many_sep(eq(',').whitespaced())
    .whitespaced()
    .braced()
    .parse(context)?;
  let mut object = HashMap::new(); 
  for (key, value) in pairs {
    object.insert(key, value);
  }
  Ok(JSON::Object(Box::new(object)))
}

pub fn jnull(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  string("null").to_string().map(|_| JSON::Null).parse(context)
}

pub fn json(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  or!(jstring, jarray, jobject, jnull).whitespaced().parse(context)
}

impl From<&str> for JSON {
  fn from(value: &str) -> Self {
      JSON::String(value.into())
  }
}

#[macro_export]
macro_rules! ja {
  ($($json:expr),*) => {{
    #[allow(unused_mut)]
    let mut array = Vec::new();
    $(
      array.push($json.into());
    )*
    JSON::Array(Box::new(array))
  }}
}

#[macro_export]
macro_rules! jo {
  ($($key:expr => $json:expr),*) => {{
    #[allow(unused_mut)]
    let mut object = HashMap::new();
    $(
      object.insert($key.into(), $json.into());
    )*
    JSON::Object(Box::new(object))
  }}
}

#[cfg(test)]
mod tests {
  use crate::*;
  use super::*;

  #[test]
  fn parse_jstring() {
    let s = r#""Odysseus""#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(JSON::String("Odysseus".to_owned())))
  }

  #[test]
  fn parse_jstring_with_escape() {
    let s = "\"\\n\\t\"";
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(JSON::String("\n\t".into())))
  }

  #[test]
  fn parse_jarray_empty() {
    let s = r#"[]"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(JSON::Array(Box::new(Vec::new()))))
  }

  #[test]
  fn parse_jarray() {
    let s = r#"["Odysseus", "Poseidon"]"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(ja!["Odysseus", "Poseidon"]))
  }

  #[test]
  fn parse_jobject_empty() {
    let s = r#"{}"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(jo!{}))
  }

  #[test]
  fn parse_jobject() {
    let s = r#"{"s": "string\n", "a": []}"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(jo!{"s" => "string\n", "a" => ja![]}))
  }

  #[test]
  fn parse_null() {
    let s = "null";
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(JSON::Null))
  }
}
