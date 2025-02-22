use crate::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum JSON {
  String(String),
  // We're interested in parsing JSON
  // to test our combinators. We're not
  // interested in things like JS-style
  // number comparisons, etc.
  //
  // I'm not writing a JSON framework.
  // This is a toy.
  Number(String),
  Array(Vec<JSON>),
  Object(HashMap<String, JSON>),
  Bool(bool),
  Null
}

fn unquoted_string(context: &mut ParseContext<char>) -> Result<String, ParseError> {
  let mut output = String::new();
  let mut escaping = false;
  let mut unicode: Option<String> = None;
  while let Some(&c) = context.current() {
    if let Some(ref mut u) = unicode {
      if c.is_ascii_hexdigit() {
        u.push(c);
        if u.len() == 4 {
          // We know this is safe because of is_ascii_hexdigit.
          let code_point = u32::from_str_radix(&u, 16).unwrap();
          if let Some(unichar) = char::from_u32(code_point) {
            output.push(unichar);
            unicode = None;
          } else {
            return context.throw_partial_match() 
          }
        }
      } else {
        return context.throw_partial_match() 
      }
    } else if escaping {
      match c {
        '\\' => output.push(c),
        '"'  => output.push(c),
        '/'  => output.push(c),
        'n'  => output.push('\n'),
        't'  => output.push('\t'),
        'r'  => output.push('\r'),
        'b'  => output.push('\x08'),
        'f'  => output.push('\x0C'),
        'u'  => unicode = Some(String::new()),
         _   => return context.throw_partial_match() 
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
    context.throw_partial_match()
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

fn non_zero_digit(context: &mut ParseContext<char>) -> Result<char, ParseError> {
  one_of_str("123456789", false).parse(context)
}

fn integer(context: &mut ParseContext<char>) -> Result<Vec<char>, ParseError> {
  let non_zero = chains(non_zero_digit.many1(), ascii_digit.many());
  or(eq('0').to_vec(), non_zero).parse(context)
}

fn decimal(context: &mut ParseContext<char>) -> Result<Vec<char>, ParseError> {
  let fractional = chains(eq('.').to_vec(), ascii_digit.many1()).maybe();
  chains(integer, fractional).parse(context)
}

fn exponent(context: &mut ParseContext<char>) -> Result<Vec<char>, ParseError> {
  let e = eqchar('e', false).to_vec();
  let sign = one_of_str("+-", true).optional();
  chains!(e, sign, integer).parse(context)
}

fn jnumber(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  let sign = eq('-').optional();
  chains!(sign, decimal, exponent.maybe()).to_string().map(JSON::Number).parse(context)
}

fn jarray(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  json
    .many_sep(eq(',').whitespaced())
    .whitespaced()
    .bracketed()
    .partial(1)
    .map(JSON::Array)
    .parse(context)
}

fn jassign(context: &mut ParseContext<char>) -> Result<(String, JSON), ParseError> {
  let key = quoted_string.parse(context)?;
  eq(':').whitespaced().parse(context)?;
  let j = json.parse(context)?;
  Ok((key, j))
}

pub fn jobject(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  let pairs = jassign
    .many_sep(eq(',').whitespaced())
    .whitespaced()
    .braced()
    .partial(1)
    .parse(context)?;
  let mut object = HashMap::new(); 
  for (key, value) in pairs {
    object.insert(key, value);
  }
  Ok(JSON::Object(object))
}

fn jtrue(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  string("true").partial(1).map(|_| JSON::Bool(true)).parse(context)
}

fn jfalse(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  string("false").partial(1).map(|_| JSON::Bool(false)).parse(context)
}

fn jbool(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  or(jtrue, jfalse).parse(context)
}

fn jnull(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  string("null").partial(1).to_string().map(|_| JSON::Null).parse(context)
}

pub fn json(context: &mut ParseContext<char>) -> Result<JSON, ParseError> {
  or!(jstring, jnumber, jbool, jarray, jobject, jnull).whitespaced().parse(context)
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
    JSON::Array(array)
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
    JSON::Object(object)
  }}
}

#[macro_export]
macro_rules! jn {
  ($s:expr) => { JSON::Number($s.into()) }
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
    assert_eq!(r, Ok(JSON::Array(Vec::new())))
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
    let s = r#"{"s": "string\n", "a": [], "n": null, "i": 38.3E2, "z": 0e0}"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(jo!{"s" => "string\n", "a" => ja![], "n" => JSON::Null, "i" => jn!("38.3E2"), "z" => jn!("0e0")}))
  }

  #[test]
  fn parse_integer() {
    let s = "-947362";
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(jn!("-947362")));
  }

  #[test]
  fn parse_float() {
    let s = "947362.32";
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(jn!("947362.32")));
  }

  #[test]
  fn parse_exponent() {
    let s = "947362.32e-4";
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(jn!("947362.32e-4")));
  }

  #[test]
  fn parse_null() {
    let s = "null";
    let r = parse_str(s, json.end());
    assert_eq!(r, Ok(JSON::Null))
  }

  #[test]
  fn parse_partial_null() {
    let s = "numm";
    let r = parse_str(s, json.end());
    assert_eq!(r, Err(PartialMatch(2)))
  }

  #[test]
  fn parse_partial_string() {
    let s = r#""foo"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Err(PartialMatch(4)))
  }

  #[test]
  fn parse_partial_object() {
    let s = r#"{"number": -734a}"#;
    let r = parse_str(s, json.end());
    assert_eq!(r, Err(PartialMatch(15)))
  }
}
