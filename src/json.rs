use crate::combinators::*;
use crate::ext::*;
use crate::or;
use crate::parser::*;
use crate::result::*;
use std::collections::HashMap;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum JSON {
    Bool(bool),
    String(String),
    Number(String),
    Object(HashMap<String, JSON>),
    Array(Vec<JSON>),
    Null,
}

fn unquoted_string<'a, E>(input: &'a str) -> ParseResult<&'a str, String, E> {
    let mut output = String::new();
    let mut escaping = false;
    let mut unicode: Option<String> = None;
    let mut chars = input.char_indices();
    let mut last_idx: usize;
    loop {
        if let Some((ix, c)) = chars.next() {
            last_idx = ix;
            if let Some(ref mut u) = unicode {
                if c.is_ascii_hexdigit() {
                    u.push(c);
                    if u.len() == 4 {
                        let code_point = u32::from_str_radix(&u, 16).unwrap();
                        if let Some(unichar) = char::from_u32(code_point) {
                            output.push(unichar);
                            unicode = None;
                        } else {
                            return err(chars.as_str(), NoMatch);
                        }
                    }
                } else {
                    return err(chars.as_str(), NoMatch);
                }
            } else if escaping {
                match c {
                    '\\' => output.push(c),
                    '"' => output.push(c),
                    '/' => output.push('/'),
                    'n' => output.push('\n'),
                    't' => output.push('\t'),
                    'r' => output.push('\r'),
                    'b' => output.push('\x08'),
                    'f' => output.push('\x0C'),
                    'u' => unicode = Some(String::new()),
                    i => {
                        return err(chars.as_str(), NoMatch)
                            .err_message(format!("Invalid escape character: {}", i))
                    }
                }
                escaping = false;
            } else if c == '"' {
                break;
            } else if c == '\\' {
                escaping = true;
            } else {
                output.push(c);
            }
        } else {
            return err(chars.as_str(), EOF);
        }
    }
    if input.len() == 0 {
        err(chars.as_str(), EOF)
    } else {
        ok(&input[last_idx..], output)
    }
}

pub fn quoted_string<'a, E>(input: &'a str) -> ParseResult<&'a str, String, E> {
    parse(input, unquoted_string.double_quoted())
}

pub fn json_string<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    parse(input, quoted_string.map(JSON::String))
}

pub fn json_number<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    let digit = one_of_str("0123456789", false);
    parse(input, digit.many1().to_string().map(JSON::Number))
}

pub fn json_array<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    parse(
        input,
        json.many_sep_by(','.whitespaced(false))
            .bracketed()
            .map(JSON::Array),
    )
}

pub fn json_assignment<'a, E>(input: &'a str) -> ParseResult<&'a str, (String, JSON), E> {
    parse(
        input,
        tuple(quoted_string, json.preceded_by(':'.whitespaced(false))),
    )
}

pub fn json_object<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    parse(
        input,
        json_assignment
            .whitespaced(false)
            .many_sep_by(','.whitespaced(false))
            .braced()
            .map(|asses| JSON::Object(asses.into_iter().collect())),
    )
}

pub fn json_bool<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    let t = string("true", false).map(|_| JSON::Bool(true));
    let f = string("false", false).map(|_| JSON::Bool(false));
    parse(input, or(t, f))
}

pub fn json_null<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    string("null", false).map(|_| JSON::Null).parse(input)
}

pub fn json<'a, E>(input: &'a str) -> ParseResult<&'a str, JSON, E> {
    let options = or!(
        json_string,
        json_number,
        json_object,
        json_array,
        json_bool,
        json_null
    );
    parse(input, options.whitespaced(false))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quoted_string() {
        let result: ParseResult<&str, String> = quoted_string("\"\\nbob\"");
        let success = result.unwrap();
        assert_eq!(success.output, "\nbob")
    }

    #[test]
    fn test_json_object() {
        let input = "{\"key\": \"value\", \"koo\": [\"voolue\", null, 743, false]}";
        let result: ParseResult<&str, JSON> = parse(input, json_object);
        result.unwrap();
    }
}
