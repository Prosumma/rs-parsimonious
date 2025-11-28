/// Moves through a `&str` in a char-aware fashion.
pub fn next_char_slice(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}
