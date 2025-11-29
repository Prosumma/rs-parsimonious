/// Moves through a `&str` in a char-aware fashion.
pub fn next_char_slice(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

pub(crate) trait ExtStr {
    fn char_offset_from<'a>(&'a self, other: &'a str) -> usize;
}

impl ExtStr for str {
    fn char_offset_from<'a>(&'a self, parent: &'a str) -> usize {
        let child_ptr = self.as_ptr();
        let parent_ptr = parent.as_ptr();
        let offset = unsafe { child_ptr.offset_from_unsigned(parent_ptr) };
        let substr = &parent[..offset];
        substr.chars().count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset() {
        let p = "abracadabra";
        let c = &p[4..];
        assert_eq!(c.char_offset_from(p), 4);
    }
}
