use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum Comparison {
    Equal,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

pub fn is_comp(s: &str) -> bool {
    match s {
        ">" | "<" | "<=" | ">=" | "=" => true,
        _ => false,
    }
}

impl fmt::Display for Comparison {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Comparison::*;
        write!(
            f,
            "{}",
            match self {
                Equal => "eq",
                LessThan => "lt",
                LessThanEqual => "lte",
                GreaterThan => "gt",
                GreaterThanEqual => "gte",
            }
        )
    }
}
