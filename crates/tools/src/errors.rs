use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct CodeErr(String);

impl Display for CodeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub trait ReportCodeErr {
    fn to_glob_err(&self, line: u64) -> CodeErr
    where
        Self: std::fmt::Display,
    {
        CodeErr(format!("Line: {}\n{}", line + 1, self))
    }
}
