use colored::*;
use std::collections::{HashMap, VecDeque};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("{0}")]
    GlobLexerError(String),

    #[error("{} while tokenizing number -{0}-, found two '.' to construct a decimal.", "Error".bold().red())]
    DoubleDotNumber(String),

    #[error("{} while tokenizing code, unsupported character: -{0}-.", "Error".bold().red())]
    UnrecognizedToken(String),

    #[error("{} while tokenizing number, only digit and one '.' are supported to declare a number.", "Error".bold().red())]
    AlphaCharInNumberToken,
}

impl LexerError {
    pub fn context(self, line: u64) -> LexerError {
        Self::GlobLexerError(format!("Line: {}, {}", line, self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literal types
    Number,
    Identifier,

    // Operator
    Equals,
    BinaryOperator,
    CompoundAssign,
    SimpleArrow,

    // Grouping
    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]
    Comma,        // ,
    Colon,        // :
    Dot,          // .

    // Keywords
    Var,
    Const,
    Structure,
    New,
    Fn,
    Return,
    SelfKw,
    Test,
    For,
    In,

    // Types
    Any,
    Int,
    Real,
    Bool,

    // File
    EndLine,
    EOF,
}

// We use a struct instead of enum to be able to loop only on value without to have to
// extract it from enum each time with if let syntax
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: u64,
}

// Append a white space at the end of the code to allow the window to work
// correctly. Otherwise, last character is never in its own window.
fn append_space_and_eof(mut source_code: String) -> String {
    source_code.push('\n');
    source_code.push(' ');
    source_code
}

// Skip listed char
fn is_skippable(c: char) -> bool {
    matches!(c, ' ' | '\t')
}

#[derive(Default)]
pub struct Lexer<'a> {
    pub tokens: VecDeque<Token>,
    reserved_keywords: HashMap<&'a str, Token>,
}

impl<'a> Lexer<'a> {
    // Keywords generation
    fn generate_keywords(&mut self) {
        self.reserved_keywords.insert("var", Token::new_var());
        self.reserved_keywords.insert("const", Token::new_const());
        self.reserved_keywords.insert("struct", Token::new_struct());
        self.reserved_keywords.insert("new", Token::new_new());
        self.reserved_keywords.insert("fn", Token::new_fn());
        self.reserved_keywords.insert("return", Token::new_return());
        self.reserved_keywords.insert("self", Token::new_self());
        self.reserved_keywords.insert("test", Token::new_test());
        self.reserved_keywords.insert("any", Token::new_any());
        self.reserved_keywords.insert("int", Token::new_int());
        self.reserved_keywords.insert("real", Token::new_real());
        self.reserved_keywords.insert("bool", Token::new_bool());
        self.reserved_keywords.insert("for", Token::new_for());
        self.reserved_keywords.insert("in", Token::new_in());
    }

    pub fn tokenize(&mut self, source_code: String) -> Result<(), LexerError> {
        self.generate_keywords();

        let modified_code = append_space_and_eof(source_code);
        let chars = modified_code.chars().collect::<Vec<char>>();
        let mut window = chars.windows(2);

        // We keep the count of the current line
        let mut line: u64 = 0;

        self.tokens.clear();

        while let Some(c) = window.next() {
            let first_char = c[0];

            if is_skippable(first_char) {
                continue;
            }

            match first_char {
                // Single character tokens
                '+' | '-' | '*' | '%' => {
                    // We check for a compound assignment
                    match c[1] {
                        '=' => {
                            self.tokens.push_back(Token::new(
                                TokenKind::CompoundAssign,
                                first_char.to_string(),
                                line,
                            ));
                            // We skip the '=' token
                            window.next();
                        }
                        '>' => {
                            if first_char == '-' {
                                self.tokens.push_back(Token::new_simple_arrow(line));
                                window.next();
                            }
                        }
                        _ => self.tokens.push_back(Token::new(
                                TokenKind::BinaryOperator,
                                first_char.to_string(),
                                line,
                            )),
                    }
                }
                '/' => {
                    // We check if this is comment
                    if c[1] == '/' {
                        while let Some(s) = window.next() {
                            // We break the loop at the end of the line
                            if s[0] == '\n' || s[0] == '\r' {
                                break;
                            }
                        }
                    } else {
                        // We check for a compound assignment
                        match c[1] {
                            '=' => {
                                self.tokens.push_back(Token::new(
                                    TokenKind::CompoundAssign,
                                    first_char.to_string(),
                                    line,
                                ));
                                // We skip the '=' token
                                window.next();
                            }
                            _ => self.tokens.push_back(Token::new(
                                TokenKind::BinaryOperator,
                                first_char.to_string(),
                                line,
                            )),
                        }
                    }
                }
                '=' => self.tokens.push_back(Token::new_equal(line)),
                '(' => self.tokens.push_back(Token::new_open_paren(line)),
                ')' => self.tokens.push_back(Token::new_close_paren(line)),
                '{' => self.tokens.push_back(Token::new_open_brace(line)),
                '}' => self.tokens.push_back(Token::new_close_brace(line)),
                '[' => self.tokens.push_back(Token::new_open_bracket(line)),
                ']' => self.tokens.push_back(Token::new_close_bracket(line)),
                ',' => self.tokens.push_back(Token::new_comma(line)),
                ':' => self.tokens.push_back(Token::new_colon(line)),
                '.' => self.tokens.push_back(Token::new_dot(line)),
                // Linux new line
                '\n' => {
                    self.tokens.push_back(Token::new_end_line(line));
                    line += 1;
                }
                // Windows new line
                '\r' => {
                    if c[1] == '\n' {
                        self.tokens.push_back(Token::new_end_line(line));
                        line += 1;
                        window.next();
                    }
                }
                _ => {
                    // Multiple characters token
                    let second_char = c[1];

                    // Handles numbers
                    if first_char.is_numeric() {
                        let mut val = String::from(first_char);
                        let mut decimal = false;

                        // If next one is a dot or another number
                        if second_char.is_numeric() || second_char == '.' {
                            while let Some(r) = window.next() {
                                // If it is a number we take it
                                if r[0].is_numeric() {
                                    val.push(r[0]);
                                // If it is a dot we take it and mark this number as decimal
                                } else if r[0] == '.' {
                                    if decimal {
                                        return Err(
                                            LexerError::DoubleDotNumber(val.clone()).context(line)
                                        );
                                    } else {
                                        val.push(r[0]);
                                        decimal = true;
                                    }
                                }

                                // If next one is a dot and number already a decimal, error
                                if r[1] == '.' && decimal {
                                    return Err(
                                        LexerError::DoubleDotNumber(val.clone()).context(line)
                                    );
                                // If next char is a letter, error
                                } else if r[1].is_alphabetic() {
                                    return Err(LexerError::AlphaCharInNumberToken.context(line));
                                // Else, end of number
                                } else if !r[1].is_numeric() && r[1] != '.' {
                                    break;
                                }
                            }
                        // If next one is a letter, error
                        } else if second_char.is_alphabetic() {
                            return Err(LexerError::AlphaCharInNumberToken.context(line));
                        }

                        // We add it
                        self.tokens.push_back(Token::new_number(val, line));
                    } else if first_char.is_alphabetic() {
                        // First char must be a alpha digit
                        let mut val = String::from(first_char);

                        // Support for variable like init_pos and variable with number like vec2
                        if second_char.is_alphanumeric() || second_char == '_' {
                            while let Some(r) = window.next() {
                                val.push(r[0]);

                                if !r[1].is_alphanumeric() && r[1] != '_' {
                                    break;
                                }
                            }
                        }

                        // Check for reserved keywords
                        if let Some(tk) = self.reserved_keywords.get(val.as_str()) {
                            self.tokens.push_back(tk.clone().set_line(line));
                        } else {
                            self.tokens.push_back(Token::new_identifier(val, line));
                        }
                    } else {
                        return Err(LexerError::UnrecognizedToken(first_char.into()).context(line));
                    }
                }
            }
        }

        // We append manualy end of file
        self.tokens.push_back(Token::new_eof(line));
        Ok(())
    }
}

// ---------
//  Helpers
// ---------
impl Token {
    pub fn new(kind: TokenKind, value: String, line: u64) -> Self {
        Self { kind, value, line }
    }

    pub fn new_number(value: String, line: u64) -> Token {
        Token {
            kind: TokenKind::Number,
            value,
            line,
        }
    }
    pub fn new_identifier(value: String, line: u64) -> Token {
        Token {
            kind: TokenKind::Identifier,
            value,
            line,
        }
    }
    pub fn new_equal(line: u64) -> Token {
        Token {
            kind: TokenKind::Equals,
            value: '='.to_string(),
            line,
        }
    }
    pub fn new_plus(line: u64) -> Token {
        Token {
            kind: TokenKind::BinaryOperator,
            value: '+'.to_string(),
            line,
        }
    }
    pub fn new_minus(line: u64) -> Token {
        Token {
            kind: TokenKind::BinaryOperator,
            value: '-'.to_string(),
            line,
        }
    }
    pub fn new_multiplication(line: u64) -> Token {
        Token {
            kind: TokenKind::BinaryOperator,
            value: '*'.to_string(),
            line,
        }
    }
    pub fn new_divide(line: u64) -> Token {
        Token {
            kind: TokenKind::BinaryOperator,
            value: '/'.to_string(),
            line,
        }
    }
    pub fn new_modulo(line: u64) -> Token {
        Token {
            kind: TokenKind::BinaryOperator,
            value: '%'.to_string(),
            line,
        }
    }
    pub fn new_plus_eq(line: u64) -> Token {
        Token {
            kind: TokenKind::CompoundAssign,
            value: '+'.to_string(),
            line,
        }
    }
    pub fn new_minus_eq(line: u64) -> Token {
        Token {
            kind: TokenKind::CompoundAssign,
            value: '-'.to_string(),
            line,
        }
    }
    pub fn new_multiplication_eq(line: u64) -> Token {
        Token {
            kind: TokenKind::CompoundAssign,
            value: '*'.to_string(),
            line,
        }
    }
    pub fn new_divide_eq(line: u64) -> Token {
        Token {
            kind: TokenKind::CompoundAssign,
            value: '/'.to_string(),
            line,
        }
    }
    pub fn new_modulo_eq(line: u64) -> Token {
        Token {
            kind: TokenKind::CompoundAssign,
            value: '%'.to_string(),
            line,
        }
    }
    pub fn new_simple_arrow(line: u64) -> Token {
        Token {
            kind: TokenKind::SimpleArrow,
            value: "->".to_string(),
            line,
        }
    }
    pub fn new_open_paren(line: u64) -> Token {
        Token {
            kind: TokenKind::OpenParen,
            value: '('.to_string(),
            line,
        }
    }
    pub fn new_close_paren(line: u64) -> Token {
        Token {
            kind: TokenKind::CloseParen,
            value: ')'.to_string(),
            line,
        }
    }
    pub fn new_open_brace(line: u64) -> Token {
        Token {
            kind: TokenKind::OpenBrace,
            value: '{'.to_string(),
            line,
        }
    }
    pub fn new_close_brace(line: u64) -> Token {
        Token {
            kind: TokenKind::CloseBrace,
            value: '}'.to_string(),
            line,
        }
    }
    pub fn new_open_bracket(line: u64) -> Token {
        Token {
            kind: TokenKind::OpenBracket,
            value: '['.to_string(),
            line,
        }
    }
    pub fn new_close_bracket(line: u64) -> Token {
        Token {
            kind: TokenKind::CloseBracket,
            value: ']'.to_string(),
            line,
        }
    }
    pub fn new_comma(line: u64) -> Token {
        Token {
            kind: TokenKind::Comma,
            value: ','.to_string(),
            line,
        }
    }
    pub fn new_colon(line: u64) -> Token {
        Token {
            kind: TokenKind::Colon,
            value: ':'.to_string(),
            line,
        }
    }
    pub fn new_dot(line: u64) -> Token {
        Token {
            kind: TokenKind::Dot,
            value: '.'.to_string(),
            line,
        }
    }
    pub fn new_var() -> Token {
        Token {
            kind: TokenKind::Var,
            value: "var".to_string(),
            line: 0,
        }
    }
    pub fn new_const() -> Token {
        Token {
            kind: TokenKind::Const,
            value: "const".to_string(),
            line: 0,
        }
    }
    pub fn new_struct() -> Token {
        Token {
            kind: TokenKind::Structure,
            value: "struct".to_string(),
            line: 0,
        }
    }
    pub fn new_new() -> Token {
        Token {
            kind: TokenKind::New,
            value: "new".to_string(),
            line: 0,
        }
    }
    pub fn new_fn() -> Token {
        Token {
            kind: TokenKind::Fn,
            value: "fn".to_string(),
            line: 0,
        }
    }
    pub fn new_return() -> Token {
        Token {
            kind: TokenKind::Return,
            value: "return".to_string(),
            line: 0,
        }
    }
    pub fn new_test() -> Token {
        Token {
            kind: TokenKind::Test,
            value: "test".to_string(),
            line: 0,
        }
    }
    pub fn new_int() -> Token {
        Token {
            kind: TokenKind::Int,
            value: "int".to_string(),
            line: 0,
        }
    }
    pub fn new_any() -> Token {
        Token {
            kind: TokenKind::Any,
            value: "any".to_string(),
            line: 0,
        }
    }
    pub fn new_real() -> Token {
        Token {
            kind: TokenKind::Real,
            value: "real".to_string(),
            line: 0,
        }
    }
    pub fn new_bool() -> Token {
        Token {
            kind: TokenKind::Bool,
            value: "bool".to_string(),
            line: 0,
        }
    }
    pub fn new_self() -> Token {
        Token {
            kind: TokenKind::SelfKw,
            value: "self".to_string(),
            line: 0,
        }
    }
    pub fn new_for() -> Token {
        Token {
            kind: TokenKind::For,
            value: "for".to_string(),
            line: 0,
        }
    }
    pub fn new_in() -> Token {
        Token {
            kind: TokenKind::In,
            value: "in".to_string(),
            line: 0,
        }
    }
    pub fn new_end_line(line: u64) -> Token {
        Token {
            kind: TokenKind::EndLine,
            value: '\n'.to_string(),
            line,
        }
    }
    pub fn new_eof(line: u64) -> Token {
        Token {
            kind: TokenKind::EOF,
            value: "EOF".to_string(),
            line,
        }
    }

    pub fn set_line(self, line: u64) -> Self {
        Self {
            kind: self.kind,
            value: self.value,
            line,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_single_char() {
        let text = String::from("()[]=->+-*/%,:}{.");
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_open_paren(0),
                Token::new_close_paren(0),
                Token::new_open_bracket(0),
                Token::new_close_bracket(0),
                Token::new_equal(0),
                Token::new_simple_arrow(0),
                Token::new_plus(0),
                Token::new_minus(0),
                Token::new_multiplication(0),
                Token::new_divide(0),
                Token::new_modulo(0),
                Token::new_comma(0),
                Token::new_colon(0),
                Token::new_close_brace(0),
                Token::new_open_brace(0),
                Token::new_dot(0),
                Token::new_end_line(0),
                Token::new_eof(1),
            ]
        );
    }

    #[test]
    fn tokenize_new_line() {
        let text = String::from("\n \r\n");
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_end_line(0),
                Token::new_end_line(1),
                Token::new_end_line(2),
                Token::new_eof(3),
            ]
        );
    }

    #[test]
    fn tokenize_numeric_literal() {
        let text = String::from("123 3454 6768");
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_number(123.to_string(), 0),
                Token::new_number(3454.to_string(), 0),
                Token::new_number(6768.to_string(), 0),
                Token::new_end_line(0),
                Token::new_eof(1),
            ]
        );
    }

    #[test]
    fn tokenize_identifier() {
        let text = String::from("position mass");
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_identifier("position".to_string(), 0),
                Token::new_identifier("mass".to_string(), 0),
                Token::new_end_line(0),
                Token::new_eof(1),
            ]
        );
    }

    #[test]
    fn tokenize_keywords() {
        let text = String::from("var const struct new fn return self test any int real bool for in");
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_var(),
                Token::new_const(),
                Token::new_struct(),
                Token::new_new(),
                Token::new_fn(),
                Token::new_return(),
                Token::new_self(),
                Token::new_test(),
                Token::new_any(),
                Token::new_int(),
                Token::new_real(),
                Token::new_bool(),
                Token::new_for(),
                Token::new_in(),
                Token::new_end_line(0),
                Token::new_eof(1),
            ]
        );
    }

    #[test]
    fn tokenize_compound_assign() {
        let text = String::from("+= -= /= *= %=");
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_plus_eq(0),
                Token::new_minus_eq(0),
                Token::new_divide_eq(0),
                Token::new_multiplication_eq(0),
                Token::new_modulo_eq(0),
                Token::new_end_line(0),
                Token::new_eof(1),
            ]
        );
    }

    #[test]
    fn tokenize_complex_code() {
        let text = String::from(
            "var a = 5
        const b = 39 + a % 2
        var b = a * (65- 1) +(( 6%7 /4 )+ 8 )
        ",
        );
        let mut lexer: Lexer = Default::default();
        let _ = lexer.tokenize(text);

        for token in &lexer.tokens {
            print!("{:?} ", token.value);
        }

        assert_eq!(
            lexer.tokens,
            vec![
                Token::new_var().set_line(0),
                Token::new_identifier("a".to_string(), 0),
                Token::new_equal(0),
                Token::new_number(5.to_string(), 0),
                Token::new_end_line(0),
                Token::new_const().set_line(1),
                Token::new_identifier("b".to_string(), 1),
                Token::new_equal(1),
                Token::new_number(39.to_string(), 1),
                Token::new_plus(1),
                Token::new_identifier("a".to_string(), 1),
                Token::new_modulo(1),
                Token::new_number(2.to_string(), 1),
                Token::new_end_line(1),
                Token::new_var().set_line(2),
                Token::new_identifier("b".to_string(), 2),
                Token::new_equal(2),
                Token::new_identifier("a".to_string(), 2),
                Token::new_multiplication(2),
                Token::new_open_paren(2),
                Token::new_number(65.to_string(), 2),
                Token::new_minus(2),
                Token::new_number(1.to_string(), 2),
                Token::new_close_paren(2),
                Token::new_plus(2),
                Token::new_open_paren(2),
                Token::new_open_paren(2),
                Token::new_number(6.to_string(), 2),
                Token::new_modulo(2),
                Token::new_number(7.to_string(), 2),
                Token::new_divide(2),
                Token::new_number(4.to_string(), 2),
                Token::new_close_paren(2),
                Token::new_plus(2),
                Token::new_number(8.to_string(), 2),
                Token::new_close_paren(2),
                Token::new_end_line(2),
                Token::new_end_line(3),
                Token::new_eof(4),
            ]
        );
    }
}
