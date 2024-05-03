use super::errors_parser::ParserError;
use super::Parser;
use super::StatementKind;
use super::TokenKind;

impl Parser {
    pub(super) fn parse_test_declaration(&mut self) -> Result<StatementKind, ParserError> {
        // We eat the "test" keyword
        self.eat()?;

        // We get test name
        let test_name = self
            .expect_token(TokenKind::Identifier)
            .map_err(|e| ParserError::NoTestName(e.to_string()))?;

        // We get rid of the open brace
        let _ = self
            .expect_token(TokenKind::OpenBrace)
            .map_err(|e| ParserError::MissingOpenBrace(e.to_string()))?;

        // Until end of test, we collect statments
        let (body, return_stmt) = self.parse_fn_body()?;

        // TODO: Check for no assert call

        // We expect test body ending with '}'
        let _ = self
            .expect_token(TokenKind::CloseBrace)
            .map_err(|e| ParserError::MissingCloseBrace(e.to_string()))?;

        // Ne return statement allowed in tests
        if return_stmt.is_some() {
            return Err(ParserError::ReturnInTest);
        }

        Ok(StatementKind::TestDeclaration {
            name: test_name.value,
            body,
        })
    }
}
