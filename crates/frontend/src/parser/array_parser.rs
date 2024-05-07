use crate::ast::ArrayIndexing;

use super::errors_parser::ParserError;
use super::Parser;

use super::ExpressionKind;
use super::TokenKind;

impl Parser {
    // Parse:
    //  var a = []
    //  var a = [1, 2, 3]
    pub(super) fn parse_array_declaration(&mut self) -> Result<ExpressionKind, ParserError> {
        match self.at().kind {
            // Empty array
            TokenKind::CloseBracket => {
                // We eat it
                self.eat()?;

                Ok(ExpressionKind::ArrayLiteral { values: Vec::new() })
            }
            // Elements parsing
            _ => {
                let mut values: Vec<ExpressionKind> = vec![self.parse_additive_expr()?];

                while let TokenKind::Comma = self.at().kind {
                    // We eat the comma
                    let _ = self.eat()?;
                    values.push(self.parse_additive_expr()?);
                }

                self.expect_token(TokenKind::CloseBracket)?;

                Ok(ExpressionKind::ArrayLiteral { values })
            }
        }
    }

    // Parse:
    //  var a = [1, 2, 3]
    //  var b = a[:2]
    //  var b = a[0]
    //  var b = a[0:2]
    //  var b = a[0:]
    // Also:
    //  var b = [mars.get_radius():mars.pos.x]
    pub(super) fn parse_array_call(
        &mut self,
        array: ExpressionKind,
    ) -> Result<ExpressionKind, ParserError>
    {
        // First and last indexes
        let start: ExpressionKind;
        let end: ExpressionKind;

        let index: ArrayIndexing;

        // First token could be a ':'
        match self.at().kind {
            // Checks for var b = a[]
            TokenKind::CloseBracket => {
                return Err(ParserError::ArraySlice(
                    ParserError::EmptyArraySlice.to_string(),
                ))
            }
            // Case a[:9]
            TokenKind::Colon => {
                // We eat it
                self.eat()?;

                // If: a[:] -> Error
                if self.at().kind == TokenKind::CloseBracket {
                    return Err(ParserError::ArraySlice(
                        ParserError::NoSliceIdenxWithColon.to_string(),
                    ))
                }

                // Get end index
                end = self.parse_member_expression()?;

                index = ArrayIndexing::Slice {
                    start: None,
                    end: Some(Box::new(end)),
                };
            }
            // Else we try to parse the value of first index
            _ => {
                start = self.parse_member_expression()?;

                // All cases
                match self.at().kind {
                    // Case a[0]
                    TokenKind::CloseBracket => {
                        index = ArrayIndexing::Single(Box::new(start));
                    }
                    _ => {
                        // Expected ':' after first index
                        self.expect_token(TokenKind::Colon).map_err(|_| {
                            ParserError::ArraySlice(ParserError::NoColonAfterFirstIndex.to_string())
                        })?;

                        match self.at().kind {
                            // Case a[0:]
                            TokenKind::CloseBracket => {
                                index = ArrayIndexing::Slice {
                                    start: Some(Box::new(start)),
                                    end: None,
                                };
                            }
                            // Case [0:10]
                            _ => {
                                end = self.parse_member_expression()?;

                                index = ArrayIndexing::Slice {
                                    start: Some(Box::new(start)),
                                    end: Some(Box::new(end)),
                                };
                            }
                        }
                    }
                }
            }
        }

        // We expect the closing ]
        self.expect_token(TokenKind::CloseBracket)
            .map_err(|_| ParserError::ArraySlice("missing closing bracket ']'".into()))?;

        match array {
            ExpressionKind::Identifier { symbol } => {
                Ok(ExpressionKind::ArrayCall {
                    name: symbol,
                    index,
                })
            }
            _ => Err(ParserError::ArrayNameNotIdent(format!("{:?}", array)))
        }
    }
}
