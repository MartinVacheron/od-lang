use crate::parser::VarType;
use super::errors_parser::ParserError;
use super::Parser;

use super::TokenKind;
use super::{ASTNodeKind, ExpressionKind, StatementKind};

impl Parser {
    // Function declaration. The syntax is:
    // fn add(x, y) {
    //    var z = x + y
    //
    //    return z * 2
    //}
    // The return is optional

    // Typed version
    // fn add(x: int, y: real) -> real {
    //    var z = x + y
    //
    //    return z * 2
    //}
    pub(super) fn parse_fn_declaration(&mut self) -> Result<StatementKind, ParserError> {
        // We eat the 'fn' keyword
        let _ = self.expect_token(TokenKind::Fn)?;

        // We get the function name
        let identifier = self
            .expect_token(TokenKind::Identifier)
            .map_err(|_| ParserError::MissingIdentifierAfterFn)?;

        // We get argument with their optional type
        let args_and_type = match self.at().kind {
            TokenKind::OpenParen => self.parse_fn_decl_args()?,
            _ => return Err(ParserError::MissingFnOpenParen)
        };

        let _ = self
            .expect_token(TokenKind::CloseParen)
            .map_err(|_| ParserError::MissingFnCloseParen)?;

        let return_type = self.parse_type_after_token(TokenKind::SimpleArrow)?;

        // We expect function body starting with '{'
        let _ = self
            .expect_token(TokenKind::OpenBrace)
            .map_err(|_| ParserError::MissingFnOpenBrace)?;

        // We get all the statements in the function's body
        let (body, return_stmt) = self.parse_fn_body()?;

        // Check for type coherence in simple return cases
        // For example: fn add() -> int { return true }  // Error
        if return_stmt.is_some() && return_type != VarType::Any {
            match return_stmt.as_ref().unwrap() {
                ExpressionKind::IntLiteral { .. } => if return_type != VarType::Int {
                    return Err(ParserError::WrongReturnType(return_type.to_string(), "int".into()))
                },
                ExpressionKind::RealLiteral { .. } => if return_type != VarType::Real {
                    return Err(ParserError::WrongReturnType(return_type.to_string(), "real".into()))
                },
                ExpressionKind::Identifier { symbol } => if return_type != VarType::Bool && (symbol == "true" || symbol == "false") {
                    return Err(ParserError::WrongReturnType(return_type.to_string(), "bool".into()))
                },
                ExpressionKind::ArrayLiteral { .. } => if return_type != VarType::Array {
                    return Err(ParserError::WrongReturnType(return_type.to_string(), "array".into()))
                },
                _ => {}
            }
        }

        // We expect function body ending with '}'
        let _ = self
            .expect_token(TokenKind::CloseBrace)
            .map_err(|_| ParserError::MissingFnCloseBrace)?;

        Ok(StatementKind::FnDeclaration {
            name: identifier.value,
            args_and_type,
            body,
            return_stmt,
            return_type
        })
    }

    pub(super) fn parse_fn_body(
        &mut self,
    ) -> Result<(Vec<ASTNodeKind>, Option<ExpressionKind>), ParserError> {
        let mut body: Vec<ASTNodeKind> = Vec::new();
        let mut return_stmt: Option<ExpressionKind> = None;

        while self.at().kind != TokenKind::CloseBrace {
            // We check if this is the return statment to end declaration
            if self.at().kind == TokenKind::Return {
                // We eat the return keyword
                let _ = self.eat()?;
                // We get the statement
                return_stmt = Some(self.parse_additive_expr()?);

                // The closing brace could be on the line after
                self.skip_end_lines();

                // We check if the declaration is over
                if self.at().kind != TokenKind::CloseBrace {
                    return Err(ParserError::StmtAfterReturn);
                }
            } else {
                // We parse all of the statements
                let stmt = self.parse_statement()?;

                if let Some(s) = stmt {
                    body.push(s);
                }
            }
        }

        Ok((body, return_stmt))
    }

    // Parse argument in prototype like:
    //  fn a(x, y: int, z: Planet)
    fn parse_fn_decl_args(&mut self) -> Result<Vec<(String, VarType)>, ParserError> {
        // We eat the open paren
        let _ = self.expect_token(TokenKind::OpenParen)?;
        // Empty output
        let mut args_and_type: Vec<(String, VarType)> = vec![];

        // If we are at an close paren, there is no arguments
        if let TokenKind::CloseParen = self.at().kind {
            return Ok(args_and_type);
        }

        // While there is an argument left
        while self.at().kind != TokenKind::CloseParen {
            // We get argument name
            let arg_name_expr = self.parse_additive_expr()?;

            // If the argument is not a identifier, error
            let arg_name = match arg_name_expr {
                 ExpressionKind::Identifier { symbol } => symbol,
                 _ => return Err(ParserError::FnDeclArgsNotString)
            };

            // We get the type
            let arg_type = self.parse_type_after_token(TokenKind::Colon)?;

            args_and_type.push((arg_name, arg_type));

            // We eat the comma if there is one
            if self.at().kind == TokenKind::Comma {
                let _ = self.eat()?;

                // If nothing after, error, can't write: fn a(x, ) {}
                if !matches!(self.at().kind, TokenKind::Identifier { .. }) {
                    return Err(ParserError::MissingIdentAfterComma);
                }
            }
            // Can't do fn a(x y) without comma
            else if matches!(self.at().kind, TokenKind::Identifier { .. }) {
                return Err(ParserError::MissingCommaBetweenArgs);
            }
        }

        Ok(args_and_type)
    }

    pub(super) fn parse_function_call(
        &mut self,
        caller: ExpressionKind,
    ) -> Result<ExpressionKind, ParserError> {
        Ok(ExpressionKind::FunctionCall {
            caller: Box::new(caller),
            args: self.parse_fn_call_args()?,
        })
    }

    pub(super) fn parse_fn_call_args(&mut self) -> Result<Vec<ExpressionKind>, ParserError> {
        // We eat the open paren
        let _ = self.expect_token(TokenKind::OpenParen)?;
        // If we are at an close paren, there is no arguments
        let args: Vec<_> = if let TokenKind::CloseParen = self.at().kind {
            Vec::new()
        } else {
            self.parse_args_call_list()?
        };

        let _ = self
            .expect_token(TokenKind::CloseParen)
            .map_err(|_| ParserError::MissingFnCloseParen)?;

        Ok(args)
    }

    fn parse_args_call_list(&mut self) -> Result<Vec<ExpressionKind>, ParserError> {
        let mut args: Vec<ExpressionKind> = vec![self.parse_additive_expr()?];

        while let TokenKind::Comma = self.at().kind {
            // We eat the comma
            let _ = self.eat()?;
            args.push(self.parse_additive_expr()?);
        }

        Ok(args)
    }
}
