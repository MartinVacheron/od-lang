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
    pub(super) fn parse_fn_declaration(&mut self, parse_constructor: bool) -> Result<StatementKind, ParserError> {
        // Token we expect to eat (case parsing constructor or not)
        let token_expected: TokenKind;

        // Cases constructor or not
        if parse_constructor {
            token_expected = TokenKind::New;
        } else {
            token_expected = TokenKind::Identifier;

            let _ = self.expect_token(TokenKind::Fn)?;

            if self.at().kind == TokenKind::New {
                return Err(ParserError::FnKwWithNew)
            }
        }

        // We get the function name
        let identifier = self
            .expect_token(token_expected)
            .map_err(|_| ParserError::MissingIdentifierAfterFn)?;

        // We get argument with their optional type
        let args_and_type = match self.at().kind {
            TokenKind::OpenParen => self.parse_fn_decl_args()?,
            _ => return Err(ParserError::MissingFnOpenParen)
        };

        let _ = self
            .expect_token(TokenKind::CloseParen)
            .map_err(|_| ParserError::MissingFnCloseParen)?;

        let tmp_return_type = self.parse_type_after_token(TokenKind::SimpleArrow)?;

        let mut body: Vec<ASTNodeKind> = vec![];
        let mut return_stmt: Option<ExpressionKind> = None;
        let return_type: VarType;

        // Two cases:
        // We are at an open brace to parse function body:
        //  -> mandatory for function
        //  -> optional for constructor
        // We are at anything else (so no function body):
        //  -> not possible if normal function
        //  -> possible for constructor
        match self.at().kind {
            TokenKind::OpenBrace => {
                self.eat()?;

                // We get all the statements in the function's body
                (body, return_stmt) = self.parse_fn_body()?;

                // Check for type coherence in simple return cases
                // For example: fn add() -> int { return true }  // Error

                // If there is a return statement, check the returned typed w/ return stmt
                if return_stmt.is_some() {
                    return_type = self.verify_expr_type(&return_stmt.clone().unwrap(), tmp_return_type)?
                } else {
                    // If there is no return statement and an non-void return type, error
                    if tmp_return_type != Some(VarType::Void) && tmp_return_type != None {
                        return Err(ParserError::ReturnTypeWithoutReturn(identifier.value))
                    }

                    // If no return statement, it is void
                    return_type = VarType::Void;
                };

                // We expect function body ending with '}'
                let _ = self
                    .expect_token(TokenKind::CloseBrace)
                    .map_err(|_| ParserError::MissingFnCloseBrace)?;
            }
            _ => {
                // Only if we dont parse the constructor we can have an empty body
                if !parse_constructor {
                    return Err(ParserError::MissingFnOpenBrace)
                }

                // For function, if there is no type it is an implicit void
                return_type = match tmp_return_type {
                    Some(t) => t,
                    None => VarType::Void
                };
            }
        }

        Ok(StatementKind::FnDeclaration {
            name: identifier.value,
            args_and_type,
            body,
            return_stmt,
            return_type
        })
    }

    // Gets all the statements to execute in the function
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
    pub(super) fn parse_fn_decl_args(&mut self) -> Result<Vec<(String, VarType)>, ParserError> {
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

            if let Some(VarType::Void) = arg_type {
                return Err(ParserError::VoidTypeForVar)
            }

            // We extract the type and put any if there is no type
            let arg_type = match arg_type {
                Some(t) => t,
                None => VarType::Any
            };

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

    // Parse function call: add(x, y)
    pub(super) fn parse_function_call(
        &mut self,
        caller: ExpressionKind,
    ) -> Result<ExpressionKind, ParserError>
    {
        match caller {
            ExpressionKind::Identifier { symbol } => {
                Ok(ExpressionKind::FunctionCall {
                    name: symbol,
                    args: self.parse_fn_call_args()?,
                })
            }
            _ => Err(ParserError::FnNameNotIdent(format!("{:?}", caller)))
        }
    }

    // Parses all the args
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

    // Parses all the names
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


#[cfg(test)]
mod tests {
    use crate::{ast::{ASTNodeKind, StatementKind}, lexer::Lexer, parser::{Parser, VarType}};

    // Function declaration. The syntax is:
    // fn add(x, y) {
    //    var z = x + y
    //
    //    return z * 2
    //}
    #[test]
    fn function_declaration() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        let code = "fn add(x, y) {
            var z = x + y
        
            return z * 2
        }";

        let _ = lexer.tokenize(code.into());

        let _ = parser.build_ast(lexer.tokens.clone());
        let nodes = parser.ast_nodes;
        
        assert!(nodes.len() == 1);
        let fn_decl = nodes.first().unwrap();

        if let ASTNodeKind::Statement(StatementKind::FnDeclaration {
            name,
            args_and_type,
            body,
            return_stmt,
            return_type
        }) = &fn_decl.node
        {
            assert_eq!(name, &"add".to_string());
            assert_eq!(
                args_and_type,
                &vec![
                    ("x".into(), VarType::Any),
                    ("y".into(), VarType::Any),
                ]
            );
            assert_eq!(body.len(), 1);
            assert!(return_stmt.is_some());
            assert_eq!(return_type, &VarType::Any);
        } else {
            assert!(false);
        }
    }

    // Test optional return
    #[test]
    fn function_declaration_no_return() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        let code = "fn add(x, y) {
            var z = x + y
        }";

        let _ = lexer.tokenize(code.into());

        parser.build_ast(lexer.tokens.clone()).unwrap();
        let nodes = parser.ast_nodes;
        
        assert!(nodes.len() == 1);
        let fn_decl = nodes.first().unwrap();

        if let ASTNodeKind::Statement(StatementKind::FnDeclaration {
            return_stmt,
            return_type,
            ..
        }) = &fn_decl.node
        {
            assert!(return_stmt.is_none());
            assert_eq!(return_type, &VarType::Void);
        } else {
            assert!(false);
        }
    }

    // Typed version
    // fn add(x: int, y: real) -> real {
    //    var z = x + y
    //
    //    return z * 2
    //}
    #[test]
    fn typed_function_declaration() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        let code = "fn add(x: int, y: []) -> bool {
            var z = x + y[0]
        
            return true
        }";

        let _ = lexer.tokenize(code.into());

        let _ = parser.build_ast(lexer.tokens.clone());
        let nodes = parser.ast_nodes;
        
        assert!(nodes.len() == 1);
        let fn_decl = nodes.first().unwrap();

        if let ASTNodeKind::Statement(StatementKind::FnDeclaration {
            name,
            args_and_type,
            body,
            return_stmt,
            return_type
        }) = &fn_decl.node
        {
            assert_eq!(name, &"add".to_string());
            assert_eq!(
                args_and_type,
                &vec![
                    ("x".into(), VarType::Int),
                    ("y".into(), VarType::Array(Box::new(VarType::Any))),
                ]
            );
            assert_eq!(body.len(), 1);
            assert!(return_stmt.is_some());
            assert_eq!(return_type, &VarType::Bool);
        } else {
            assert!(false);
        }
    }

    // Test optional return
    #[test]
    fn return_type_but_no_return() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        let code = "fn add(x, y) -> int {
            var z = x + y
        }";

        let _ = lexer.tokenize(code.into());

        assert!(parser.build_ast(lexer.tokens.clone()).is_err());
    }

    // Test easy type error handling
    #[test]
    fn wrong_return_type() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        // Bool
        let code = "fn add() -> bool { return 1 }";
        let _ = lexer.tokenize(code.into());

        assert!(parser.build_ast(lexer.tokens.clone()).is_err());

        // Int
        let code = "fn add() -> int { return true }";
        let _ = lexer.tokenize(code.into());

        assert!(parser.build_ast(lexer.tokens.clone()).is_err());

        // Real
        let code = "fn add() -> real { return [1, 2] }";
        let _ = lexer.tokenize(code.into());

        assert!(parser.build_ast(lexer.tokens.clone()).is_err());

        // Array
        let code = "fn add() -> [] { return 1 }";
        let _ = lexer.tokenize(code.into());

        assert!(parser.build_ast(lexer.tokens.clone()).is_err());
    }
}