use super::errors_parser::ParserError;
use super::Parser;

use super::TokenKind;
use super::{ExpressionKind, StatementKind};

impl Parser {
    // Variable declaration. Different possibilities:
    //  var x
    //  var x = 5
    //  const x = 5
    //  var x, y, z
    //  var x, y, z = 1, 2, 3
    //
    // Typed ones are possible too. Value mandatory
    //  var x: int = 1
    //  var y: real = 1.2
    //  const a, b, c: real = 5.6
    pub(crate) fn parse_var_declaration(&mut self) -> Result<StatementKind, ParserError> {
        // We check what type of declaration it is
        // If we are here, the parser knows we are at a 'var' or 'const' so
        // the second arm is ok
        let is_constant = match self.eat()?.kind {
            TokenKind::Const => true,
            _ => false,
        };

        // We get first identifier
        let identifier = self
            .expect_token(TokenKind::Identifier)
            .map_err(|_| ParserError::ExpectedVarNameAfterKw(self.at().value.clone()))?;

        // All cases
        match self.at().kind {
            // Check for comma declaration
            TokenKind::Comma => {
                // We eat the comma
                self.eat()?;
                self.parse_comma_declaration(identifier.value, is_constant)
            }
            // Else signle declaration
            _ => self.parse_single_var_decl(identifier.value, is_constant),
        }
    }

    fn parse_single_var_decl(
        &mut self,
        var_name: String,
        constant: bool,
    ) -> Result<StatementKind, ParserError> {
        // If there is no value in declaration like: var x
        if self.at().kind == TokenKind::EndLine && constant {
            // But this is not possible with constants declaration
            return Err(ParserError::ConstVarNoVal(self.at().value.clone()));
        }

        // We get the var type
        let var_type = self.parse_type_after_token(TokenKind::Colon)?;

        // All cases
        match self.at().kind {
            // End of line
            TokenKind::EndLine => {
                Ok(StatementKind::VarDeclaration {
                    name: var_name,
                    value: None,
                    constant,
                    var_type,
                })
            }
            TokenKind::Equals => {
                self.eat()?;

                // We declare the var expecting its value to follow. We check if we create a new struct
                if let TokenKind::New = self.at().kind {
                    Ok(self
                        .parse_struct_creation(var_name, false)
                        .map_err(|e| ParserError::ParseVarDecl(e.to_string()))?)
                } else {
                    let declaration = self
                        .parse_additive_expr()
                        .map_err(|e| ParserError::ParseVarDecl(e.to_string()))?;

                    Ok(StatementKind::VarDeclaration {
                        name: var_name,
                        value: Some(declaration),
                        constant,
                        var_type,
                    })
                }
            }
            _ => Err(ParserError::ParseVarDecl("expected 'end line' or '=' after var name or type name".into()))
        }
    }

    fn parse_comma_declaration(
        &mut self,
        first_ident: String,
        constant: bool,
    ) -> Result<StatementKind, ParserError> {
        // All of variable names
        let mut other_idents: Vec<String> = vec![first_ident];

        // We check first that next token is an identifier to avoid:
        // var a,
        if !matches!(self.at().kind, TokenKind::Identifier { .. }) {
            return Err(ParserError::MissingIdentCommaDecl(self.at().value.clone()));
        }

        // While we don't hit '=' or end of the line, we parse names
        while (self.at().kind != TokenKind::Equals
            && self.at().kind != TokenKind::EndLine
            && self.at().kind != TokenKind::Colon)
            || self.at().kind == TokenKind::Comma
        {
            // If there is a comma, we eat it to continue
            if self.at().kind == TokenKind::Comma {
                self.eat()?;
            }

            other_idents.push(
                self.expect_token(TokenKind::Identifier)
                    .map_err(|_| ParserError::MissingIdentCommaDecl(self.at().value.clone()))?
                    .value,
            );
        }

        let var_type = self.parse_type_after_token(TokenKind::Colon)?;

        // If we were at the end of line, it means that declared var dont have a value
        if self.at().kind == TokenKind::EndLine {
            // But this is not possible with constants declaration
            if constant {
                return Err(ParserError::ConstCommaDeclNoValue);
            }

            // All declaration
            let mut declarations: Vec<StatementKind> = Vec::new();

            for ident in other_idents {
                declarations.push(StatementKind::VarDeclaration {
                    name: ident,
                    value: None,
                    constant,
                    var_type: var_type.clone(), // Clone because of loop iterations
                });
            }
            return Ok(StatementKind::VarCommaDeclaration { declarations });
        }

        // We know we are at a '=' sign because of previous conditions
        self.expect_token(TokenKind::Equals)?;

        // All values
        let mut values: Vec<ExpressionKind> = Vec::new();

        // Until end of line we parse values
        while self.at().kind != TokenKind::EndLine {
            values.push(
                self.parse_additive_expr()
                    .map_err(|_| ParserError::NonExprCommaDecl)?,
            );

            // We eat the comma until reaching \n
            if self.at().kind == TokenKind::Comma {
                self.eat()?;
            }
        }

        // We need at least one value
        if values.is_empty() {
            return Err(ParserError::NoExprProvidedCommaDecl);
        }

        // We need to have either one value and so all the identifier have the same
        // or the exact same number of values and identifier
        if values.len() != 1 && values.len() != other_idents.len() {
            return Err(ParserError::WrongExprNbCommaDecl);
        }

        // We return all the values
        let mut declarations: Vec<StatementKind> = Vec::new();

        // If only one value declared
        if values.len() == 1 {
            let val = values.pop().unwrap();

            // We push the same for all
            for ident in other_idents.into_iter() {
                declarations.push(StatementKind::VarDeclaration {
                    name: ident,
                    value: Some(val.clone()),
                    constant,
                    var_type: var_type.clone(), // Same, for iterations
                });
            }
        } else {
            // Else, each has its value
            for (ident, val) in other_idents.into_iter().zip(values) {
                declarations.push(StatementKind::VarDeclaration {
                    name: ident,
                    value: Some(val),
                    constant,
                    var_type: var_type.clone(), // Same
                });
            }
        }

        Ok(StatementKind::VarCommaDeclaration { declarations })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTNode, ASTNodeKind};
    use crate::lexer::Lexer;
    use crate::parser::VarType;

    #[test]
    fn parse_var_declarations() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "var x
        var y = 6
        const z = 12.
        ",
        );
        let _ = lexer.tokenize(code);
        let _ = parser.build_ast(lexer.tokens);

        assert_eq!(
            parser.ast_nodes,
            vec![
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'x'.to_string(),
                        value: None,
                        constant: false,
                        var_type: VarType::Any
                    }),
                    0
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'y'.to_string(),
                        value: Some(ExpressionKind::IntLiteral { value: 6 }),
                        constant: false,
                        var_type: VarType::Any
                    }),
                    1
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'z'.to_string(),
                        value: Some(ExpressionKind::RealLiteral { value: 12. }),
                        constant: true,
                        var_type: VarType::Any
                    }),
                    2
                )
            ]
        );
    }

    #[test]
    fn parse_var_type_decl() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "var x: int = 1
            const z: real = 12.
        ",
        );
        let _ = lexer.tokenize(code);
        let _ = parser.build_ast(lexer.tokens);

        assert_eq!(
            parser.ast_nodes,
            vec![
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'x'.to_string(),
                        value: Some(ExpressionKind::IntLiteral { value: 1 }),
                        constant: false,
                        var_type: VarType::Int
                    }),
                    0
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'z'.to_string(),
                        value: Some(ExpressionKind::RealLiteral { value: 12. }),
                        constant: true,
                        var_type: VarType::Real
                    }),
                    1
                )
            ]
        );
    }
}
