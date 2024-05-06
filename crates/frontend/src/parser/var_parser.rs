use super::errors_parser::ParserError;
use super::Parser;

use super::TokenKind;
use super::VarType;
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
        // If there is no value in declaration like: const x
        if self.at().kind == TokenKind::EndLine && constant {
            // But this is not possible with constants declaration
            return Err(ParserError::ConstVarNoVal(var_name));
        }

        // We get the var type
        let var_type = self.parse_type_after_token(TokenKind::Colon)?;

        // If there is no value in declaration like: const x: int
        if self.at().kind == TokenKind::EndLine && constant {
            // But this is not possible with constants declaration
            return Err(ParserError::ConstVarNoVal(var_name));
        }

        // Check for use of 'void' on variables
        if let Some(VarType::Void) = var_type {
            return Err(ParserError::VoidTypeForVar)
        }

        // All cases
        match self.at().kind {
            // End of line
            TokenKind::EndLine => {
                // We extract the type and put any if there is no type
                let var_type = match var_type {
                    Some(t) => t,
                    None => VarType::Any
                };

                // So no initialization value, we are gonna give one ourself
                  Ok(StatementKind::VarDeclaration {
                    name: var_name,
                    value: var_type.get_default_value(),
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

                    let var_type = self.verify_expr_type(&declaration, var_type)?;

                    // Usefull for the case:    var a: real = 1
                    // We convert the 'int' literal into a 'real' one
                    let declaration = if var_type == VarType::Real {
                        // We potentially cast an 'int' literal to go inside 'real'
                        cast_int_literal_to_real(declaration)
                    } else {
                        declaration
                    };

                    Ok(StatementKind::VarDeclaration {
                        name: var_name,
                        value: declaration,
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

        if let Some(VarType::Void) = var_type {
            return Err(ParserError::VoidTypeForVar)
        }

        // If we were at the end of line, it means that declared var dont have a value
        if self.at().kind == TokenKind::EndLine {
            // But this is not possible with constants declaration
            if constant {
                return Err(ParserError::ConstCommaDeclNoValue);
            }

            // We extract the type and put any if there is no type
            let var_type = match var_type {
                Some(t) => t,
                None => VarType::Any
            };

            // All declaration
            let mut declarations: Vec<StatementKind> = Vec::new();

            // We get default value
            for ident in other_idents {
                declarations.push(StatementKind::VarDeclaration {
                    name: ident,
                    value: var_type.get_default_value(),
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

            let var_type = self.verify_expr_type(&val, var_type)?;

            // We push the same for all
            for ident in other_idents.into_iter() {
                declarations.push(StatementKind::VarDeclaration {
                    name: ident,
                    value: val.clone(),
                    constant,
                    var_type: var_type.clone(), // Same, for iterations
                });
            }
        } else {
            // Else, each has its value
            for (ident, val) in other_idents.into_iter().zip(values) {
                let var_type = self.verify_expr_type(&val, var_type.clone())?;
                
                declarations.push(StatementKind::VarDeclaration {
                    name: ident,
                    value: val,
                    constant,
                    var_type: var_type, // Same
                });
            }
        }

        Ok(StatementKind::VarCommaDeclaration { declarations })
    }

    // Verifies that an expression has the same type as the variable assigned
    pub(super) fn verify_expr_type(&self, expr: &ExpressionKind, var_type: Option<VarType>) -> Result<VarType, ParserError> {
        if let Some(t) = var_type {
            // If the variable is 'any' type, no problem
            if t == VarType::Any {
                return Ok(t)
            }

            match expr.get_expr_type() {
                // We allow implicit cast from int to real
                VarType::Int => if t != VarType::Int && t != VarType::Real {
                    return Err(ParserError::WrongVarType(t.to_string(), "int".into()))
                },
                VarType::Real => if t != VarType::Real {
                    return Err(ParserError::WrongVarType(t.to_string(), "real".into()))
                },
                VarType::Bool => if t != VarType::Bool {
                    return Err(ParserError::WrongVarType(t.to_string(), "bool".into()))
                },
                VarType::Array(..) => if !matches!(t, VarType::Array(..)) {
                    return Err(ParserError::WrongVarType(t.to_string(), "array".into()))
                },
                _ => if t == VarType::Void {
                        return Err(ParserError::WrongVarType(t.to_string(), "void".into()))
                }
            };

            Ok(t)
        } else {
            Ok(expr.get_expr_type())
        }
    }

}

pub(super) fn cast_int_literal_to_real(expr: ExpressionKind) -> ExpressionKind {
    if let ExpressionKind::IntLiteral { value } = expr {
        ExpressionKind::RealLiteral { value: value as f64 }
    } else {
        expr
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ASTNode, ASTNodeKind};
    use crate::lexer::Lexer;
    use crate::parser::VarType;

    // Variable declarations and type inference
    #[test]
    fn parse_var_declarations() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "var x
            var y = 6
            const z = 12.
            var a: any
            var b: int
            var bb: int = -6
            var c = 87
            var d: real
            var e: real = 1
            var f: real = -87.9
            var g: bool
            var h = true
            var i: []
            var j = [1, 2]
            var k: Planet
            var l: any = [1, 2]
            var m: any = foo
            "
        );
        
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes,
            vec![
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'x'.to_string(),
                        value: ExpressionKind::Identifier { symbol: "null".into() },
                        constant: false,
                        var_type: VarType::Any
                    }),
                    0
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'y'.to_string(),
                        value: ExpressionKind::IntLiteral { value: 6 },
                        constant: false,
                        var_type: VarType::Int
                    }),
                    1
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'z'.to_string(),
                        value: ExpressionKind::RealLiteral { value: 12. },
                        constant: true,
                        var_type: VarType::Real
                    }),
                    2
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'a'.to_string(),
                        value: ExpressionKind::Identifier { symbol: "null".into() },
                        constant: false,
                        var_type: VarType::Any
                    }),
                    3
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'b'.to_string(),
                        value: ExpressionKind::IntLiteral { value: 0 },
                        constant: false,
                        var_type: VarType::Int
                    }),
                    4
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: "bb".to_string(),
                        value: ExpressionKind::IntLiteral { value: -6 },
                        constant: false,
                        var_type: VarType::Int
                    }),
                    5
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'c'.to_string(),
                        value: ExpressionKind::IntLiteral { value: 87 },
                        constant: false,
                        var_type: VarType::Int
                    }),
                    6
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'd'.to_string(),
                        value: ExpressionKind::RealLiteral { value: 0. },
                        constant: false,
                        var_type: VarType::Real
                    }),
                    7
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'e'.to_string(),
                        value: ExpressionKind::RealLiteral { value: 1. },
                        constant: false,
                        var_type: VarType::Real
                    }),
                    8
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'f'.to_string(),
                        value: ExpressionKind::RealLiteral { value: -87.9 },
                        constant: false,
                        var_type: VarType::Real
                    }),
                    9
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'g'.to_string(),
                        value: ExpressionKind::Identifier { symbol: "false".into() },
                        constant: false,
                        var_type: VarType::Bool
                    }),
                    10
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'h'.to_string(),
                        value: ExpressionKind::Identifier { symbol: "true".into() },
                        constant: false,
                        var_type: VarType::Bool
                    }),
                    11
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'i'.to_string(),
                        value: ExpressionKind::ArrayLiteral { values: vec![] },
                        constant: false,
                        var_type: VarType::Array(Box::new(VarType::Any))
                    }),
                    12
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'j'.to_string(),
                        value: ExpressionKind::ArrayLiteral { values: vec![
                            ExpressionKind::IntLiteral { value: 1 },
                            ExpressionKind::IntLiteral { value: 2 }
                        ]},
                        constant: false,
                        var_type: VarType::Array(Box::new(VarType::Any))
                    }),
                    13
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'k'.to_string(),
                        value: ExpressionKind::EmptyStructLiteral { name: "Planet".into() },
                        constant: false,
                        var_type: VarType::Struct("Planet".into())
                    }),
                    14
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'l'.to_string(),
                        value: ExpressionKind::ArrayLiteral { values: vec![
                            ExpressionKind::IntLiteral { value: 1 },
                            ExpressionKind::IntLiteral { value: 2 }
                        ]},
                        constant: false,
                        var_type: VarType::Any
                    }),
                    15
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: 'm'.to_string(),
                        value: ExpressionKind::Identifier { symbol: "foo".into() },
                        constant: false,
                        var_type: VarType::Any
                    }),
                    16
                ),
            ]
        );
    }

    // Var declaration on forbidden type
    #[test]
    fn forbidden_type_var_decl() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from("var x: void");
        
        lexer.tokenize(code).unwrap();

        assert!(parser.build_ast(lexer.tokens).is_err());
    }

    // Constant with no value
    #[test]
    fn constant_with_no_value() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from("const x");
        lexer.tokenize(code).unwrap();

        assert!(parser.build_ast(lexer.tokens.clone()).is_err());
        
        let code = String::from("const x: int");
        lexer.tokenize(code).unwrap();

        assert!(parser.build_ast(lexer.tokens).is_err());
    }

    // Comma var declaration
    #[test]
    fn comma_declarations() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "var x, y, z
            const a, b: int = 98
            var c, d, e: real
            var f, g = [1, 2, 3], [2, 3]
            var h, i, j = true
            var k, l: Planet
            "
        );
        
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes,
            vec![
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarCommaDeclaration {
                        declarations: vec![
                            StatementKind::VarDeclaration {
                                name: 'x'.to_string(),
                                value: ExpressionKind::Identifier { symbol: "null".into() },
                                constant: false,
                                var_type: VarType::Any
                            },
                            StatementKind::VarDeclaration {
                                name: 'y'.to_string(),
                                value: ExpressionKind::Identifier { symbol: "null".into() },
                                constant: false,
                                var_type: VarType::Any
                            },
                            StatementKind::VarDeclaration {
                                name: 'z'.to_string(),
                                value: ExpressionKind::Identifier { symbol: "null".into() },
                                constant: false,
                                var_type: VarType::Any
                            },
                        ]
                    }),
                    0
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarCommaDeclaration {
                        declarations: vec![
                            StatementKind::VarDeclaration {
                                name: 'a'.to_string(),
                                value: ExpressionKind::IntLiteral { value: 98 },
                                constant: true,
                                var_type: VarType::Int
                            },
                            StatementKind::VarDeclaration {
                                name: 'b'.to_string(),
                                value: ExpressionKind::IntLiteral { value: 98 },
                                constant: true,
                                var_type: VarType::Int
                            }
                        ]
                    }),
                    1
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarCommaDeclaration {
                        declarations: vec![
                            StatementKind::VarDeclaration {
                                name: 'c'.to_string(),
                                value: ExpressionKind::RealLiteral { value: 0. },
                                constant: false,
                                var_type: VarType::Real
                            },
                            StatementKind::VarDeclaration {
                                name: 'd'.to_string(),
                                value: ExpressionKind::RealLiteral { value: 0. },
                                constant: false,
                                var_type: VarType::Real
                            },
                            StatementKind::VarDeclaration {
                                name: 'e'.to_string(),
                                value: ExpressionKind::RealLiteral { value: 0. },
                                constant: false,
                                var_type: VarType::Real
                            },
                        ]
                    }),
                    2
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarCommaDeclaration {
                        declarations: vec![
                            StatementKind::VarDeclaration {
                                name: 'f'.to_string(),
                                value: ExpressionKind::ArrayLiteral { values: vec![
                                    ExpressionKind::IntLiteral { value: 1 },
                                    ExpressionKind::IntLiteral { value: 2 },
                                    ExpressionKind::IntLiteral { value: 3 }
                                ]},
                                constant: false,
                                var_type: VarType::Array(Box::new(VarType::Any))
                            },
                            StatementKind::VarDeclaration {
                                name: 'g'.to_string(),
                                value: ExpressionKind::ArrayLiteral { values: vec![
                                    ExpressionKind::IntLiteral { value: 2 },
                                    ExpressionKind::IntLiteral { value: 3 }
                                ]},
                                constant: false,
                                var_type: VarType::Array(Box::new(VarType::Any))
                            }
                        ]
                    }),
                    3
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarCommaDeclaration {
                        declarations: vec![
                            StatementKind::VarDeclaration {
                                name: 'h'.to_string(),
                                value: ExpressionKind::Identifier { symbol: "true".into() },
                                constant: false,
                                var_type: VarType::Bool
                            },
                            StatementKind::VarDeclaration {
                                name: 'i'.to_string(),
                                value: ExpressionKind::Identifier { symbol: "true".into() },
                                constant: false,
                                var_type: VarType::Bool
                            },
                            StatementKind::VarDeclaration {
                                name: 'j'.to_string(),
                                value: ExpressionKind::Identifier { symbol: "true".into() },
                                constant: false,
                                var_type: VarType::Bool
                            }
                        ]
                    }),
                    4
                ),
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarCommaDeclaration {
                        declarations: vec![
                            StatementKind::VarDeclaration {
                                name: 'k'.to_string(),
                                value: ExpressionKind::EmptyStructLiteral { name: "Planet".into() },
                                constant: false,
                                var_type: VarType::Struct("Planet".into())
                            },
                            StatementKind::VarDeclaration {
                                name: 'l'.to_string(),
                                value: ExpressionKind::EmptyStructLiteral { name: "Planet".into() },
                                constant: false,
                                var_type: VarType::Struct("Planet".into())
                            }
                        ]
                    }),
                    5
                ),
            ]
        );
    }
}
