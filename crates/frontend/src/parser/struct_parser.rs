use super::errors_parser::ParserError;
use super::Parser;

use super::TokenKind;
use super::VarType;
use super::StatementKind;

impl Parser {
    // Structure declaration are:
    // struct Planet {
    //    var position
    //    const mass
    //
    //    new(position, mass)
    //
    //    fn update_pos(arg1, arg2) {
    //      do stuff
    //    }
    //}
    // All constants must have been initialized in the new function
    pub(super) fn parse_struct_declaration(&mut self) -> Result<StatementKind, ParserError> {
        // We eat struct keyword
        let _ = self.eat()?;

        // We get struct name
        let identifier = self
            .expect_token(TokenKind::Identifier)
            .map_err(|_| ParserError::NoStructIdentifier)?;

        // We expect { to start members and functions declaration
        let _ = self
            .expect_token(TokenKind::OpenBrace)
            .map_err(|_| ParserError::MissingStructOpenBrace)?;

        // We collect all the members
        let mut members: Vec<(String, VarType, bool)> = Vec::new();
        // let mut constructor_args: Option<Vec<(String, VarType)>> = None;
        // let mut constructor_body: Option<Vec<ASTNodeKind>> = None;
        let mut functions: Vec<StatementKind> = Vec::new();

        let mut has_constructor: bool = false;
        
        // let mut constructor: Option<Box<StatementKind>> = None;

        // Temporary members to be able to parse a full line et get potential
        // comma declaration. We use it to apply the type at the end of the line
        let mut line_members: Vec<(String, VarType, bool)> = Vec::new();

        while !self.is_eof() && self.at().value != "}" {
            match self.at().kind {
                // If new line, we skip it
                TokenKind::EndLine => {
                    let _ = self.eat()?;
                }
                // Declaration of all the members
                TokenKind::Var | TokenKind::Const => {
                    // We get const or not
                    let constant = self.at().kind == TokenKind::Const;

                    // We eat the var kw
                    let _ = self.eat()?;

                    // Manages comma declaration: var a, b, c
                    while self.at().kind != TokenKind::EndLine {
                        // We get the member's name
                        let membre_name = self.expect_token(TokenKind::Identifier)
                                .map_err(|e| { ParserError::NoIdentifierAfterMemberDecl(e.to_string()) })?
                                .value;

                        // We put 'any' type at first, and we'll see at the end if there
                        // is a type declaration
                        line_members.push((
                            membre_name,
                            VarType::Any,
                            constant,
                        ));

                        // Cases
                        match self.at().kind {
                            // We can be at a comma, we eat it and continue
                            TokenKind::Comma => {
                                self.eat()?;

                                // If this is the end of the lien, the code is:
                                // var a, b,
                                // We don't accept the last comma
                                if self.at().kind == TokenKind::EndLine {
                                    return Err(ParserError::MissingIdentCommaDecl(
                                        self.at().value.clone(),
                                    ));
                                }
                            }
                            // If we are at endline, it's gonna end on its own
                            TokenKind::EndLine => {}
                            // Could be struct Vec { var a, b, c }. We exit the loop
                            TokenKind::CloseBrace => break,
                            // Could be struct Vec {
                            //      var a, b: real
                            //  }
                            // We exit the loop
                            TokenKind::Colon => break,
                            // Else, error
                            _ => {
                                return Err(ParserError::MissingIdentCommaDecl(
                                    self.at().value.clone(),
                                ));
                            }
                        }
                    }

                    // We get the member type
                    let var_type = self.parse_type_after_token(TokenKind::Colon)?;

                    if let Some(VarType::Void) = var_type {
                        return Err(ParserError::VoidTypeForVar)
                    }

                    // We extract the type and put it to all the members if there is one
                    if let Some(t) = var_type {
                        line_members.iter_mut().for_each(|m| m.1 = t.clone());
                    }

                    // We had the members of the line to the global list
                    members.append(&mut line_members);
                }
                // We parse the constructor. It is not mandatory unless there is at least one const member
                TokenKind::New => {
                    if has_constructor {
                        return Err(ParserError::MultipleConstructor)
                    } else {
                        has_constructor = true;
                    }
                    
                    let constructor = self.parse_fn_declaration(true)?;

                    // TODO: Error check
                    if let StatementKind::FnDeclaration { args_and_type, return_stmt, return_type, .. } = &constructor {
                        let args_name = args_and_type.iter().map(|a| &a.0).collect::<Vec<&String>>();

                        // We get all the constant members
                        let const_members = members
                            .iter()
                            .filter(|m| m.2)
                            .collect::<Vec<&(String, VarType, bool)>>();

                        println!("Const members:{:?}", const_members);
                        println!("All args name:{:?}", args_name);

                        // All the constant members must have been declared in the constructor
                        if !const_members.iter().all(|m| args_name.contains(&&m.0)) {
                            return Err(ParserError::MissingConstMemberInConstructor)
                        }

                        // We check if there is a return type
                        if return_type != &VarType::Void {
                            return Err(ParserError::ConstructorWithType)
                        }

                        // We check if there is a return statement
                        if return_stmt.is_some() {
                            return Err(ParserError::ConstructorWithType)
                        }

                        // We save it
                        functions.push(constructor);
                    } else {
                        panic!("Constructor must be a function")
                    }
                }
                TokenKind::Fn => {
                    let function = self.parse_fn_declaration(false)?;

                    if let StatementKind::FnDeclaration { .. } = function {
                        functions.push(function);
                    }
                }
                _ => {
                    return Err(ParserError::NonAuthorizedKeywordInStruct(
                        self.at().value.clone(),
                    ))
                }
            }
        }

        // End of struct declaration, expect '}'
        let _ = self
            .expect_token(TokenKind::CloseBrace)
            .map_err(|_| ParserError::MissingStructCloseBrace)?;

        if !members
            .iter()
            .filter(|m| m.2)
            .collect::<Vec<&(String, VarType, bool)>>()
            .is_empty()
            && !has_constructor
        {
            return Err(ParserError::MissingConstMemberInConstructor)
        }

        Ok(StatementKind::StructDeclaration {
            name: identifier.value,
            members,
            functions,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::*;
    use crate::{ast::*, lexer::Lexer};

    #[test]
    fn parse_struct_member_declaration() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var y
                const z

                new(z)
            }",
        );
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes.first().unwrap().node,
            ASTNodeKind::Statement(StatementKind::StructDeclaration {
                name: "Planet".to_string(),
                members: vec![("y".to_string(), VarType::Any, false), ("z".to_string(), VarType::Any, true),],
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "new".into(),
                        args_and_type: vec![("z".into(), VarType::Any)],
                        body: vec![],
                        return_stmt: None,
                        return_type: VarType::Void
                    }
                ],
            })
        );
    }

    #[test]
    fn parse_struct_constructor_return() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var x

                new() -> int
            }",
        );
        
        lexer.tokenize(code).unwrap();
        assert!(parser.build_ast(lexer.tokens).is_err());
    }

    #[test]
    fn parse_struct_constructor_missing_with_const() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var y
                const z
            }",
        );
        let _ = lexer.tokenize(code);

        assert!(parser.build_ast(lexer.tokens).is_err());
    }

    #[test]
    fn parse_struct_const_missing_in_constructor() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var y
                const z

                new(x)
            }",
        );
        let _ = lexer.tokenize(code);

        assert!(parser.build_ast(lexer.tokens).is_err());
    }

    #[test]
    fn parse_struct_double_constructor() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var y
                const z

                new(z)
                new(x)
            }",
        );
        let _ = lexer.tokenize(code);

        assert!(parser.build_ast(lexer.tokens).is_err());
    }

    #[test]
    fn parse_struct_comma_decl() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var x, y

                new(x, y)
            }");
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes.first().unwrap().node,
            ASTNodeKind::Statement(StatementKind::StructDeclaration {
                name: "Planet".to_string(),
                members: vec![("x".to_string(), VarType::Any, false), ("y".to_string(), VarType::Any, false)],
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "new".into(),
                        args_and_type: vec![
                            ("x".into(), VarType::Any),
                            ("y".into(), VarType::Any),
                        ],
                        body: vec![],
                        return_stmt: None,
                        return_type: VarType::Void
                    }
                ],
            })
        );
    }

    #[test]
    fn parse_struct_extra_constructor_args() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var x, y

                new(x, y, z, a: int)
            }");
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes.first().unwrap().node,
            ASTNodeKind::Statement(StatementKind::StructDeclaration {
                name: "Planet".to_string(),
                members: vec![("x".to_string(), VarType::Any, false), ("y".to_string(), VarType::Any, false)],
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "new".into(),
                        args_and_type: vec![
                            ("x".into(), VarType::Any),
                            ("y".into(), VarType::Any),
                            ("z".into(), VarType::Any),
                            ("a".into(), VarType::Int),
                        ],
                        body: vec![],
                        return_stmt: None,
                        return_type: VarType::Void
                    }
                ],
            })
        );
    }

    #[test]
    fn parse_struct_typed_members() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var x, y: real
                const z: []

                new(z)
            }");
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes.first().unwrap().node,
            ASTNodeKind::Statement(StatementKind::StructDeclaration {
                name: "Planet".to_string(),
                members: vec![
                    ("x".to_string(), VarType::Real, false),
                    ("y".to_string(), VarType::Real, false),
                    ("z".to_string(), VarType::Array(Box::new(VarType::Any)), true),
                ],
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "new".into(),
                        args_and_type: vec![
                            ("z".into(), VarType::Any),
                        ],
                        body: vec![],
                        return_stmt: None,
                        return_type: VarType::Void
                    }
                ],
            })
        );
    }

    #[test]
    fn parse_struct_constructor_body() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var x, y: real
                const z: []

                new(z) {
                    self.x = z[0]
                    self.y = z[1]
                }
            }");
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes.first().unwrap().node,
            ASTNodeKind::Statement(StatementKind::StructDeclaration {
                name: "Planet".to_string(),
                members: vec![
                    ("x".to_string(), VarType::Real, false),
                    ("y".to_string(), VarType::Real, false),
                    ("z".to_string(), VarType::Array(Box::new(VarType::Any)), true),
                ],
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "new".into(),
                        args_and_type: vec![
                            ("z".into(), VarType::Any),
                        ],
                        body: vec![
                            ExpressionKind::VarAssignment { 
                                assigne: Box::new(ExpressionKind::MemberCall {
                                    member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                                    property: Box::new(ExpressionKind::Identifier { symbol: "x".into() })
                                }),
                                value: Box::new(ExpressionKind::ArrayCall {
                                    name: "z".into(),
                                    index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 0 }))
                                })
                            }.into(),
                            ExpressionKind::VarAssignment { 
                                assigne: Box::new(ExpressionKind::MemberCall {
                                    member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                                    property: Box::new(ExpressionKind::Identifier { symbol: "y".into() })
                                }),
                                value: Box::new(ExpressionKind::ArrayCall {
                                    name: "z".into(),
                                    index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 1 }))
                                })
                            }.into()
                        ],
                        return_stmt: None,
                        return_type: VarType::Void
                    }
                ],
            })
        );
    }

    #[test]
    fn parse_struct_fn() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "struct Planet {
                var x: real

                new() {
                    self.x = 1
                }

                fn get_x(a: [], b) -> real {
                    var foo: int = 5
                    return self.x
                }
            }");
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes.first().unwrap().node,
            ASTNodeKind::Statement(StatementKind::StructDeclaration {
                name: "Planet".to_string(),
                members: vec![
                    ("x".to_string(), VarType::Real, false)
                ],
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "new".into(),
                        args_and_type: vec![],
                        body: vec![
                            ExpressionKind::VarAssignment { 
                                assigne: Box::new(ExpressionKind::MemberCall {
                                    member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                                    property: Box::new(ExpressionKind::Identifier { symbol: "x".into() })
                                }),
                                value: Box::new(ExpressionKind::IntLiteral { value: 1 })
                            }.into()
                        ],
                        return_stmt: None,
                        return_type: VarType::Void
                    },
                    StatementKind::FnDeclaration {
                        name: "get_x".into(),
                        args_and_type: vec![
                            ("a".into(), VarType::Array(Box::new(VarType::Any))),
                            ("b".into(), VarType::Any)
                        ],
                        body: vec![
                            StatementKind::VarDeclaration {
                                name: "foo".into(),
                                value: ExpressionKind::IntLiteral { value: 5 },
                                constant: false,
                                var_type: VarType::Int
                            }.into()
                        ],
                        return_stmt: Some(ExpressionKind::MemberCall {
                            member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                            property: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        }),
                        return_type: VarType::Real
                    }
                ],
            })
        );
    }
}
