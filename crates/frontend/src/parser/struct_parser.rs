use super::errors_parser::ParserError;
use super::Parser;

use super::TokenKind;
use super::VarType;
use super::{ASTNodeKind, ExpressionKind, StatementKind};

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
        let mut constructor_args: Option<Vec<(String, VarType)>> = None;
        let mut constructor_body: Option<Vec<ASTNodeKind>> = None;
        let mut functions: Vec<StatementKind> = Vec::new();

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
                    // We eat the keyword
                    let _ = self
                        .eat()
                        .map_err(|e| ParserError::ConstructorError(format!("{e}")))?;

                    // If we already have a declared constructor
                    if constructor_args.is_some() {
                        return Err(ParserError::MultipleConstructor);
                    }

                    // We get all the var names as ExpressionKind
                    let args = self
                        .parse_fn_decl_args()
                        .map_err(|e| ParserError::ConstructorError(format!("{e}")))?;

                    // We eat the close paren
                    self.expect_token(TokenKind::CloseParen)?;

                    let args_name = args.iter().map(|a| &a.0).collect::<Vec<&String>>();

                    // We get all the constant members
                    let const_members = members
                        .iter()
                        .filter(|m| m.2)
                        .collect::<Vec<&(String, VarType, bool)>>();

                    // All the constant members must have been declared in the constructor
                    if !const_members.iter().all(|m| args_name.contains(&&m.0)) {
                        return Err(ParserError::MissingConstMemberInConstructor)
                    }

                    // We save the argument names
                    constructor_args = match args.is_empty() {
                        true => None,
                        false => Some(args)
                    };

                    // Now, we check if there is a body
                    self.skip_end_lines();

                    // We check if there is a return type '-> <type>'
                    if self.at().kind == TokenKind::SimpleArrow {
                        return Err(ParserError::ConstructorWithType)
                    }

                    if self.at().kind == TokenKind::OpenBrace {
                        let _ = self.eat()?;

                        // We get the constructor body and return statement
                        let (c, r) = self.parse_fn_body()?;

                        // If there is a return statement, error
                        if r.is_some() {
                            return Err(ParserError::ReturnInConstructor)
                        }

                        constructor_body = Some(c);

                        // End of constructor declaration, expect '}'
                        let _ = self
                            .expect_token(TokenKind::CloseBrace)
                            .map_err(|_| ParserError::MissingStructCloseBrace)?;
                    }
                }
                TokenKind::Fn => {
                    let function = self.parse_fn_declaration()?;

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

        // We check that is we have at least one constant member, there is a constructor declared
        if !members
            .iter()
            .filter(|m| m.2)
            .collect::<Vec<&(String, VarType, bool)>>()
            .is_empty()
            && constructor_args.is_none()
        {
            return Err(ParserError::MissingConstMemberInConstructor);
        }

        Ok(StatementKind::StructDeclaration {
            name: identifier.value,
            members,
            constructor_args,
            constructor_body,
            functions,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::ArrayIndexing, lexer::Lexer};

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
                constructor_args: Some(vec![("z".to_string(), VarType::Any)]),
                constructor_body: None,
                functions: Vec::new(),
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
                constructor_args: Some(vec![("x".to_string(), VarType::Any), ("y".to_string(), VarType::Any)]),
                constructor_body: None,
                functions: Vec::new(),
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
                constructor_args: Some(vec![
                    ("x".to_string(), VarType::Any),
                    ("y".to_string(), VarType::Any),
                    ("z".to_string(), VarType::Any),
                    ("a".to_string(), VarType::Int)
                ]),
                constructor_body: None,
                functions: Vec::new(),
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
                constructor_args: Some(vec![
                    ("z".to_string(), VarType::Any)
                ]),
                constructor_body: None,
                functions: Vec::new(),
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
                constructor_args: Some(vec![
                    ("z".to_string(), VarType::Any)
                ]),
                constructor_body: Some(vec![
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment { 
                        assigne: Box::new(ExpressionKind::MemberCall {
                            member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                            property: Box::new(ExpressionKind::Identifier { symbol: "x".into() })
                        }),
                        value: Box::new(ExpressionKind::ArrayCall {
                            name: "z".into(),
                            index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 0 }))
                        })
                    }),
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment { 
                        assigne: Box::new(ExpressionKind::MemberCall {
                            member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                            property: Box::new(ExpressionKind::Identifier { symbol: "y".into() })
                        }),
                        value: Box::new(ExpressionKind::ArrayCall {
                            name: "z".into(),
                            index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 1 }))
                        })
                    })
                ]),
                functions: Vec::new(),
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
                constructor_args: None,
                constructor_body: Some(vec![
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment { 
                        assigne: Box::new(ExpressionKind::MemberCall {
                            member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                            property: Box::new(ExpressionKind::Identifier { symbol: "x".into() })
                        }),
                        value: Box::new(ExpressionKind::IntLiteral { value: 1 })
                    })
                ]),
                functions: vec![
                    StatementKind::FnDeclaration {
                        name: "get_x".into(),
                        args_and_type: vec![
                            ("a".into(), VarType::Array(Box::new(VarType::Any))),
                            ("b".into(), VarType::Any)
                        ],
                        body: vec![
                            ASTNodeKind::Statement(StatementKind::VarDeclaration {
                                name: "foo".into(),
                                value: ExpressionKind::IntLiteral { value: 5 },
                                constant: false,
                                var_type: VarType::Int
                            })
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
