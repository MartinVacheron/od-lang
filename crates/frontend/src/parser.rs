use std::collections::VecDeque;
use std::fmt::Display;

mod array_parser;
mod errors_parser;
mod fn_parser;
mod struct_parser;
mod test_parser;
mod var_parser;

pub use crate::ast::{ASTNode, ASTNodeKind, ExpressionKind, StatementKind};
pub use crate::lexer::{Token, TokenKind};

use tools::errors::{CodeErr, ReportCodeErr};

use self::errors_parser::ParserError;


#[derive(Debug, PartialEq, Clone)]
pub enum VarType {
    Any,
    Int,
    Real,
    Bool,
    Array(Box<VarType>),
    Struct(String),
    Func,
    Void
}

// TODO: Remove panic
impl VarType {
    // Default value for basic types. Null, fn and structure are themselves
    // their default value
    pub fn get_default_value(&self) -> ExpressionKind {
        match self {
            VarType::Any => ExpressionKind::Identifier { symbol: "null".into() },
            VarType::Int => ExpressionKind::IntLiteral { value: 0 },
            VarType::Real => ExpressionKind::RealLiteral { value: 0. },
            VarType::Bool => ExpressionKind::Identifier { symbol: "false".into() } ,
            VarType::Array(_) => ExpressionKind::ArrayLiteral { values: vec![] },
            VarType::Struct(name) => ExpressionKind::EmptyStructLiteral { name: name.into() },
            VarType::Func => panic!("Error, cannot have type here"),
            VarType::Void => panic!("Error, cannot have type for void"),
        }
    }
}

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Any => write!(f, "any"),
            VarType::Int => write!(f, "int"),
            VarType::Real => write!(f, "real"),
            VarType::Bool => write!(f, "bool"),
            VarType::Array(u) => write!(f, "[]{}", *u),
            VarType::Struct(name) =>  write!(f, "{}", name),
            VarType::Func =>  write!(f, "function"),
            VarType::Void =>  write!(f, "void"),
        }
    }
}


#[derive(Default)]
pub struct Parser {
    tokens: VecDeque<Token>,
    pub ast_nodes: Vec<ASTNode>,
}

impl Parser {
    pub fn build_ast(&mut self, tokens: VecDeque<Token>) -> Result<(), CodeErr> {
        self.ast_nodes.clear();
        self.tokens = tokens;

        while !self.is_eof() {
            // We parse the statements
            let stmt = self
                .parse_statement()
                .map_err(|e| e.to_glob_err(self.at().line))?;

            if let Some(s) = stmt {
                // We check for some rules
                self.check_current_line_nodes(&s)
                    .map_err(|e| e.to_glob_err(self.at().line))?;

                // If ok, we push it with the line
                self.ast_nodes.push(ASTNode::new(s, self.at().line));
            }
        }

        Ok(())
    }

    // Checks some rules to prevent weird syntaxe to be parsed
    fn check_current_line_nodes(&self, cur_node: &ASTNodeKind) -> Result<(), ParserError> {
        // Current node can't be a number, identifier or array literal alone
        // otherwise this is correct: var a = 5 5  or  hello() [9] or  g fn hello() {}
        match cur_node {
            ASTNodeKind::Expression(ExpressionKind::IntLiteral { value, .. }) => Err(
                ParserError::VarOutsideExpr("Numeric literal".into(), value.to_string()),
            ),
            ASTNodeKind::Expression(ExpressionKind::RealLiteral { value, .. }) => Err(
                ParserError::VarOutsideExpr("Numeric literal".into(), value.to_string()),
            ),
            ASTNodeKind::Expression(ExpressionKind::Identifier { symbol, .. }) => Err(
                ParserError::VarOutsideExpr("Identifier".into(), symbol.to_string()),
            ),
            ASTNodeKind::Expression(ExpressionKind::ArrayLiteral { .. }) => Err(
                ParserError::VarOutsideExpr("Array literal".into(), "array".into()),
            ),
            _ => Ok(()),
        }
    }

    // Entry point of parsing
    fn parse_statement(&mut self) -> Result<Option<ASTNodeKind>, ParserError> {
        match self.at().kind {
            // New lines are only used at end of statement (var declaration, ...)
            // We treat them only when in statements
            TokenKind::EndLine => {
                self.eat()?;
                Ok(None)
            }
            TokenKind::Var | TokenKind::Const => {
                Ok(Some(ASTNodeKind::from(self.parse_var_declaration()?)))
            }
            TokenKind::Structure => Ok(Some(ASTNodeKind::from(self.parse_struct_declaration()?))),
            TokenKind::Fn => Ok(Some(ASTNodeKind::from(self.parse_fn_declaration(false)?))),
            TokenKind::Test => Ok(Some(ASTNodeKind::from(self.parse_test_declaration()?))),
            _ => Ok(Some(ASTNodeKind::from(self.parse_assignment_expr()?))),
        }
    }

    fn parse_assignment_expr(&mut self) -> Result<ExpressionKind, ParserError> {
        // We get the identifier / member call
        let left = self.parse_additive_expr()?;

        match self.at().kind {
            // Allows syntax like x = y = z
            TokenKind::Equals => {
                self.eat()?;
                let right = self.parse_assignment_expr()?;

                Ok(ExpressionKind::VarAssignment {
                    assigne: Box::new(left),
                    value: Box::new(right),
                })
            }
            // Allows syntax like a += 2
            TokenKind::CompoundAssign => {
                let ope = self.eat()?;
                // We parse as usual the assignment expression
                let right = self.parse_assignment_expr()?;
                // We add a layer of operation to add the identifier in the expression
                let new_value = ExpressionKind::BinaryOp {
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    operator: ope.value,
                };

                Ok(ExpressionKind::VarAssignment {
                    assigne: Box::new(left),
                    value: Box::new(new_value),
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_additive_expr(&mut self) -> Result<ExpressionKind, ParserError> {
        let mut left = self.parse_multiplicative_expr()?;

        // We skip in case of compound
        while (self.at().value == "+" || self.at().value == "-")
            && self.at().kind != TokenKind::CompoundAssign
        {
            let operator = self.eat()?.value;
            let right = self.parse_multiplicative_expr()?;

            left = ExpressionKind::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            };
        }

        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<ExpressionKind, ParserError> {
        let mut left = self.parse_member_expression()?;

        // We skip in case of compound
        while (self.at().value == "*" || self.at().value == "/" || self.at().value == "%")
            && self.at().kind != TokenKind::CompoundAssign
        {
            let operator = self.eat()?.value;
            let right = self.parse_member_expression()?;

            left = ExpressionKind::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            };
        }

        Ok(left)
    }

    // Parse recursivly statements like: space_obj.planet.position.x
    fn parse_member_expression(&mut self) -> Result<ExpressionKind, ParserError> {
        // First, we get space_obj
        let mut member = self
            .parse_primary_expr()
            .map_err(|e| ParserError::MemberDeclartion(format!("{e}")))?;

        // Manage array and function call in between the member call chain
        if self.at().kind == TokenKind::OpenParen {
            member = self.parse_function_call(member)?;
        } else if self.at().kind == TokenKind::OpenBracket {
            // We eat the bracket
            self.eat()?;
            member = self.parse_array_call(member)?;
        }

        while self.at().kind == TokenKind::Dot {
            // We eat the dot
            let _ = self.eat()?;

            // First step, we get the property planet
            // We get the property position
            let mut property = self.parse_primary_expr()?;

            // If it's not an identifier, error
            if let ExpressionKind::Identifier { .. } = property {
            } else {
                return Err(ParserError::MissingIdentifierAfterDot);
            }

            // Manage array and function call in between the member call chain
            if self.at().kind == TokenKind::OpenParen {
                property = self.parse_function_call(property)?;
            } else if self.at().kind == TokenKind::OpenBracket {
                // We eat the bracket
                self.eat()?;
                property = self.parse_array_call(property)?;
            }

            // Now member is space_obj.planet
            // Second, member is space_obj.planet.position
            member = ExpressionKind::MemberCall {
                member: Box::new(member),
                property: Box::new(property),
            }
        }

        Ok(member)
    }

    fn parse_primary_expr(&mut self) -> Result<ExpressionKind, ParserError> {
        match self.at().kind {
            // Numbers
            TokenKind::Number => Ok(self.parse_literal_number_expr(false)?),
            // Manages negative numbers
            TokenKind::BinaryOperator => {
                if self.at().value == String::from('-') {
                    // We get rid of the minus
                    self.eat()?;
                    
                    // We return '-' the value
                    Ok(self.parse_literal_number_expr(true)?)
                } else {
                    Err(ParserError::WrongInLineOperator)
                }
            }
            // We treat 'self' as an identifier
            TokenKind::Identifier | TokenKind::SelfKw => Ok(ExpressionKind::Identifier {
                symbol: self.eat()?.value,
            }),
            // Manages the beginning of paranthesis var a = 5 + (...
            TokenKind::OpenParen => {
                let _ = self.eat()?;
                // We get the expression
                let expr = self.parse_additive_expr()?;
                // End with a parenthesis
                let _ = self.expect_token(TokenKind::CloseParen)?;

                Ok(expr)
            }
            // Array declaration
            TokenKind::OpenBracket => {
                // We eat the '['
                self.eat()?;
                self.parse_array_declaration()
            }
            // Manages wrong call
            TokenKind::New => Err(ParserError::ConstructorCall),
            _ => Err(ParserError::UnrecognizedToken(self.at().clone())),
        }
    }

    fn parse_literal_number_expr(&mut self, negative: bool) -> Result<ExpressionKind, ParserError> {
        let number = self.eat()?;

        if number.value.contains(".") {
            let neg = if negative { -1f64 } else { 1f64 };
            Ok(ExpressionKind::RealLiteral { value: number.value.parse::<f64>().unwrap() * neg })
        } else {
            let neg = if negative { -1i64 } else { 1i64 };
            Ok(ExpressionKind::IntLiteral { value: number.value.parse::<i64>().unwrap() * neg})
        }
    }

    fn at(&self) -> &Token {
        self.tokens.front().unwrap()
    }

    fn eat(&mut self) -> Result<Token, ParserError> {
        match self.tokens.pop_front() {
            Some(tk) => Ok(tk),
            None => Err(ParserError::EmptyTokenBufferUsed),
        }
    }

    fn expect_token(&mut self, token_kind: TokenKind) -> Result<Token, ParserError> {
        let tk = self.eat()?;

        if tk.kind != token_kind {
            return Err(ParserError::FoundWrongToken(token_kind, tk.kind));
        }

        Ok(tk)
    }

    // Is end of file
    fn is_eof(&self) -> bool {
        self.at().kind == TokenKind::EOF
    }

    // Check if there is a type after a specific token.
    // Can be used for:  x: int with ':' token  or  fn(a, b) -> int  with '->' token
    fn parse_type_after_token(&mut self, token: TokenKind) -> Result<Option<VarType>, ParserError> {
        if self.at().kind == token {
            // If there is
            self.eat()?;

            // And we are at a type definition, we get it
            let var_type = self.parse_type()?;

            // We eat the type if we didnt get an array
            // Array type parsing manages itself
            if !matches!(var_type, VarType::Array(..)) {
                self.eat()?;
            }

            Ok(Some(var_type))
        } else {
            // If not, any type
            Ok(None)
        }
    }

    fn parse_type(&mut self) -> Result<VarType, ParserError> {
        let var_type = match self.at().kind {
            TokenKind::Any => VarType::Any,
            TokenKind::Int => VarType::Int,
            TokenKind::Real => VarType::Real,
            TokenKind::Bool => VarType::Bool,
            TokenKind::Void => VarType::Void,
            TokenKind::Identifier => VarType::Struct(self.at().value.clone()),
            TokenKind::OpenBracket => self.parse_array_type()?,
            _ => return Err(ParserError::ExpectedType(self.at().value.clone()))
        };

        Ok(var_type)
    }

    // Parse syntaxes like:   [], []int, []real, ...
    // TODO: Recursive parsing for example: [][][]int
    fn parse_array_type(&mut self) -> Result<VarType, ParserError> {
        // We eat the opening '['
        self.expect_token(TokenKind::OpenBracket)?;
        self.expect_token(TokenKind::CloseBracket).map_err(|_| ParserError::ArrayTypeNoCloseBracket)?;

        let underlying = match self.parse_type() {
            Ok(t) => { self.eat()?; t },
            Err(_) => VarType::Any
        };
        
        Ok(VarType::Array(Box::new(underlying)))
    }

    fn skip_end_lines(&mut self) {
        while self.at().kind == TokenKind::EndLine {
            let _ = self.eat();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn parse_nested_binop() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from("1 - 2 + 3");
        let _ = lexer.tokenize(code);

        let _ = parser.build_ast(lexer.tokens);

        assert_eq!(
            parser.ast_nodes,
            vec![ASTNode::new(
                ASTNodeKind::Expression(ExpressionKind::BinaryOp {
                    left: Box::new(ExpressionKind::BinaryOp {
                        left: Box::new(ExpressionKind::IntLiteral { value: 1i64 }),
                        right: Box::new(ExpressionKind::IntLiteral { value: 2i64 }),
                        operator: '-'.to_string(),
                    }),
                    right: Box::new(ExpressionKind::IntLiteral { value: 3i64 }),
                    operator: '+'.to_string(),
                }),
                0
            )]
        );
    }

    #[test]
    fn parse_multiplication_precedence() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from("1 * 2 + 3");
        let _ = lexer.tokenize(code);

        let _ = parser.build_ast(lexer.tokens.clone());

        assert_eq!(
            parser.ast_nodes,
            vec![ASTNode::new(
                ASTNodeKind::Expression(ExpressionKind::BinaryOp {
                    left: Box::new(ExpressionKind::BinaryOp {
                        left: Box::new(ExpressionKind::IntLiteral { value: 1 }),
                        right: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: '*'.to_string(),
                    }),
                    right: Box::new(ExpressionKind::IntLiteral { value: 3 }),
                    operator: '+'.to_string(),
                }),
                0
            )]
        );

        let code = String::from("1 + 2 * 3");
        let _ = lexer.tokenize(code);

        let _ = parser.build_ast(lexer.tokens);

        assert_eq!(
            parser.ast_nodes,
            vec![ASTNode::new(
                    ASTNodeKind::Expression(ExpressionKind::BinaryOp {
                    left: Box::new(ExpressionKind::IntLiteral { value: 1 }),
                    right: Box::new(ExpressionKind::BinaryOp {
                        right: Box::new(ExpressionKind::IntLiteral { value: 3 }),
                        left: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: '*'.to_string()
                    }),
                    operator: '+'.to_string(),
                }),
                0
        )]
        );
    }

    #[test]
    fn parse_compound_assign() {
        let mut lexer: Lexer = Default::default();
        let mut parser: Parser = Default::default();

        let code = String::from(
            "var x = 2
             x += 2
             x -= 2
             x *= 2
             x /= 2
             x %= 2",
        );
        lexer.tokenize(code).unwrap();
        parser.build_ast(lexer.tokens).unwrap();

        assert_eq!(
            parser.ast_nodes,
            vec![
                ASTNode::new(
                    ASTNodeKind::Statement(StatementKind::VarDeclaration {
                        name: "x".into(),
                        value: ExpressionKind::IntLiteral { value: 2 },
                        constant: false,
                        var_type: VarType::Int
                    }),
                    0
                ),
                ASTNode::new(
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment {
                        assigne: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        value: Box::new(ExpressionKind::BinaryOp {
                            left: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                            right: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: "+".into()
                        })
                    }),
                    1
                ),
                ASTNode::new(
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment {
                        assigne: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        value: Box::new(ExpressionKind::BinaryOp {
                            left: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                            right: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: "-".into()
                        })
                    }),
                    2
                ),
                ASTNode::new(
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment {
                        assigne: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        value: Box::new(ExpressionKind::BinaryOp {
                            left: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                            right: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: "*".into()
                        })
                    }),
                    3
                ),
                ASTNode::new(
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment {
                        assigne: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        value: Box::new(ExpressionKind::BinaryOp {
                            left: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                            right: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: "/".into()
                        })
                    }),
                    4
                ),
                ASTNode::new(
                    ASTNodeKind::Expression(ExpressionKind::VarAssignment {
                        assigne: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        value: Box::new(ExpressionKind::BinaryOp {
                            left: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                            right: Box::new(ExpressionKind::IntLiteral { value: 2 }),
                        operator: "%".into()
                        })
                    }),
                    5
                )
            ]
        );
    }
}
