use colored::*;
use thiserror::*;

use super::{Token, TokenKind};
use tools::errors::ReportCodeErr;

#[derive(Error, Debug, PartialEq)]
pub enum ParserError {
    // Token
    #[error("expected token: -{0:?}-, token found: -{1:?}-")]
    FoundWrongToken(TokenKind, TokenKind),

    #[error("Tryed to use non existant token")]
    EmptyTokenBufferUsed,

    #[error("Parsing for this token is not implemented: {0:?}")]
    UnrecognizedToken(Token),

    #[error("expected type annotation, found: {0}")]
    ExpectedType(String),

    // Variables declarations
    #[error("{} while parsing variable declaration: {0}
There are different ways to declare variables:
\tvar {}              -> declare a {} 'x' initialized to '{}'
\tvar {} = 5          -> decalre a {} 'x' with value '{}'
\tconst {} = 5        -> declare a constant {} 'x' with value 'any number'. Value can't be changed in the future.
\tvar x: type = value -> typed variable must be initialized with a value of the same type
\tconst x: type = val -> same but for constant
\tvarx, y: type = val -> define a type and a value for all the variables",
"Error".red().bold(), "x".cyan(), "variable".cyan(), "null".underline(), "x".cyan(), "variable".cyan(),
"any number".underline(), "x".cyan(), "variable".cyan())]
    ParseVarDecl(String),

    #[error("Expected variable name after 'var' and 'const' keyword, found: {0}.")]
    ExpectedVarNameAfterKw(String),

    #[error("Constant variable {0} must be initialized with a value.")]
    ConstVarNoVal(String),

    // Structures declaration
    #[error("{} while parsing structure declaration. Expected structure name after {} keyword.", "Error".red().bold(), "struct".cyan())]
    NoStructIdentifier,

    #[error("{} while parsing structure declaration. After {} and {} keywords, {0}.", "Error".red().bold(), "var".cyan(), "const".cyan())]
    NoIdentifierAfterMemberDecl(String),

    #[error("{} while parsing structure declaration. Non authorized keyword found: {0}.", "Error".red().bold())]
    NonAuthorizedKeywordInStruct(String),

    #[error("{} while parsing structure declaration. Expected '{{' after structure name for members declaration.", "Error".red().bold())]
    MissingStructOpenBrace,

    #[error("{} while parsing structure declaration. Expected '}}' after structure name declaration.", "Error".red().bold())]
    MissingStructCloseBrace,

    #[error("{} while parsing constructor: {0}", "Error".red().bold())]
    ConstructorError(String),

    #[error("{} while parsing structure declaration. Only one constructor declaration is allowed", "Error".red().bold())]
    MultipleConstructor,

    #[error("{} while parsing structure declaration. All variable must be identifier in the constructor, not expression", "Error".red().bold())]
    NonIdentifierVarNameInConstructor,

    #[error("{} while parsing structure declaration. All constant members must be declared in the constructor.", "Error".red().bold())]
    MissingConstMemberInConstructor,

    #[error("{} while parsing structure declaration. The constructor can't return a value.", "Error".red().bold())]
    ReturnInConstructor,

    #[error("{} while parsing structure creation: {0}", "Error".red().bold())]
    StructCreation(String),

    #[error("{} while parsing structure creation: expected structure name after 'new' keyword", "Error".red().bold())]
    MissingStructIdentifier,

    #[error("{} while parsing structure creation: the constructor can't return anything. Remove '-> <type>'", "Error".red().bold())]
    ConstructorWithType,

    #[error("{} while parsing structure creation: missing parenthesis after structure name.\n
The syntaxe are:
\tvar mars = new Planet()
\t         or
\tvar mars = new Planet(arg1, arg2, ...)
Even without a constructor, paranthesis are mandatory", "Error".red().bold())]
    MissingParenStructCreation,

    // Functions declaration
    #[error("{} while parsing function. Expected '(' after function name.", "Error".red().bold())]
    MissingFnOpenParen,

    #[error("{} while parsing function. Expected ')' after arguments list.", "Error".red().bold())]
    MissingFnCloseParen,

    #[error("{} while parsing function declaration: expected identifier after 'fn' keyword.", "Error".red().bold())]
    MissingIdentifierAfterFn,

    #[error("{} while parsing function arguments declaration: expected identifier after ','.", "Error".red().bold())]
    MissingIdentAfterComma,

    #[error("{} while parsing function arguments declaration: expected ',' between two arguments.", "Error".red().bold())]
    MissingCommaBetweenArgs,

    #[error("{} while parsing function declaration: arguments must be identifiers", "Error".red().bold())]
    FnDeclArgsNotString,

    #[error("{} while parsing function declaration: expected '{{' to start function body", "Error".red().bold())]
    MissingFnOpenBrace,

    #[error("{} while parsing function declaration: expected '}}' to end function body", "Error".red().bold())]
    MissingFnCloseBrace,

    #[error("{} while parsing function declaration: expected '}}' to end function body after 'return' keyword", "Error".red().bold())]
    StmtAfterReturn,
    
    #[error("{} while parsing function declaration. Return statement's type doesn't match the declared return type. \
Expected -{0}-, found -{1}-", "Error".red().bold())]
    WrongReturnType(String, String),

    #[error("{} while parsing member function call. This is not possible to call the constructor of a structure", "Error".red().bold())]
    ConstructorCall,

    // Member expressions
    #[error("{} while parsing member. Expected identifier after dot '.' operator.", "Error".red().bold())]
    MissingIdentifierAfterDot,

    #[error("{} while parsing member: {0}", "Error".red().bold())]
    MemberDeclartion(String),

    // Inline
    #[error("{} while parsing number. Only '-' is allowed to declare inline number: -1, -2.", "Error".red().bold())]
    WrongInLineOperator,

    // Typo errors
    #[error("{} while parsing line. {0}: {1} found outside expression", "Error".red().bold())]
    VarOutsideExpr(String, String),

    // Comma declaration
    #[error("{} while parsing multiple variable declaration: identifier must follow the ',' token found: {0}", "Error".red().bold())]
    MissingIdentCommaDecl(String),

    #[error("{} while parsing multiple variable declaration: values must be expressions.", "Error".red().bold())]
    NonExprCommaDecl,

    #[error("{} while parsing multiple variable declaration: at least one value must be provided after '=' token.", "Error".red().bold())]
    NoExprProvidedCommaDecl,

    #[error("{} while parsing multiple variable declaration: if more than one expression is provided after '=' token, \
    there must be as many as variable identifiers.", "Error".red().bold())]
    WrongExprNbCommaDecl,

    #[error("{} while parsing multiple variable declaration: constants must have a value at initialization. Syntaxes are:\
    \tconst x, y, z = 6
    \tconst x, y, z = 6, 20, 12", "Error".red().bold())]
    ConstCommaDeclNoValue,

    // Array slices
    #[error("{} while parsing array slice indexing: {0}
Syntaxes are:
\ta[0:10]  ->  get elements from index 0 to 10
\ta[:10]   ->  get elements from the first element to the 10th
\ta[5:]    ->  get elements from the 5th to the last", "Error".red().bold())]
    ArraySlice(String),

    #[error("expected ':' after first slice index.")]
    NoColonAfterFirstIndex,

    #[error("empty slice not allowed.")]
    EmptyArraySlice,

    #[error("if using array slice, at least one index must be given with the ':' token.")]
    NoSliceIdenxWithColon,

    // Test
    #[error("{} while parsing test declaration: missing test name after 'test' keyword: {0}", "Error".red().bold())]
    NoTestName(String),

    #[error("{} while parsing test declaration. To begin test declaration after test identifier: {0}", "Error".red().bold())]
    MissingOpenBrace(String),

    #[error("{} while parsing test declaration. To end test declaration: {0}", "Error".red().bold())]
    MissingCloseBrace(String),

    #[error("{} while parsing test declaration. No return statement allowed", "Error".red().bold())]
    ReturnInTest,
}

// Implement global trait for final error
impl ReportCodeErr for ParserError {}
