use colored::*;
use thiserror::Error;

use tools::errors::ReportCodeErr;
use crate::environment::EnvError;
use crate::values::ValueError;

#[derive(Error, Debug, PartialEq)]
pub enum InterpreterError {
    #[error("{} while evaluating expression: {0}", "Error".red().bold())]
    Evaluation(String),

    // Variables
    #[error("{} during variable declaration in: {0}", "Error".red().bold())]
    VarDeclaration(String),

    #[error("{} while assigning value to variable: {0}. Assigne must be a variable", "Error".red().bold())]
    NonLiteralAssigne(String),

    // Structures
    #[error("{} during structure declaration in: {0}", "Error".red().bold())]
    StructDeclaration(String),

    #[error("{} during structure creation, undeclared structure: {0}", "Error".red().bold())]
    UndeclaredStructCreation(String),

    #[error("{} during member call -{0}-: {1}", "Error".red().bold())]
    MemberCall(String, String),

    #[error("{} during structure -{0}- member call: {1}", "Error".red().bold())]
    WrongStructMemberCall(String, String),

    #[error("{} during member call: member has to be either a structure or an identifier", "Error".red().bold())]
    WrongStructMemberTypeCall,

    #[error("{} during structure creation -{0}-: {1}", "Error".red().bold())]
    StructCreation(String, String),

    #[error("{} during structure creation -{0}-: constructor expects no argument", "Error".red().bold())]
    StructEmptyConstructor(String),

    #[error("{} during structure creation -{0}-: constructor expects {1} arguments but {2} were given", "Error".red().bold())]
    StructConstructorWrongArgNb(String, usize, usize),

    #[error("{} during structure creation -{0}-: {1}", "Error".red().bold())]
    SelfInConstructor(String, String),

    // Functions
    #[error("{} during function evaluation: {0}", "Error".red().bold())]
    FunctionEvaluation(String),

    #[error("{} during '{0}' function declaration: {1}", "Error".red().bold())]
    FunctionDeclaration(String, String),

    #[error("{} during '{0}' function evaluation: expected {1} argument but found {2}", "Error".red().bold())]
    WrongArgNumberFnCall(String, usize, usize),

    #[error("{} during '{0}' function evaluation: identifier is not a function", "Error".red().bold())]
    NonFunctionCall(String),

    #[error("{} during '{0}' function evaluation: variable must be a structure or a built-in type withy", "Error".red().bold())]
    FnCallOnSimpleVar(String),

    // Arrays
    #[error("{} indexing non array variable.", "Error".red().bold())]
    NonArrayIndexing,

    #[error("{} index must be a number.", "Error".red().bold())]
    NonIntegerArrayIndex,

    #[error("{} during array creation: all elements must be of same type.", "Error".red().bold())]
    ArrayElemDiffType,

    #[error("{} during array access -{0}-: {1}", "Error".red().bold())]
    ArrayOverIndexing(String, String),

    #[error("{0}")]
    ArrayGetFnCall(#[from] ValueError),

    #[error("{0}")]
    InterpFromEnv(#[from] EnvError),

    #[error("{} while getting names to resolve array function call.", "Error".red().bold())]
    ArrayFnNameInterp,

    #[error("{} during array assignment -{0}- at index {1}: {2}", "Error".red().bold())]
    WrongArrayAssignment(String, usize, String),

    #[error("{} during array assignment -{0}-, slice indexing is not allowed in assignment, only in expressions", "Error".red().bold())]
    ArraySliceAssign(String),

    // Types
    #[error("{} argument: -{0}-, found wrong type: -{1}-.", "Error".red().bold())]
    FnArgWrongType(String, String),
}

// Implement global trait for final error
impl ReportCodeErr for InterpreterError {}
