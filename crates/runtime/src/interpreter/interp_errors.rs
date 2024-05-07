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
    
    #[error("{} during structure member access: structure -{0}- dosen't have member -{1}-", "Error".red().bold())]
    WrongStructMember(String, String),

    // Functions
    #[error("{} during function evaluation: {0}", "Error".red().bold())]
    FunctionEvaluation(String),

    #[error("{} during '{0}' function declaration: {1}", "Error".red().bold())]
    FunctionDeclaration(String, String),

    #[error("{} during '{0}' function evaluation: expected {1} argument but found {2}", "Error".red().bold())]
    WrongArgNumberFnCall(String, usize, usize),

    #[error("{} during '{0}' function evaluation: identifier is not a function", "Error".red().bold())]
    NonFunctionCall(String),

    // Arrays
    #[error("{} indexing non array variable.", "Error".red().bold())]
    NonArrayIndexing,

    #[error("{} during array creation: all elements must be of same type.", "Error".red().bold())]
    ArrayElemDiffType,

    #[error("{0}")]
    ArrayGetFnCall(#[from] ValueError),

    #[error("{0}")]
    InterpFromEnv(#[from] EnvError),

    #[error("{} during array assignment -{0}-, slice indexing is not allowed in assignment, only in expressions", "Error".red().bold())]
    ArraySliceAssign(String),
    
    #[error("{} during method call on array, non existing method call", "Error".red().bold())]
    ArrayNonMethodCall,

    // Types
    #[error("{} argument: -{0}-, found wrong type: -{1}-.", "Error".red().bold())]
    FnArgWrongType(String, String),
}

// Implement global trait for final error
impl ReportCodeErr for InterpreterError {}
