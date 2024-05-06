use colored::*;
use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum ValueError {
    // Operations
    #[error("{} while computing value. Operator unknown: {0}", "Error".red().bold())]
    UndefinedOperator(String),

    #[error("{} while computing value. Uninitialized variable in operation", "Error".red().bold())]
    UninitVarInOp,

    #[error("{} while computing value. Binary operation non accepted on functions or structure.", "Error".red().bold())]
    BinopOnFunction,

    #[error("{} while computing value. Can't resolve an operation with a bool as variable.", "Error".red().bold())]
    BoolInOperation,

    // Structures
    #[error("{} while assigning value to sub member. Only structures have sub members.", "Error".red().bold())]
    NonStructSubMemberAssign,

    #[error("{} while fetching sub member: -{0}-: can't find member.", "Error".red().bold())]
    SubMemberNotFound(String),

    #[error("{} during member call: variable must be a structure", "Error".red().bold())]
    NonStructVarMemberCall,

    #[error("{} during member -{0}- assignment: member is constant", "Error".red().bold())]
    ConstSubMemberAssignment(String),

    #[error("{} during member -{0}- assignment: {1}", "Error".red().bold())]
    SubMemAssignWrongType(String, String),

    // Types
    #[error("function -{0}- dosen't exist on type -{1}-.")]
    NonExistentFnOnType(String, String),

    #[error("expected -{0}- arguments, found: -{1}-.")]
    WrongArgsSize(usize, usize),

    #[error("to call '{0}' function on '{1}' type: -{2}-.")]
    WrongArgsSizeContext(String, String, String),

    #[error("argument -{0}- was not found.")]
    MissingKwArgInFunc(String),
    
    #[error("arguments number -{0}- of function: -{1}- should be of type: -{2}-, found -{3}-.")]
    WrongArgType(i64, String, String, String),
    
    #[error("{}: trying to cast type -{0}- to type -{1}-, operation not allowed.", "Error".red().bold())]
    NotAllowedCast(String, String),

    // Arrays
    #[error("both of array slice indexes must be positive, found: {0}.")]
    ArrSliceIdxNeg(i64),

    #[error("{}: index {0} is out of bound of array which is of size: -{1}-", "Error".red().bold())]
    ArrayOverIndexing(isize, usize),

    #[error("can't pop last element of empty array")]
    EmptyArrayPop,
}
