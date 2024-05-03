pub mod interpreter;
mod native_functions;
pub mod environment;
pub mod values;

extern crate tools;
extern crate frontend;

pub use frontend::{
    parser::VarType,
    ast::{ASTNodeKind, ExpressionKind}
};
