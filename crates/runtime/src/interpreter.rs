mod expr;
mod interp_errors;
mod stmt;

use tools::errors::{CodeErr, ReportCodeErr};
use interp_errors::InterpreterError;

use super::{environment::Env, values::RuntimeVal};
use crate::frontend::ast::{ASTNode, ASTNodeKind};


pub struct Interpreter {}

impl Interpreter {
    pub fn execute_program(
        &self,
        nodes: Vec<ASTNode>,
        env: &mut Env,
    ) -> Result<RuntimeVal, CodeErr> {
        // Final result
        let mut result = Ok(RuntimeVal::Null);

        for n in nodes {
            result = Ok(self.interpret_node(n.node, env).map_err(|e| e.to_glob_err(n.line))?);
        }

        result
    }

    fn interpret_node(&self, node: ASTNodeKind, env: &mut Env) -> Result<RuntimeVal, InterpreterError> {
        match node {
            ASTNodeKind::Expression(expr) => Ok(self.evaluate(expr, env)?),
            ASTNodeKind::Statement(stmt) => Ok(self.resolve(stmt, env)?),
        }
    }
}
