use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::InterpreterError;
use crate::frontend::ast::{ASTNodeKind, ArrayIndexing, ExpressionKind};
use crate::{
    environment::Env,
    values::{AssignType, RuntimeVal},
};
use frontend::parser::VarType;
use super::Interpreter;

impl Interpreter {
    pub(super) fn evaluate(&self, expr: ExpressionKind, env: &mut Env) -> Result<RuntimeVal, InterpreterError> {
        match expr {
            ExpressionKind::IntLiteral { value } => {
                Ok(RuntimeVal::Int(value))
            },
            ExpressionKind::RealLiteral { value } => {
                Ok(RuntimeVal::Real(value))
            },
            ExpressionKind::Identifier { symbol } => Ok(env
                .lookup_var(&symbol)
                .map_err(|e| InterpreterError::Evaluation(format!("{e}")))?
                .clone()),
            ExpressionKind::ArrayLiteral { values } => {
                let mut val: Vec<RuntimeVal> = vec![];
                // We evaluate all the values
                for v in values {
                    // val.push(v.evaluate(env)?);
                    val.push(self.evaluate(v, env)?);
                }

                // We check for their type if there is at least 2 elems
                if val.len() >= 2 {
                    let first_type = val.first().unwrap().get_type();

                    match val.iter().all(|v| v.get_type() == first_type) {
                        true => {},
                        false => return Err(InterpreterError::ArrayElemDiffType),
                    }
                }

                Ok(RuntimeVal::new_array(val))
            }
            ExpressionKind::BinaryOp {
                left,
                right,
                operator,
            } => {
                let lhs = self.evaluate(*left, env)?;
                let rhs = self.evaluate(*right, env)?;

                Ok(lhs
                    .calculate(rhs, &operator)
                    .map_err(|e| InterpreterError::Evaluation(format!("{e}")))?)
            }
            // TODO: Test
            ExpressionKind::VarAssignment { assigne, value } => {
                let assignment_value = self.evaluate(*value, env)?;

                match *assigne {
                    ExpressionKind::Identifier { symbol } => {
                        // We clone it to return the value too, in case of x = y = z = 5 for ex
                        env.assign_var(&symbol, AssignType::Replace(assignment_value.clone()))?;

                        Ok(assignment_value)
                    }
                    ExpressionKind::MemberCall { member, property } => {
                        let props = parse_mem_call(&*member, &*property)?;
                        
                        // Cloning for same reason than above
                        env.assign_struct_var(props, AssignType::Replace(assignment_value.clone()))?;

                        Ok(assignment_value)
                    }
                    ExpressionKind::ArrayCall { member, index } => {
                        // We get the name for errors
                        let (_, arr_name) = member.get_first_and_last();

                        // Cannot have empty array name
                        if arr_name.is_none() {
                            return Err(InterpreterError::NonArrayIndexing);
                        }

                        // All cases
                        match index {
                            // Single index is the only supported assignment
                            ArrayIndexing::Single(i) => {
                                // Evaluate the index
                                let id = self.evaluate(*i, env)?;

                                match *member {
                                    ExpressionKind::Identifier { symbol } => {
                                        env.assign_var(
                                            &symbol,
                                            AssignType::ArrayModification {
                                                index: id,
                                                value: assignment_value,
                                            },
                                        )?;
                                    }
                                    ExpressionKind::MemberCall { member, property } => {
                                        let props = parse_mem_call(&*member, &*property)?;

                                        env.assign_struct_var(
                                            props,
                                            AssignType::ArrayModification {
                                                index: id,
                                                value: assignment_value,
                                            },
                                        )?;
                                    }
                                    // It's impossible to get anything else
                                    _ => return Err(InterpreterError::NonArrayIndexing),
                                }
                            }
                            // Else, error
                            ArrayIndexing::Slice { .. } => {
                                return Err(InterpreterError::ArraySliceAssign(arr_name.unwrap()))
                            }
                        }

                        Ok(RuntimeVal::Null)
                    }
                    _ => Err(InterpreterError::NonLiteralAssigne(format!(
                        "{:?}",
                        assigne
                    ))),
                }
            }
            ExpressionKind::MemberCall {
                member: m1,
                property,
            } => {
                let mut props = parse_mem_call(&*m1, &*property)?;

                // We get the struct variable
                let structure = env
                    .lookup_var(props.first().unwrap())
                    .map_err(|e| InterpreterError::MemberCall(props.remove(0), e.to_string()))?;

                // We remove the struct name
                let struct_name = props.remove(0);

                // We get the value of the member call
                Ok(structure.get_sub_member(&mut props).map_err(|e| {
                    InterpreterError::WrongStructMemberCall(struct_name, format!("{e}"))
                })?)
            }
            ExpressionKind::FunctionCall {
                caller,
                args: args_in,
            } => {
                // We extract the name of the variable and the function.
                let (var_name, fn_name) = caller.get_first_and_last();

                // We evaluate each of the argument
                let mut args: Vec<RuntimeVal> = vec![];
                for arg in args_in {
                    args.push(self.evaluate(arg, env)?);
                }

                // Temporary env of execution
                let mut tmp_env = Env::new(Some(env));

                // Update env if sub member call
                if let ExpressionKind::MemberCall { member, property } = &*caller {
                    // We parse the members chain
                    let mut members_chain = parse_mem_call(&*member, &*property)?;

                    let fn_name = fn_name.as_ref().expect("Error getting function name");
                    let var_name = var_name.as_ref().expect("Error getting variable name");

                    // We get the variable
                    let var = env.lookup_var(&var_name)?;

                    if let RuntimeVal::Structure { prototype, members } = var {
                        // We remove function name in members chain list
                        let _ = members_chain.pop();

                        // If after remove fn name we have only one element
                        if members_chain.len() == 1 {
                            // We create the self structure
                            tmp_env
                                .create_self(prototype.clone(), members.clone())
                                .map_err(|e| {
                                    InterpreterError::SelfInConstructor(
                                        fn_name.clone(),
                                        e.to_string(),
                                    )
                                })?;
                        } else {
                            // We remove the structure name
                            let _ = members_chain.remove(0);
                            // We get the member resolved
                            let sub_mem = var.get_sub_member(&mut members_chain)?;

                            // Last cases
                            if let RuntimeVal::Structure { prototype: p2, members: m2 } = sub_mem {
                                // We create the self structure
                                tmp_env.create_self(p2.clone(), m2.clone()).map_err(
                                    |e| {
                                        InterpreterError::SelfInConstructor(
                                            fn_name.clone(),
                                            e.to_string(),
                                        )
                                    },
                                )?;
                            }
                        }
                    }
                }

                let func = self.evaluate(*caller, &mut tmp_env)?;

                match func {
                    // Native functions
                    RuntimeVal::NativeFunction { func, return_type } => {
                        // We execute it
                        func(&args).map_err(|e| {
                            InterpreterError::FunctionEvaluation(e.to_string())
                        })
                    }
                    // User defined functions
                    RuntimeVal::Function {
                        args_and_type,
                        body,
                        return_stmt,
                        return_type
                    } => {
                        let fn_name = fn_name.expect("Error getting function name");

                        match self.execute_fn_in_env(
                            args_and_type,
                            args,
                            fn_name,
                            body,
                            return_stmt,
                            &mut tmp_env,
                        )? {
                            Some(res) => Ok(res),
                            None => Ok(RuntimeVal::Null),
                        }
                    }
                    RuntimeVal::Array(_) => {
                        let fn_name = fn_name.expect("Error getting function name");
                        let var_name = var_name.expect("Error getting variable name");

                        env.assign_var(
                            &var_name,
                            AssignType::FunctionCall { fn_name, args },
                        )?;

                        Ok(RuntimeVal::Null)
                    }
                    _ => Err(InterpreterError::NonFunctionCall(fn_name.expect("Error getting function name"))),
                }
            }
            // Array calls are: arr[0], arr[:x], ...
            ExpressionKind::ArrayCall { member, index } => {
                // We evaluate array variable and index
                let mem = self.evaluate(*member, env)?;

                // All cases
                match mem {
                    RuntimeVal::Array(mut arr) => {
                        // Different indexing syntaxes
                        match index {
                            // Indexing like: arr[0], arr[-1], ..
                            ArrayIndexing::Single(i) => {
                                let id = self.evaluate(*i, env)?;

                                Ok(arr
                                    .call("get", &vec![id])
                                    .map_err(|e| InterpreterError::ArrayGetFnCall(e))?)
                            }
                            // Indexing like: arr[:x], arr[2:], arr[2:4]
                            ArrayIndexing::Slice { start, end } => {
                                let s = match start {
                                    Some(expr) => self.evaluate(*expr, env)?,
                                    _ => RuntimeVal::Null,
                                };
                                let e = match end {
                                    Some(expr) => self.evaluate(*expr, env)?,
                                    _ => RuntimeVal::Null,
                                };

                                Ok(arr
                                    .call("get_slice", &vec![s, e])
                                    .map_err(|e| InterpreterError::ArrayGetFnCall(e))?)
                            }
                        }
                    }
                    // Not an array
                    _ => Err(InterpreterError::NonArrayIndexing),
                }
            }
            ExpressionKind::EmptyStructLiteral { name } => {
                let proto = env.lookup_struct_prototype(&name)?;

                Ok(RuntimeVal::PlaceholderStruct { prototype: proto.clone() })
            }
        }
    }

    fn execute_fn_in_env(
        &self,
        proto_args: Vec<(String, VarType)>,
        fn_args: Vec<RuntimeVal>,
        fn_name: String,
        body: Vec<ASTNodeKind>,
        return_stmt: Option<ExpressionKind>,
        env: &mut Env,
    ) -> Result<Option<RuntimeVal>, InterpreterError> {
        // We check if we have the good number of arguments
        if proto_args.len() != fn_args.len() {
            return Err(InterpreterError::WrongArgNumberFnCall(
                fn_name,
                proto_args.len(),
                fn_args.len(),
            ));
        }

        // We declare the input arguments as variables
        for ((arg_name, arg_type), arg_val) in proto_args.into_iter().zip(fn_args) {
            // Check the types if it wasn't declare with 'any'
            // If type is 'any', we don't cast, variable can be anything
            let value = if arg_val.get_type() != arg_type && arg_type != VarType::Any {
                // We try to cast it
                arg_val.try_cast_to(&arg_type)
                                .map_err(|e| InterpreterError::FnArgWrongType(
                                    arg_name.clone(), e.to_string()
                                ))?
            } else {
                arg_val
            };

            // We delcare the var with its type
            env.declare_var(arg_name, value, true, arg_type)
                .map_err(|e| InterpreterError::FunctionEvaluation(e.to_string()))?;
        }

        // We execute all the statements
        for stmt in body {
            let _ = self.interpret_node(stmt, env)?;
        }

        // Return a value if asked
        match return_stmt {
            Some(s) => Ok(Some(self.evaluate(s, env)?)),
            None => Ok(None),
        }
    }
}

fn parse_mem_call(
    m1: &ExpressionKind,
    prop: &ExpressionKind,
) -> Result<Vec<String>, InterpreterError>
{
    let mut props: Vec<String> = Vec::new();

    parse_recurs_mem_call(m1, &mut props)?;

    // We add the last one.
    if let ExpressionKind::Identifier { symbol, .. } = prop {
        props.push(symbol.to_string());
    } else {
        return Err(InterpreterError::WrongStructMemberCall(
            props.remove(0),
            "Missing identifier".to_string(),
        ));
    }

    Ok(props)
}

// Recursivly parse member call. say we parse foo.bar.biz.buz
fn parse_recurs_mem_call(
    m1: &ExpressionKind,
    prop: &mut Vec<String>,
) -> Result<(), InterpreterError> {
    // Here, m1 is foo.bar.biz
    match m1 {
        ExpressionKind::MemberCall {
            member: m2,
            property,
        } => {
            // Here, m2 is foo.bar
            match &**m2 {
                ExpressionKind::MemberCall { .. } => parse_recurs_mem_call(&*m2, prop)?,
                ExpressionKind::Identifier { symbol } => prop.push(symbol.clone()),
                _ => {
                    return Err(InterpreterError::WrongStructMemberTypeCall);
                }
            }

            if let ExpressionKind::Identifier { symbol } = &**property {
                prop.push(symbol.clone());
            }

            Ok(())
        }
        // Else, it's a single member call like foo.bar
        ExpressionKind::Identifier { symbol } => {
            prop.push(symbol.clone());
            Ok(())
        }
        _ => Err(InterpreterError::WrongStructMemberTypeCall),
    }
}



#[cfg(test)]
mod tests {
    use crate::frontend::ast::{ASTNode, ASTNodeKind, ExpressionKind};
    use crate::interpreter::expr::ArrayIndexing;
    use crate::interpreter::{Interpreter, InterpreterError};
    use crate::{environment::Env, values::RuntimeVal};
    use frontend::parser::VarType;

    #[test]
    fn evaluate_num_literal_expr() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);

        let expr = ASTNodeKind::Expression(ExpressionKind::RealLiteral { value: 88. });

        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Real(88.))
        );
    }

    #[test]
    fn evaluate_identifier_expr() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);
        env.declare_var("mass".to_string(), RuntimeVal::Real(120.), false, VarType::Any)
            .unwrap();

        let expr = ASTNodeKind::Expression(ExpressionKind::Identifier {
            symbol: "mass".to_string(),
        });

        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Real(120.))
        );
    }

    #[test]
    fn evaluate_const_identifier_expr() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);
        env.declare_var("mass".to_string(), RuntimeVal::Int(120), false, VarType::Any)
            .unwrap();

        let expr = ASTNodeKind::Expression(ExpressionKind::Identifier {
            symbol: "mass".to_string(),
        });

        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Int(120))
        )
    }

    #[test]
    fn evaluate_binop_expr() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);

        // Addition
        let lhs = Box::new(ExpressionKind::IntLiteral { value: 60 });
        let rhs = Box::new(ExpressionKind::IntLiteral { value: 3 });
        let expr = ASTNodeKind::Expression(ExpressionKind::BinaryOp {
            left: lhs,
            right: rhs,
            operator: '+'.to_string(),
        });
        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Int(63))
        );

        // Substraction
        let lhs = Box::new(ExpressionKind::RealLiteral { value: 60. });
        let rhs = Box::new(ExpressionKind::RealLiteral { value: 3. });
        let expr = ASTNodeKind::Expression(ExpressionKind::BinaryOp {
            left: lhs,
            right: rhs,
            operator: '-'.to_string(),
        });
        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Real(57.))
        );

        // Multiplication
        let lhs = Box::new(ExpressionKind::IntLiteral { value: 60 });
        let rhs = Box::new(ExpressionKind::IntLiteral { value: 3 });
        let expr = ASTNodeKind::Expression(ExpressionKind::BinaryOp {
            left: lhs,
            right: rhs,
            operator: '*'.to_string(),
        });
        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Int(180))
        );

        // Division
        let lhs = Box::new(ExpressionKind::RealLiteral { value: 60. });
        let rhs = Box::new(ExpressionKind::RealLiteral { value: 3. });
        let expr = ASTNodeKind::Expression(ExpressionKind::BinaryOp {
            left: lhs,
            right: rhs,
            operator: '/'.to_string(),
        });
        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Real(20.))
        );

        // Modulo
        let lhs = Box::new(ExpressionKind::IntLiteral { value: 60 });
        let rhs = Box::new(ExpressionKind::IntLiteral { value: 3 });
        let expr = ASTNodeKind::Expression(ExpressionKind::BinaryOp {
            left: lhs,
            right: rhs,
            operator: '%'.to_string(),
        });
        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Int(0))
        );
    }

    #[test]
    fn evaluate_nested_binop_expr() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);
        env.declare_var("mass".to_string(), RuntimeVal::Real(120.), false, VarType::Any)
            .unwrap();

        let lhs1 = Box::new(ExpressionKind::RealLiteral { value: 60. });
        let lhs2 = Box::new(ExpressionKind::RealLiteral { value: 2. });
        let rhs = Box::new(ExpressionKind::RealLiteral { value: 3. });
        let expr = ASTNodeKind::Expression(ExpressionKind::BinaryOp {
            left: lhs1,
            right: Box::new(ExpressionKind::BinaryOp {
                left: lhs2,
                right: rhs,
                operator: '*'.to_string(),
            }),
            operator: '/'.to_string(),
        });
        assert_eq!(
            interpr.execute_program(vec![ASTNode::new(expr, 0)], &mut env),
            Ok(RuntimeVal::Real(10.))
        );
    }

    #[test]
    fn evaluate_array_call() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);

        env.declare_var(
            "values".into(),
            RuntimeVal::new_array(vec![RuntimeVal::Real(2.), RuntimeVal::Real(4.), RuntimeVal::Real(-6.)]),
            false,
            VarType::Any,
        )
        .expect("Err arr decl");

        let mut nodes: Vec<ASTNode> = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                member: Box::new(ExpressionKind::Identifier {
                    symbol: "values".into(),
                }),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 0 })),
            }
            .into(),
            0,
        )];
        let mut val = interpr
            .execute_program(nodes, &mut env)
            .expect("Error interpreting");
        assert_eq!(val, RuntimeVal::Real(2.));

        nodes = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                member: Box::new(ExpressionKind::Identifier {
                    symbol: "values".into(),
                }),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 2 })),
            }
            .into(),
            0,
        )];
        val = interpr
            .execute_program(nodes, &mut env)
            .expect("Error interpreting");
        assert_eq!(val, RuntimeVal::Real(-6.));

        nodes = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                member: Box::new(ExpressionKind::Identifier {
                    symbol: "values".into(),
                }),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: -1 })),
            }
            .into(),
            0,
        )];
        val = interpr
            .execute_program(nodes, &mut env)
            .expect("Error interpreting");
        assert_eq!(val, RuntimeVal::Real(-6.));

        nodes = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                member: Box::new(ExpressionKind::Identifier {
                    symbol: "values".into(),
                }),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: -3 })),
            }
            .into(),
            0,
        )];
        val = interpr
            .execute_program(nodes, &mut env)
            .expect("Error interpreting");
        assert_eq!(val, RuntimeVal::Real(2.));

        nodes = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                member: Box::new(ExpressionKind::Identifier {
                    symbol: "values".into(),
                }),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 4 })),
            }
            .into(),
            0,
        )];
        let mut _err = interpr.execute_program(nodes, &mut env);
        assert!(matches!(InterpreterError::ArrayOverIndexing, _err));

        nodes = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                member: Box::new(ExpressionKind::Identifier {
                    symbol: "values".into(),
                }),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: -5 })),
            }
            .into(),
            0,
        )];
        _err = interpr.execute_program(nodes, &mut env);
        assert!(matches!(InterpreterError::ArrayOverIndexing, _err));
    }
}
