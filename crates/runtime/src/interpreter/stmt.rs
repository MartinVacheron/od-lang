use std::cell::RefCell;
use std::rc::Rc;

use colored::*;

use super::{Interpreter, InterpreterError};
use crate::frontend::ast::{ASTNodeKind, ExpressionKind, StatementKind};
use crate::{environment::Env, values::RuntimeVal};
use frontend::parser::VarType;


impl Interpreter {
    pub(super) fn resolve(&self, stmt: StatementKind, env: &mut Env) -> Result<RuntimeVal, InterpreterError> {
        match stmt {
            StatementKind::VarDeclaration {
                name,
                value,
                constant,
                var_type,
            } => {
                // We get declaration value
                let mut declaration_value = self.evaluate(value, env)?;

                // If this is structure that we assign, we create new members otherwise we point
                // to the other structure, like a pointer
                // TODO: test
                if let RuntimeVal::Structure { prototype, members } = declaration_value {
                    declaration_value = RuntimeVal::Structure {
                            prototype: prototype.clone(),
                            members: Rc::new(RefCell::new(members.borrow().clone()))
                        }
                }

                // We declare the variable
                env.declare_var(name, declaration_value, constant, var_type)
                    .map_err(|e| InterpreterError::VarDeclaration(format!("{e}")))?;

                Ok(RuntimeVal::Null)
            }
            StatementKind::VarCommaDeclaration { declarations } => {
                for decl in declarations {
                    self.resolve(decl, env)?;
                }

                Ok(RuntimeVal::Null)
            }
            StatementKind::StructDeclaration {
                name,
                members,
                functions,
            } => {
                env.declare_struct(name, members, functions)
                    .map_err(|e| InterpreterError::StructDeclaration(format!("{e}")))?;

                Ok(RuntimeVal::Null)
            }
            // FIXME: put real type for return type
            StatementKind::FnDeclaration {
                name,
                args_and_type,
                body,
                return_stmt,
                return_type
            } => {
                env.declare_var(
                    name.clone(),  // Clone for closure capture for error
                    RuntimeVal::Function {
                        args_and_type,
                        body,
                        return_stmt,
                        return_type
                    },
                    true,
                    VarType::Func,
                )
                .map_err(|e| InterpreterError::FunctionDeclaration(name, e.to_string()))?;

                Ok(RuntimeVal::Null)
            }
            // TODO: Faire fonctionner la fonction de test err
            StatementKind::TestDeclaration { name, body } => {
                print!("Running test: {}...", name);

                let mut res: Vec<bool> = vec![];

                // Tmp env
                let mut tmp_env = Env::new(Some(env));

                // We check if this is a assert function that is called
                for node in body {
                    let is_test = match &node {
                        ASTNodeKind::Expression(ExpressionKind::FunctionCall {
                            name, ..
                        }) => {
                            match &**name {
                                "assert_eq" | "assert_neq" => true,
                                _ => false,
                            }
                        }
                        _ => false,
                    };

                    // We interpret the node
                    let run_val = self.interpret_node(node, &mut tmp_env);

                    if is_test {
                        match run_val {
                            Ok(RuntimeVal::Bool(b)) => res.push(b),
                            _ => res.push(false),
                        }
                    }
                    // If not, if it is an error, we propagate it
                    else {
                        if let Err(e) = run_val {
                            return Err(e);
                        }
                    }
                }

                // We check if there are only true result
                let final_res = res.into_iter().all(|x| x);

                // Result
                let final_text = match final_res {
                    true => "Ok".green().bold(),
                    false => "Ko".red().bold(),
                };

                print!("    {}\n", final_text);

                Ok(RuntimeVal::Bool(final_res))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        environment::{Env, EnvError},
        interpreter::Interpreter,
        values::{RuntimeVal, StructMember, StructPrototype},
    };
    use frontend::parser::VarType;

    use crate::frontend::ast::{ASTNode, ASTNodeKind, ExpressionKind, StatementKind};
    use std::rc::Rc;

    #[test]
    fn evaluate_var_declaration() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);
        env.declare_var("mass".into(), RuntimeVal::Real(120.), false, VarType::Real)
            .unwrap();

        let var_name = String::from("position");

        // We check that the var isn't yet declared
        assert!(matches!(
            env.lookup_var(&var_name),
            Err(EnvError::UndeclaredVar(_))
        ));

        let var_decl = ASTNodeKind::Statement(StatementKind::VarDeclaration {
            name: var_name.clone(),
            value: ExpressionKind::RealLiteral { value: 45. },
            constant: false,
            var_type: VarType::Any,
        });

        let _ = interpr.execute_program(vec![ASTNode::new(var_decl, 0)], &mut env);
        // We should retreive it now
        assert_eq!(env.lookup_var(&var_name), Ok(&RuntimeVal::Real(45.)));
    }

    #[test]
    fn evaluate_var_declaration_already_declared() {
        let interpr = Interpreter {};
        let var_name = String::from("mass");
        let mut env = Env::new(None);
        env.declare_var(var_name.clone(), RuntimeVal::Real(120.), false, VarType::Any)
            .unwrap();

        // We check that the var isn't yet declared
        assert!(env.lookup_var(&var_name).is_ok());

        let var_decl = ASTNodeKind::Statement(StatementKind::VarDeclaration {
            name: var_name.clone(),
            value: ExpressionKind::RealLiteral { value: 45. },
            constant: false,
            var_type: VarType::Any,
        });

        let res = interpr.execute_program(vec![ASTNode::new(var_decl, 0)], &mut env);
        // We should retreive it now
        assert!(res.is_err());
    }

    #[test]
    fn evaluate_var_assignment() {
        let interpr = Interpreter {};
        let var_name = String::from("mass");
        let var_name2 = String::from("gravity");
        let mut env = Env::new(None);

        let var_decl = ASTNodeKind::Statement(StatementKind::VarDeclaration {
            name: var_name.clone(),
            value: ExpressionKind::RealLiteral { value: 45. },
            constant: false,
            var_type: VarType::Any,
        });
        let var_decl2 = ASTNodeKind::Statement(StatementKind::VarDeclaration {
            name: var_name2.clone(),
            value: ExpressionKind::RealLiteral { value: 20. },
            constant: false,
            var_type: VarType::Any,
        });

        let var_assign = ASTNodeKind::Expression(ExpressionKind::VarAssignment {
            assigne: Box::new(ExpressionKind::Identifier {
                symbol: var_name.clone(),
            }),
            value: Box::new(ExpressionKind::RealLiteral { value: 12. }),
        });
        let var_assign_var = ASTNodeKind::Expression(ExpressionKind::VarAssignment {
            assigne: Box::new(ExpressionKind::Identifier {
                symbol: var_name2.clone(),
            }),
            value: Box::new(ExpressionKind::BinaryOp {
                left: Box::new(ExpressionKind::Identifier {
                    symbol: var_name.clone(),
                }),
                right: Box::new(ExpressionKind::RealLiteral { value: 60. }),
                operator: '+'.to_string(),
            }),
        });

        let _ = interpr.execute_program(
            vec![
                ASTNode::new(var_decl, 0),
                ASTNode::new(var_decl2, 0),
                ASTNode::new(var_assign, 0),
                ASTNode::new(var_assign_var, 0),
            ],
            &mut env,
        );
        // We should retreive it now
        assert_eq!(
            env.lookup_var(&var_name).unwrap(),
            &RuntimeVal::Real(12.)
        );
        assert_eq!(
            env.lookup_var(&var_name2).unwrap(),
            &RuntimeVal::Real(72.)
        );
    }

    #[test]
    fn evaluate_const_var_assignment() {
        let interpr = Interpreter {};
        let var_name = String::from("mass");
        let mut env = Env::new(None);

        let var_decl = ASTNodeKind::Statement(StatementKind::VarDeclaration {
            name: var_name.clone(),
            value: ExpressionKind::RealLiteral { value: 45. },
            constant: true,
            var_type: VarType::Any,
        });
        let var_assign = ASTNodeKind::Expression(ExpressionKind::VarAssignment {
            assigne: Box::new(ExpressionKind::Identifier {
                symbol: var_name.clone(),
            }),
            value: Box::new(ExpressionKind::RealLiteral { value: 12. }),
        });

        let res = interpr.execute_program(
            vec![ASTNode::new(var_decl, 0), ASTNode::new(var_assign, 0)],
            &mut env,
        );
        // We should retreive it now
        assert!(res.is_err());
    }

    #[test]
    fn evaluate_struct_declaration() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);

        let name: String = "Planet".into();
        let members = vec![("pos".into(), VarType::Any, false), ("radius".into(), VarType::Any, true)];
        let constructor_args: Vec<(String, VarType)> = vec![
            ("pos".into(), VarType::Any),
            ("x".into(), VarType::Any),
            ("radius".into(), VarType::Any),
            ("y".into(), VarType::Any),
        ];

        let constructor = StatementKind::FnDeclaration {
            name: "new".into(),
            args_and_type: constructor_args.clone(),
            body: vec![],
            return_stmt: None,
            return_type: VarType::Void
        };

        let struct_decl: ASTNodeKind = StatementKind::StructDeclaration {
            name: name.clone(),
            members,
            functions: vec![constructor],
        }.into();

        interpr
            .execute_program(vec![ASTNode::new(struct_decl, 0)], &mut env)
            .expect("Error interpreting");

        let members_expected: Vec<StructMember> = vec![
            StructMember { 
                name: "pos".into(), value: RuntimeVal::Null,
                constant: false, member_type: VarType::Any
            },
            StructMember { 
                name: "radius".into(), value: RuntimeVal::Null,
                constant: true, member_type: VarType::Any
            },
        ];

        // We should retreive it now
        assert_eq!(
            Rc::new(StructPrototype {
                name: name.clone(),
                members: members_expected,
                constructor: Some(RuntimeVal::Function {
                    args_and_type: constructor_args,
                    body: vec![],
                    return_stmt: None,
                    return_type: VarType::Void
                }),
            }),
            env.lookup_struct_prototype(&name)
                .expect("Error looking up structure proto")
        );
    }

    #[test]
    fn evaluate_member_assignment_struct_decl() {
        let interpr = Interpreter {};
        let mut env = Env::new(None);

        let name = String::from("Planet");

        let constructor_args = vec![
            ("pos".into(), VarType::Any),
            ("x".into(), VarType::Any),
            ("radius".into(), VarType::Any),
            ("y".into(), VarType::Any),
        ];

        let constructor = StatementKind::FnDeclaration {
            name: "new".into(),
            args_and_type: constructor_args.clone(),
            body: vec![],
            return_stmt: None,
            return_type: VarType::Void
        };

        let struct_decl: ASTNodeKind = StatementKind::StructDeclaration {
            name: name.clone(),
            members: vec![("pos".into(), VarType::Any, false), ("radius".into(), VarType::Any, true)],
            functions: vec![constructor],
        }.into();

        let struct_create = ExpressionKind::FunctionCall {
            name: "Planet".into(),
            args: vec![
                ExpressionKind::RealLiteral { value: 80. },
                ExpressionKind::RealLiteral { value: 1. },
                ExpressionKind::RealLiteral { value: 25. },
                ExpressionKind::RealLiteral { value: 64. },
            ]
        };

        let var_decl = ASTNodeKind::Statement(StatementKind::VarDeclaration {
            name: "mars".into(),
            value: struct_create,
            constant: false,
            var_type: VarType::Struct("Planet".into())
        });

        let _ = interpr.execute_program(
            vec![ASTNode::new(struct_decl, 0), ASTNode::new(var_decl, 0)],
            &mut env,
        );

        // We get the var
        let mars = env.lookup_var(&"mars".into()).expect("Error fetching mars");

        if let RuntimeVal::Structure { members, .. } = mars {
            assert_eq!(
                members
                    .borrow()
                    .get(&String::from("pos"))
                    .expect("Error getting member"),
                &RuntimeVal::Real(80.)
            );
            assert_eq!(
                members
                    .borrow()
                    .get(&String::from("radius"))
                    .expect("Error getting member"),
                &RuntimeVal::Real(25.)
            );
            assert_eq!(members.borrow().get(&String::from("x")), None);
            assert_eq!(members.borrow().get(&String::from("y")), None);
        } else {
            assert!(false);
        }
    }
}
