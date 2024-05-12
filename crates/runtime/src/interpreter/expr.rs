use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::InterpreterError;
use crate::frontend::ast::{ASTNodeKind, ArrayIndexing, ExpressionKind};
use crate::values::{StructMember, StructPrototype};
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
                .map_err(|e| InterpreterError::Evaluation(e.to_string()))?
                .clone()),
            ExpressionKind::ArrayLiteral { values } => {
                let mut val: Vec<RuntimeVal> = vec![];
                // We evaluate all the values
                for v in values {
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
                    .map_err(|e| InterpreterError::Evaluation(e.to_string()))?)
            }
            // TODO: Test
            // TODO: use new memcall parser
            ExpressionKind::VarAssignment { assigne, value } => {
                let mut assignment_value = self.evaluate(*value, env)?;
                println!("\nIn var assign, value evaluated to: {:?}", assignment_value);

                // TODO: if it is a structure, we have to break the link of the Rc otherwise they
                // share same data as a pointer
                assignment_value = match assignment_value {
                    RuntimeVal::Structure { prototype, members } => {
                        println!("\nDuplicating members to break link");
                        
                        RuntimeVal::Structure {
                            prototype: prototype.clone(),
                            members: Rc::new(RefCell::new(members.borrow().clone()))
                        }
                    },
                    _ => assignment_value
                };

                match *assigne {
                    ExpressionKind::Identifier { symbol } => {
                        // We clone it to return the value too, in case of x = y = z = 5 for ex
                        env.assign_var(&symbol, AssignType::Replace(assignment_value.clone()))?;

                        Ok(assignment_value)
                    }
                    ExpressionKind::MemberCall { member, property } => {
                        // let props = parse_mem_call(&*member, &*property)?;
                        
                        // Cloning for same reason than above
                        // env.assign_struct_var(props, AssignType::Replace(assignment_value.clone()))?;


                        // TEST: 
                        let (members, proto) = self.get_member_call_last_struct(&*member, env)?;
                        let mut tmp_env = Env::new(Some(env));
                        tmp_env.create_self(proto.clone(), members.clone())?;

                        match &*property {
                            ExpressionKind::Identifier { symbol } => {
                                members.borrow_mut().insert(symbol.clone(), assignment_value.clone()).expect("Member not found on struct");
                            }
                            _ => panic!("Just trying for now")
                        }






                        // self.resolve_assign_mem_call(&member, &property, env, Some(assignment_value.clone()))?;

                        Ok(assignment_value)
                    }
                    // TODO: use lookup_mut_var and to the replacement here?
                    ExpressionKind::ArrayCall { name, index } => {
                        // All cases
                        match index {
                            // Single index is the only supported assignment
                            ArrayIndexing::Single(i) => {
                                // Evaluate the index
                                let id = self.evaluate(*i, env)?;

                                env.assign_var(
                                    &name,
                                    AssignType::ArrayModification {
                                        index: id,
                                        value: assignment_value,
                                    },
                                )?;
                            }
                            // Else, error
                            ArrayIndexing::Slice { .. } => {
                                return Err(InterpreterError::ArraySliceAssign(name))
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
                member,
                property,
            } => {
                // We check first that we are not just calling a method on an array because it has
                // the same shape as the member calls like: arr.push(5)
                // with 'arr' as member and 'push(5)' as property

                let (members, proto) = self.get_member_call_last_struct(&*member, env)?;

                let mut tmp_env = Env::new(Some(env));

                tmp_env.create_self(proto.clone(), members.clone())?;

                // Here, we clone the value, no need to return a reference
                match &*property {
                    ExpressionKind::FunctionCall { name, args: args_expr } => {
                        let args = self.evaluate_fn_args_value(args_expr.clone(), &mut tmp_env)?;

                        let struct_fn = proto
                            .members
                            .iter()
                            .filter(|m| &m.name == name)
                            .collect::<Vec<&StructMember>>();

                        let struct_fn = struct_fn.first().expect("Method not found");

                        if let RuntimeVal::Function { args_and_type, body, return_stmt, return_type } = &struct_fn.value {
                            match self.execute_fn_in_env(
                                args_and_type.clone(),
                                args,
                                name.clone(),
                                body.clone(),
                                return_stmt.clone(),
                                &mut tmp_env
                            )?
                            {
                                Some(res) => Ok(res),
                                None => Ok(RuntimeVal::Null),
                            }
                        } else {
                            panic!("Struct member not a fn")
                        }
                    }
                    ExpressionKind::Identifier { symbol } => {
                        Ok(members.borrow().get(symbol).expect("Member not found on struct").clone())
                    }
                    ExpressionKind::ArrayCall { name, index } => {
                        let rv = members.borrow().get(name).expect("Member not found on struct").clone();

                        match rv {
                            RuntimeVal::Array(_) => {
                                self.evaluate_array_indexing(rv, index.clone(), env)
                            }
                            _ => panic!("Expected to get an array as member for array call")
                        }
                    },
                    _ => todo!("Can't be anything else than a function call on an identifier member expression")
                }
            }
            ExpressionKind::FunctionCall {
                name,
                args: args_expr,
            } => {
                // We evaluate each of the argument
                let args = self.evaluate_fn_args_value(args_expr, env)?;

                // Temporary env of execution
                let mut tmp_env = Env::new(Some(env));

                // We check if this is a structure that we are calling
                let proto = env.lookup_struct_prototype(&name);
                if let Ok(p) = proto {
                    return self.create_structure(p.clone(), args, &mut tmp_env)
                }

                let func = env.lookup_var(&name)?.clone();

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
                        match self.execute_fn_in_env(
                            args_and_type,
                            args,
                            name,
                            body,
                            return_stmt,
                            &mut tmp_env,
                        )?
                        {
                            Some(res) => Ok(res),
                            None => Ok(RuntimeVal::Null),
                        }
                    }
                    _ => Err(InterpreterError::NonFunctionCall(name)),
                }
            }
            // Array calls are: arr[0], arr[:x], ...
            ExpressionKind::ArrayCall { name, index } => {
                // We evaluate array variable and index
                let arr = env.lookup_var(&name)?.clone();

                self.evaluate_array_indexing(arr, index, env)
            }
            ExpressionKind::EmptyStructLiteral { name } => {
                let proto = env.lookup_struct_prototype(&name)?;

                Ok(RuntimeVal::PlaceholderStruct { prototype: proto.clone() })
            }
        }
    }

    // Evaluates an array call
    fn evaluate_array_indexing(&self, array: RuntimeVal, index: ArrayIndexing, env: &mut Env) -> Result<RuntimeVal, InterpreterError> {
        // All cases
        match array {
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

    fn execute_fn_in_env(
        &self,
        proto_args: Vec<(String, VarType)>,
        fn_args: Vec<RuntimeVal>,
        fn_name: String,
        body: Vec<ASTNodeKind>,
        return_stmt: Option<ExpressionKind>,
        env: &mut Env,
    ) -> Result<Option<RuntimeVal>, InterpreterError>
    {
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

    // Create the structure objects
    fn create_structure(
        &self,
        proto: Rc<StructPrototype>,
        args_value: Vec<RuntimeVal>,
        env: &mut Env
    ) -> Result<RuntimeVal, InterpreterError>
    {
        // All members. We get the prototype and clone all it's value to use them
        // It belongs to the new struct now
        let members: Rc<RefCell<HashMap<String, RuntimeVal>>> = Rc::new(RefCell::new(HashMap::new()));
        for mem in proto.members.clone() {
            members.borrow_mut().insert(mem.name, mem.value);
        }

        if let Some(RuntimeVal::Function { args_and_type, body, .. }) = proto.constructor.clone() {
            // If some were given while creating the structure, error
            if !args_value.is_empty() && args_and_type.is_empty() {
                return Err(InterpreterError::StructEmptyConstructor(
                    proto.name.clone(),
                ))
            }

            // If the wrong number of arguments was given, error
            if args_value.len() != args_and_type.len() {
                return Err(InterpreterError::StructConstructorWrongArgNb(
                    proto.name.clone(),
                    args_and_type.len(),
                    args_value.len(),
                ))
            }

            // We iterate over the tuple (arg_name, arg_value)
            // This allow to avoid boilerplate code to the user
            args_and_type
                .into_iter()
                .zip(args_value)
                .try_for_each(|((arg_name, arg_type), arg_val)| -> Result<(), InterpreterError> {
                    // We declare it in the tmp env if there is one (meaning there is
                    // a constructor body). Arguments are obviously constant
                    env.declare_var(arg_name.clone(), arg_val.clone(), true, arg_type)
                        .map_err(|e| {
                            InterpreterError::StructCreation(
                                proto.name.clone(),
                                e.to_string(),
                            )
                        })?;

                    // If it is a member
                    if proto.has_member_named(&arg_name) {
                        members.borrow_mut().insert(arg_name.clone(), arg_val);
                    }

                    Ok(())
                })
                .map_err(|e| {InterpreterError::StructCreation(
                        proto.name.clone(),
                        e.to_string(),
                )})?;

            if !body.is_empty() {
                // We execute the constructor body
                // We create the self structure. Can't fail
                // We clone the Rc and as it is a RefCell, values updates will
                // directly go to members. No need to get them back
                env.create_self(proto.clone(), members.clone()).map_err(|e| {
                    InterpreterError::SelfInConstructor(proto.name.clone(), e.to_string())
                })?;

                for stmt in body {
                    // println!("\nStmt in struct decl: {:?}", stmt);
                    let _ = self.interpret_node(stmt.clone(), env)?;
                }
            }
        }

        // We return the structure
        Ok(RuntimeVal::Structure {
                prototype: proto.clone(),
                members
            }
        )
    }


    // TODO: Checker tous les 'clone'
    // TODO: checker les 'todo!'
    // Hard to understand function. A member call has to be parsed in reverse order
    // The shape is:
    // Member: MemberCall {
    //     member: MemberCall {
    //         member: MemberCall {
    //             member: Identifier {
    //                 symbol: "top",
    //             },
    //             property: Identifier {
    //                 symbol: "glob",
    //             },
    //         },
    //         property: Identifier {
    //             symbol: "p",
    //         },
    //     },
    //     property: Identifier {
    //         symbol: "pos",
    //     },
    // }
    // So we parse the member calls until the first identifier and go upward


    // TODO: before entering this fn, be sure the prop is either a fn call or array call
    fn resolve_assign_mem_call(
        &self,
        member: &ExpressionKind,
        property: &ExpressionKind,
        env: &mut Env,
        out: Option<RuntimeVal>
    ) -> Result<RuntimeVal, InterpreterError>
    {
        let mut all_sub_mem: Vec<ExpressionKind> = vec![];

        let first_var: &mut RuntimeVal;
        let mut tmp_mem: ExpressionKind = member.clone();


        loop {
            match tmp_mem {
                // While the member is a sub member call, we continue
                ExpressionKind::MemberCall { member, property } => {
                    tmp_mem = *member;
                    // And we add the property to the list to resolve later
                    all_sub_mem.push(*property);
                }
                // If member is an identifier, we reached the end, ex: foo.bar....
                ExpressionKind::Identifier { ref symbol } => {
                    first_var = env.lookup_mut_var(&symbol)?;
                    break
                }
                _ => panic!("Impossible to be here")
            }
        }
        
        all_sub_mem.reverse();

        let mut final_proto;
        let mut final_members;

        match first_var {
            RuntimeVal::Structure { prototype, members } => {
                final_proto = prototype.clone();
                final_members = members.clone();
            },
            _ => panic!("impossible to be here")
        };

        for sub_mem in all_sub_mem {
            match sub_mem {
                ExpressionKind::Identifier { symbol } => {
                    if let RuntimeVal::Structure { prototype, members } = final_members.clone().borrow().get(&symbol).unwrap() {
                        final_proto = prototype.clone();
                        final_members = members.clone();
                    } else {
                        todo!("Not a struct member call chain")
                    }
                }
                _ => panic!("impossible to be here")
            }
        }

        println!("Final member: {:?}", final_members);

        // 'final borrow' is the RefCell of members of last struct of chain
        match property {
            ExpressionKind::FunctionCall { name, args: args_expr } => {
                let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

                // We get the function signature in the members
                // TODO: create self
                println!("In member call resolve, fn call on struct, self create");
                
                let mut tmp_env = Env::new(Some(env));
                tmp_env.create_self(final_proto.clone(), final_members.clone())?;

                // drop(final_members);

                // FIXME: Double borrow with the execute in env after because of 'self' create...
                // To fix, we have to change the assignment process so its not the value that does
                // it but same fn as this one, so one owner?
                // FIXME: extract args and type and stuff or panic and then call execute to live the borrow of if let
                if let RuntimeVal::Function { args_and_type, body, return_stmt, .. } = final_members.as_ref().borrow().get(name).unwrap() {
                    println!("In member call resolve, fn call on struct");

                    match self.execute_fn_in_env(args_and_type.clone(), args, name.clone(), body.clone(), return_stmt.clone(), &mut tmp_env)? {
                        Some(r) => Ok(r),
                        None => Ok(RuntimeVal::Null)
                    }
                // TODO: support fn call on arrays
                } else {
                    panic!("Function not found on structure fn member call")
                }
            }
            // If property at the end, we could be assigning a value
            ExpressionKind::Identifier { symbol } => {
                match out {
                    Some(v) => {
                        println!("Nb borrows: {}", Rc::strong_count(&final_members));

                        println!("Env self var: {:?}", env.lookup_var(&"self".into())?);

                        // final_members.borrow_mut().insert(symbol.clone(), v);

                        env.assign_to_self(symbol, v);
                        Ok(RuntimeVal::Null)
                    }
                    None => Ok(final_members.borrow().get(symbol).unwrap().clone())
                }
            }
            _ => panic!("Property called is neither a fn or member: {:?}", property)
        }

        // match property {
        //     ExpressionKind::FunctionCall { name, args: args_expr } => {
        //         match final_members.borrow_mut().get_mut(name).unwrap() {
        //             RuntimeVal::Structure { prototype, members } => {
        //                 if let RuntimeVal::Function {
        //                     args_and_type,
        //                     body,
        //                     return_stmt,
        //                     return_type
        //                 } = members.borrow().get(name).unwrap()
        //                 {
        //                     let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

        //                     match self.execute_fn_in_env(args_and_type.clone(), args, name.clone(), body.clone(), return_stmt.clone(), env)? {
        //                         Some(r) => Ok(r),
        //                         None => Ok(RuntimeVal::Null)
        //                     }
        //                 } else { panic!("flemme") }
        //             }
        //             RuntimeVal::Array(arr) => {
        //                 // We evaluate each of the argument
        //                 let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

        //                 Ok(arr.call(name.as_str(), args.as_slice())?)
        //             }
        //             _ => panic!("Non struct or array member fn call")
        //         }
        //     }
        //     ExpressionKind::Identifier { symbol } => {
        //         Ok(final_members.borrow().get(symbol).unwrap().clone())
        //     }
        //     _ => panic!("No more choices for: {:?}", property)
        // }
        

        // if let ExpressionKind::Identifier { symbol } = last_name {
        //     let mut bor = final_members.borrow_mut();
        //     let last_mem = bor.get_mut(&symbol).unwrap();

        // match last_mem {
        //     RuntimeVal::Structure { prototype, members } => {
        //         match property {
        //             ExpressionKind::FunctionCall { name, args: args_expr } => {
        //                 if let RuntimeVal::Function {
        //                     args_and_type,
        //                     body,
        //                     return_stmt,
        //                     return_type
        //                 } = members.borrow().get(name).unwrap()
        //                 {
        //                     let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

        //                     match self.execute_fn_in_env(args_and_type.clone(), args, name.clone(), body.clone(), return_stmt.clone(), env)? {
        //                         Some(r) => Ok(r),
        //                         None => Ok(RuntimeVal::Null)
        //                     }
        //                 } else { panic!("Not a function") }
        //             }
        //             _ => panic!("Can't be anything else")
        //         }
        //     }
        //     RuntimeVal::Array(arr) => {
        //         match property {
        //             ExpressionKind::FunctionCall { name, args: args_expr } => {
        //                 // We evaluate each of the argument
        //                 let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

        //                 Ok(arr.call(name.as_str(), args.as_slice())?)
        //             }
        //             _ => Err(InterpreterError::ArrayNonMethodCall)
        //         }
        //     }
        //     _ => panic!("Last mem not a struct")
        // }
        // }
        // else { panic!("Last sub member not an identifier") }
    }

    
    
    // TEST: returns a Rc<RefCell<>> of last member call members
    fn get_member_call_last_struct(
        &self,
        member: &ExpressionKind,
        env: &mut Env
    ) -> Result<(
            Rc<RefCell<HashMap<String, RuntimeVal>>>,
            Rc<StructPrototype>
        ), InterpreterError>
    {
        let mut all_sub_mem: Vec<ExpressionKind> = vec![];

        let first_var: &mut RuntimeVal;
        let mut tmp_mem: ExpressionKind = member.clone();

        loop {
            match tmp_mem {
                // While the member is a sub member call, we continue
                ExpressionKind::MemberCall { member, property } => {
                    tmp_mem = *member;
                    // And we add the property to the list to resolve later
                    all_sub_mem.push(*property);
                }
                // If member is an identifier, we reached the end, ex: foo.bar....
                ExpressionKind::Identifier { ref symbol } => {
                    first_var = env.lookup_mut_var(&symbol)?;
                    break
                }
                // TODO: Separate in own fn like 'get_first_elem(tmp_mem)
                // Same for array, ex: foo[0].bar....
                ExpressionKind::ArrayCall { name, index } => {
                    let idx = match index {        
                        ArrayIndexing::Single(idx) => {
                            let index = self.evaluate(*idx, env)?;
    
                            match index {
                                RuntimeVal::Int(i) => i,
                                _ => panic!("Array index not an int")
                            }
                        },
                        _ => panic!("Cannot slice array for member call")
                    };

                    match env.lookup_mut_var(&name)? {
                        RuntimeVal::Array(arr) => {
                            // Manages negative indexes
                            let i = arr.check_and_get_index(idx)?;

                            first_var = arr.val.get_mut(i).expect("Wrong array index")
                        },
                        _ => panic!("Expected to get an array as member for array call")
                    }
                    break
                }
                // TODO: support first elem as a fn call
                ExpressionKind::FunctionCall { name, args } => todo!("First element of member being a function not implemented"),
                _ => panic!("First member's type of member call not supported")
            }
        }
        
        all_sub_mem.reverse();

        let mut final_proto;
        let mut final_members;

        match first_var {
            RuntimeVal::Structure { prototype, members } => {
                final_proto = prototype.clone();
                final_members = members.clone();
            },
            _ => panic!("First var in member call is not a struct")
        };

        for sub_mem in all_sub_mem {
            match sub_mem {
                ExpressionKind::Identifier { symbol } => {
                    if let RuntimeVal::Structure { prototype, members } = final_members.clone().borrow().get(&symbol).unwrap() {
                        final_proto = prototype.clone();
                        final_members = members.clone();
                    } else {
                        todo!("Not a struct member call chain in identifier property")
                    }
                }
                ExpressionKind::ArrayCall { name, index } => {
                    let idx = match index {        
                        ArrayIndexing::Single(idx) => {
                            let index = self.evaluate(*idx, env)?;
    
                            match index {
                                RuntimeVal::Int(i) => i,
                                _ => panic!("Array index not an int")
                            }
                        },
                        _ => panic!("Cannot slice array for member call")
                    };

                    match final_members.clone().borrow().get(&name).expect("Array member not found in member call") {
                        RuntimeVal::Array(arr) => {
                            let i = arr.check_and_get_index(idx)?;

                            match arr.val.get(i).expect("Wrong array index") {
                                RuntimeVal::Structure { prototype, members } => {
                                    final_proto = prototype.clone();
                                    final_members = members.clone();
                                },
                                _ => panic!("Array call gave a non-struct object in member call")
                            }
                        },
                        _ => panic!("Indexing non array object in member call")
                    }
                }
                ExpressionKind::FunctionCall { name, args } => todo!("Support function call in member call chains"),
                _ => panic!("property access other than identifier or array call: {:?}", sub_mem)
            }
        }

        Ok((final_members, final_proto))
    }
    
    fn evaluate_fn_args_value(
        &self,
        args_expr: Vec<ExpressionKind>,
        env: &mut Env
    ) -> Result<Vec<RuntimeVal>, InterpreterError>
    {
        let mut args: Vec<RuntimeVal> = vec![];
        for arg in args_expr {
            args.push(self.evaluate(arg, env)?);
        }

        Ok(args)
    }
}


#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use crate::frontend::ast::{ASTNode, ASTNodeKind, ExpressionKind};
    use crate::interpreter::expr::ArrayIndexing;
    use crate::interpreter::{Interpreter, InterpreterError};
    use crate::values::{StructMember, StructPrototype};
    use crate::{environment::{Env, create_global_env}, values::RuntimeVal};
    use frontend::lexer::Lexer;
    use frontend::parser::{Parser, VarType};

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

    // TODO: Add test on array slice indexing
    #[test]
    fn evaluate_array_indexing() {
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
                name: "values".into(),
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
                name: "values".into(),
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
                name: "values".into(),
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
                name: "values".into(),
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
                name: "values".into(),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: 4 })),
            }
            .into(),
            0,
        )];
        
        assert!(interpr.execute_program(nodes, &mut env).is_err());

        nodes = vec![ASTNode::new(
            ExpressionKind::ArrayCall {
                name: "values".into(),
                index: ArrayIndexing::Single(Box::new(ExpressionKind::IntLiteral { value: -5 })),
            }
            .into(),
            0,
        )];
        
        assert!(interpr.execute_program(nodes, &mut env).is_err());
    }
    
    #[test]
    fn array_elem_different_type() {
        let interpr = Interpreter {};
        let mut env = create_global_env();
        

        let expr = ExpressionKind::ArrayLiteral { values: vec![
            ExpressionKind::IntLiteral { value: 8 },
            ExpressionKind::RealLiteral { value: 9.7 },
        ]};

        assert_eq!(
            interpr.evaluate(expr, &mut env),
            Err(InterpreterError::ArrayElemDiffType)
        );

        let expr = ExpressionKind::ArrayLiteral { values: vec![
            ExpressionKind::RealLiteral { value: 9.7 },
            ExpressionKind::Identifier { symbol: "true".into() },
        ]};

        assert_eq!(
            interpr.evaluate(expr, &mut env),
            Err(InterpreterError::ArrayElemDiffType)
        );
    }

    const member_call_code: &str = "
        struct Planet {
            var pos: vec2
            var mass: real
            var sat: []

            new(pos, mass, sat) {
                self.pos.x += 10

                var i: int = 5
                self.mass /= i
            }
        }

        struct Sat {
            var orbite: vec2

            new(orbite)
        }

        struct vec2 {
            var x, y: real

            new(x: real, y: real)

            fn add(x: int, y: int) -> int {
                return self.x + y
            }

            fn div2() {
                self.x /= 2.
                self.y /= 6.
            }
        }

        struct SolSystem {
            var planets: []
            var nb_obj: int
            var main_planet: Planet

            new(planets, nb_obj, main_planet)
        }

        var satelites: [] = [Sat(vec2(56, 89)), Sat(vec2(-5, 999)), Sat(vec2(-54765, 10345))]
        var mars = Planet(vec2(1, 3), 50, satelites)
        var sol_sys = SolSystem([mars], 4, mars)
    ";

    fn get_add_fn() -> RuntimeVal {
        RuntimeVal::Function {
            args_and_type: vec![("x".into(), VarType::Int), ("y".into(), VarType::Int)],
            body: vec![],
            return_stmt: Some(
                ExpressionKind::BinaryOp {
                    left: Box::new(ExpressionKind::MemberCall {
                        member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                        property: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                    }),
                    right: Box::new(ExpressionKind::Identifier { symbol: "y".into() }),
                    operator: "+".into()
                }
            ),
            return_type: VarType::Int
        }
    }
    fn get_div2() -> RuntimeVal {
        RuntimeVal::Function {
            args_and_type: vec![],
            body: vec![
                ExpressionKind::VarAssignment {
                    assigne: Box::new(ExpressionKind::MemberCall {
                        member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                        property: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                    }),
                    value: Box::new(ExpressionKind::BinaryOp {
                        left: Box::new(ExpressionKind::MemberCall {
                            member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                            property: Box::new(ExpressionKind::Identifier { symbol: "x".into() }),
                        }),
                        right: Box::new(ExpressionKind::RealLiteral { value: 2. }),
                        operator: "/".into()
                    })
                }.into(),
                ExpressionKind::VarAssignment {
                    assigne: Box::new(ExpressionKind::MemberCall {
                        member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                        property: Box::new(ExpressionKind::Identifier { symbol: "y".into() }),
                    }),
                    value: Box::new(ExpressionKind::BinaryOp {
                        left: Box::new(ExpressionKind::MemberCall {
                            member: Box::new(ExpressionKind::Identifier { symbol: "self".into() }),
                            property: Box::new(ExpressionKind::Identifier { symbol: "y".into() }),
                        }),
                        right: Box::new(ExpressionKind::RealLiteral { value: 6. }),
                        operator: "/".into()
                    })
                }.into()
            ],
            return_stmt: None,
            return_type: VarType::Void
        }
    }

    fn get_vec2_proto() -> Rc<StructPrototype> {
        let proto = Rc::new(StructPrototype {
            name: "vec2".into(),
            members: vec![
                StructMember {
                    name: "x".into(),
                    value: RuntimeVal::Null,
                    constant: false,
                    member_type: VarType::Real
                },
                StructMember {
                    name: "y".into(),
                    value: RuntimeVal::Null,
                    constant: false,
                    member_type: VarType::Real
                },
                StructMember {
                    name: "add".into(),
                    value: get_add_fn(),
                    constant: true,
                    member_type: VarType::Func,
                },
                StructMember {
                    name: "div2".into(),
                    value: get_div2(),
                    constant: true,
                    member_type: VarType::Func,
                }
            ],
            constructor: Some(RuntimeVal::Function {
                args_and_type: vec![("x".into(), VarType::Real), ("y".into(), VarType::Real)],
                body: vec![],
                return_stmt: None,
                return_type: VarType::Void
            })
        });

        proto
    }

    fn get_vec2_members(x: f64, y: f64) -> Rc<RefCell<HashMap<String, RuntimeVal>>> {
        let mem = Rc::new(RefCell::new(HashMap::new()));

        mem.borrow_mut().insert("x".to_string(), RuntimeVal::Real(x));
        mem.borrow_mut().insert("y".to_string(), RuntimeVal::Real(y));
        mem.borrow_mut().insert("add".to_string(), get_add_fn());
        mem.borrow_mut().insert("div2".to_string(), get_div2());

        mem
    }

    fn get_sat_proto() -> Rc<StructPrototype> {
        let proto = Rc::new(StructPrototype {
            name: "Sat".into(),
            members: vec![
                StructMember {
                    name: "orbite".into(),
                    value: RuntimeVal::Null,
                    constant: false,
                    member_type: VarType::Struct("vec2".into())
                }
            ],
            constructor: Some(RuntimeVal::Function {
                args_and_type: vec![("orbite".into(), VarType::Any)],
                body: vec![],
                return_stmt: None,
                return_type: VarType::Void
            })
        });

        proto
    }

    fn get_sat_members(x: f64, y: f64) -> Rc<RefCell<HashMap<String, RuntimeVal>>> {
        let mem = Rc::new(RefCell::new(HashMap::new()));
        mem.borrow_mut().insert(
            "orbite".to_string(),
            RuntimeVal::Structure {
                prototype: get_vec2_proto(),
                members: get_vec2_members(x, y)
            }
        );

        mem
    }

    #[test]
    fn member_call_ident_first() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Identifier first in the call
        let add_code = "mars.pos.x";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Real(11.)
        );
    }

    #[test]
    fn member_call_array_first() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Array first in the call
        let add_code = "satelites[-1].orbite";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Structure {
                prototype: get_vec2_proto(),
                members: get_vec2_members(-54765., 10345.)
            }
        );

        // Array first in the call
        let add_code = "satelites[-1].orbite.y";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Real(10345.)
        );
    }

    #[test]
    fn member_call_fn_call_last() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Fn call at the end of member call (this add fn is stragne, see code)
        let add_code = "sol_sys.main_planet.pos.add(5, 6)";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Real(7.)
        );
    }

    #[test]
    fn member_call_array_call_last() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Fn call at the end of member call (this add fn is stragne, see code)
        let add_code = "sol_sys.main_planet.sat[1]";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let sat_struct = RuntimeVal::Structure {
                prototype: get_sat_proto(),
                members: get_sat_members(-5., 999.)
        };

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            sat_struct
        );
    }

    #[test]
    fn member_call_fn_call_last_self_mut() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Fn call at the end modifying 'self' values
        let add_code = "
            sol_sys.main_planet.pos.div2()
            sol_sys.main_planet.pos.x
        ";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Real(0.5)
        );

        let add_code = "sol_sys.main_planet.pos.y";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Real(0.5)
        );
    }

    #[test]
    fn member_call_array_calls_inside() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Fn call at the end modifying 'self' values
        let add_code = "sol_sys.planets[-1].sat[1].orbite.add(1, 6)";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            RuntimeVal::Real(1.)
        );
    }

    #[test]
    fn member_call_array_calls_inside_fn_call_mut() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(member_call_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let interp = Interpreter {};
        let mut env = create_global_env();

        interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap();

        // Fn call at the end modifying 'self' values
        let add_code = "
            sol_sys.planets[-1].sat[1].orbite.div2()
            sol_sys.planets[-1].sat[1]
        ";

        lexer.tokenize(add_code.to_string()).unwrap();
        parser.build_ast(lexer.tokens.clone()).unwrap();

        let sat_struct = RuntimeVal::Structure {
                prototype: get_sat_proto(),
                members: get_sat_members(-2.5, 166.5)
        };

        assert_eq!(
            interp.execute_program(parser.ast_nodes.clone(), &mut env).unwrap(),
            sat_struct
        );
    }
}
