use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
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
                member: m1,
                property,
            } => {
                // We check first that we are not just calling a method on an array because it has
                // the same shape as the member calls like: arr.push(5)
                // with 'arr' as member and 'push(5)' as property

                // TODO: peut etre plus utile
                // if let ExpressionKind::Identifier { symbol } = &*m1 {
                //     if let ExpressionKind::FunctionCall { name, args: args_expr } = &*property {
                //         let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

                //         // We take &mut because some methods modify the array
                //         let var = env.lookup_mut_var(&symbol)?;

                //         match var {
                //             RuntimeVal::Array(arr) => {
                //                 return arr.call(name.as_str(), args.as_slice()).map_err(|e| InterpreterError::ArrayGetFnCall(e))
                //             }
                //             // Else, we continue because it is a structure
                //             _ => {}
                //         }
                //     }
                // }

                // We check first for simple cases
                match &*m1 {
                    // Identifier function calls could be either on a struct or an array
                    ExpressionKind::Identifier { symbol } => {
                        match &*property {
                            ExpressionKind::FunctionCall { name, args: args_expr } => {
                                let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

                                // We take &mut because some methods modify the array
                                let var = env.lookup_mut_var(&symbol)?;

                                match var {
                                     RuntimeVal::Array(arr) => arr.call(name.as_str(), args.as_slice()).map_err(|e| InterpreterError::ArrayGetFnCall(e)),
                                     RuntimeVal::Structure { prototype, members } => {
                                       let struct_fn = prototype
                                            .members
                                            .iter()
                                            .filter(|m| &m.name == name)
                                            .collect::<Vec<&StructMember>>();

                                        let struct_fn = struct_fn.first().unwrap();

                                        if let RuntimeVal::Function { args_and_type, body, return_stmt, return_type } = &struct_fn.value {
                                            match self.execute_fn_in_env(
                                                args_and_type.clone(),
                                                args,
                                                name.clone(),
                                                body.clone(),
                                                return_stmt.clone(),
                                                env,
                                            )?
                                            {
                                                Some(res) => Ok(res),
                                                None => Ok(RuntimeVal::Null),
                                            }
                                        } else {
                                            todo!("Struct member not a fn")
                                        }
                                    }
                                    _ => todo!("Error calling fn on neither a struct or an array")
                                }
                            }
                            _ => todo!("Can't be anuthing else than a function call on an identifier member expression")
                        }
                    }
                    ExpressionKind::ArrayCall { name, index } => {
                        let var = env.lookup_var(name)?;

                        match var {
                            arr @ RuntimeVal::Array(_) => {
                                let elem = self.evaluate_array_indexing(arr.clone(), index.clone(), env)?;

                                match elem {
                                    RuntimeVal::Structure { prototype, members } => {
                                        match &*property {
                                            ExpressionKind::Identifier { symbol } => {
                                                match members.borrow().get(symbol) {
                                                    Some(m) => Ok(m.clone()),
                                                    None => todo!("Non existing member on struct in array")
                                                }
                                            }
                                            _ => todo!("Implement function call on array element in member call")
                                        }
                                    }
                                    _ => todo!("Error, element of array is not a struct, can't member assign on it")
                                }
                            },
                            _ => todo!("Error on array high level member call")
                        }
                    }
                    // Nested ones
                    ExpressionKind::MemberCall { .. } => {
                        self.resolve_assign_mem_call(&m1, &property, env, RuntimeVal::Null)
                    }
                    _ => todo!("Implement other member calls")
                }

                // TODO: Check if need to go deeper because of nested member calls.
                // Otherwise, more simple function
                // self.resolve_assign_mem_call(&m1, &property, env, RuntimeVal::Null)

                // let mut out: RuntimeVal = RuntimeVal::Null;
                // self.resolve_mem_call(&*m1, env, &mut out)?;

                // let mut tmp_env = Env::new(Some(env));

                // match out {
                //     RuntimeVal::Structure { prototype, members } => {
                //         // We extract the property
                //         match *property {
                //             ExpressionKind::Identifier { symbol } => {
                //                 let b = members.borrow();
                                
                //                 match b.get(&symbol) {
                //                     Some(m) => Ok(m.clone()),
                //                     None => Err(InterpreterError::WrongStructMember(prototype.name.clone(), symbol))
                //                 }
                //             },
                //             ExpressionKind::FunctionCall { name, args: args_expr } => {
                //                 // We evaluate each of the argument
                //                 let args = self.evaluate_fn_args_value(args_expr, &mut tmp_env)?;

                //                 tmp_env
                //                     .create_self(prototype.clone(), members.clone())
                //                     .map_err(|e| {
                //                         InterpreterError::SelfInConstructor(
                //                             name.clone(),
                //                             e.to_string(),
                //                         )
                //                     })?;

                //                 let struct_fn = prototype
                //                     .members
                //                     .iter()
                //                     .filter(|m| &m.name == &name)
                //                     .collect::<Vec<&StructMember>>();

                //                 let struct_fn = struct_fn.first().unwrap();

                //                 if let RuntimeVal::Function { args_and_type, body, return_stmt, return_type } = &struct_fn.value {
                //                     match self.execute_fn_in_env(
                //                         args_and_type.clone(),
                //                         args,
                //                         name,
                //                         body.clone(),
                //                         return_stmt.clone(),
                //                         &mut tmp_env,
                //                     )?
                //                     {
                //                         Some(res) => Ok(res),
                //                         None => Ok(RuntimeVal::Null),
                //                     }
                //                 } else {
                //                     todo!("Struct member not a fn")
                //                 }
                //             },
                //             // TODO: Not possible isn't it?
                //             ExpressionKind::ArrayCall { name, index } => {
                //                 let mem_bor = members.borrow();

                //                 let struct_arr = mem_bor
                //                     .iter()
                //                     .filter(|m| m.0 == &name)
                //                     .collect::<Vec<(&String, &RuntimeVal)>>();

                //                 // Check if we found one
                //                 let struct_arr = match struct_arr.len() {
                //                     1 => struct_arr.first().unwrap(),
                //                     _ => todo!()
                //                 };

                //                 self.evaluate_array_indexing(struct_arr.1.clone(), index, env)
                //             }
                //             _ => todo!("Not a fn call in mem call")
                //         }
                //     }
                //     RuntimeVal::Array(mut arr) => {
                //         match *property {
                //             ExpressionKind::FunctionCall { name, args: args_expr } => {
                //                 // We evaluate each of the argument
                //                 let args = self.evaluate_fn_args_value(args_expr, env)?;

                //                 Ok(arr.call(name.as_str(), args.as_slice())?)
                //             }
                //             _ => Err(InterpreterError::ArrayNonMethodCall)
                //         }
                //     }
                //     _ => {
                //         println!("It was: {:?}", out);
                //         todo!("Not a struct or array in mem call")
                //     }
                // }
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

        // If args in the prototype
        match proto.constructor_args.clone() {
            // No argument in constructor
            None => {
                // If some were given while creating the structure, error
                if !args_value.is_empty() {
                    return Err(InterpreterError::StructEmptyConstructor(
                        proto.name.clone(),
                    ));
                }
            }
            // Arguments in constructor
            Some(proto_args) => {
                // If the wrong number of arguments was given, error
                if proto_args.len() != args_value.len() {
                    return Err(InterpreterError::StructConstructorWrongArgNb(
                        proto.name.clone(),
                        proto_args.len(),
                        args_value.len(),
                    ));
                }

                // We iterate over the tuple (arg_name, arg_value)
                // This allow to avoid boilerplate code to the user
                proto_args
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
            }
        };

        // We execute the constructor body
        // We create the self structure. Can't fail
        // We clone the Rc and as it is a RefCell, values updates will
        // directly go to members. No need to get them back
        env.create_self(proto.clone(), members.clone()).map_err(|e| {
            InterpreterError::SelfInConstructor(proto.name.clone(), e.to_string())
        })?;

        // We execute all the statements. We clone them from the Rc ref to own them
        if proto.constructor_body.is_some() {
            for stmt in proto.constructor_body.as_ref().unwrap() {
                let _ = self.interpret_node(stmt.clone(), env)?;
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

    // TODO: use if we know that we are not going to call a function on the last member
    // or there is no array call? Because that's the problem of this fn, we don't get
    // a &mut of the runtimeval at the end so if the fn we should have called was 
    // supposed to modify it, we're screwed. Calling 'get' on an array demands
    // &mut too so maybe only for plain member read-only
    fn resolve_mem_call(
        &self,
        member: &ExpressionKind,
        env: &mut Env,
        out: &mut RuntimeVal
    ) -> Result<(), InterpreterError>
    {
        match member {
            ExpressionKind::MemberCall { member: m1, property } => {
                self.resolve_mem_call(&m1, env, out)?;

                if let RuntimeVal::Structure { members, .. } = out.clone() {
                    match &**property {
                        ExpressionKind::Identifier { symbol } => {
                            *out = members.borrow().get(symbol).unwrap().clone();
                        },
                        ExpressionKind::ArrayCall { name, index } => {
                            let arr = members.borrow().get(name).unwrap().clone();

                            if let RuntimeVal::Array(_) = arr {
                                *out = self.evaluate_array_indexing(arr, index.clone(), env)?;
                            } else {
                                todo!("Error sub memeber array")
                            }
                        }
                        _ => todo!("Not supported sub member call")
                    }
                }
            },
            ExpressionKind::Identifier { symbol } => {
                *out = env.lookup_var(symbol)?.clone();
            },
            _ => todo!("Not supported member call")
        }

        Ok(())
    }

    // TODO: before entering this fn, be sure the prop is either a fn call or array call
    fn resolve_assign_mem_call(
        &self,
        member: &ExpressionKind,
        property: &ExpressionKind,
        env: &mut Env,
        out: RuntimeVal
    ) -> Result<RuntimeVal, InterpreterError>
    {
        let mut all_sub_mem: Vec<ExpressionKind> = vec![];

        let first_var: &mut RuntimeVal;
        let mut tmp_mem: ExpressionKind = member.clone();

        println!("Member when enter: {:#?}", member);

        loop {
            match tmp_mem {
                ExpressionKind::MemberCall { member, property } => {
                    tmp_mem = *member;
                    all_sub_mem.push(*property);
                }
                ExpressionKind::Identifier { ref symbol } => {
                    first_var = env.lookup_mut_var(&symbol)?;
                    break
                }
                _ => panic!("Impossible to be here")
            }
        }
        
        all_sub_mem.reverse();

        println!("First: {:#?}", first_var);
        println!("Chain: {:#?}", all_sub_mem);

        let mut final_members = match first_var {
            RuntimeVal::Structure { members, .. } => {
                members.clone()
            },
            _ => panic!("impossible to be here")
        };

        // if all_sub_mem.len() > 0 {
        //     if let ExpressionKind::Identifier { symbol } = all_sub_mem.last().unwrap() {
        //         let mut bor = final_members.borrow_mut();
        //         last_mem = bor.get_mut(symbol).unwrap();
        //     }
        // }
        for sub_mem in all_sub_mem {
            match sub_mem {
                ExpressionKind::Identifier { symbol } => {
                    if let RuntimeVal::Structure { prototype, members } = final_members.clone().borrow().get(&symbol).unwrap() {
                        final_members = members.clone();
                    }
                }
                _ => panic!("impossible to be here")
            }
        }

        match property {
            ExpressionKind::FunctionCall { name, args: args_expr } => {
                match final_members.borrow_mut().get_mut(name).unwrap() {
                    RuntimeVal::Structure { prototype, members } => {
                        if let RuntimeVal::Function {
                            args_and_type,
                            body,
                            return_stmt,
                            return_type
                        } = members.borrow().get(name).unwrap()
                        {
                            let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

                            match self.execute_fn_in_env(args_and_type.clone(), args, name.clone(), body.clone(), return_stmt.clone(), env)? {
                                Some(r) => Ok(r),
                                None => Ok(RuntimeVal::Null)
                            }
                        } else { panic!("flemme") }
                    }
                    RuntimeVal::Array(arr) => {
                        // We evaluate each of the argument
                        let args = self.evaluate_fn_args_value(args_expr.clone(), env)?;

                        Ok(arr.call(name.as_str(), args.as_slice())?)
                    }
                    _ => Err(InterpreterError::ArrayNonMethodCall)
                }
            }
            _ => panic!("No more choices")
        }
        

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

// Recursivly parse member call. say we parse:
//  ex1: foo.bar.biz.buz
//  ex2: foo
//  ex3: foo[0].bar
fn parse_recurs_mem_call(
    m1: &ExpressionKind,
    prop: &mut Vec<String>,
) -> Result<(), InterpreterError> {
    // Here,:
    //  ex1: m1 is foo.bar.biz
    //  ex2: m1 is foo
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
        // We are at ex1,  m1 = foo and prop = bar
        ExpressionKind::Identifier { symbol } => {
            prop.push(symbol.clone());
            Ok(())
        }
        _ => {
            Err(InterpreterError::WrongStructMemberTypeCall)
        },
    }
}



#[cfg(test)]
mod tests {
    use crate::frontend::ast::{ASTNode, ASTNodeKind, ExpressionKind};
    use crate::interpreter::expr::ArrayIndexing;
    use crate::interpreter::{Interpreter, InterpreterError};
    use crate::{environment::{Env, create_global_env}, values::RuntimeVal};
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
}
