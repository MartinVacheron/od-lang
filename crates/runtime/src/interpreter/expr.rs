use std::borrow::BorrowMut;
use std::cell::{RefCell, RefMut};
use std::collections::{
    HashMap,
    hash_map::Entry::Occupied
};
use std::rc::Rc;

use super::InterpreterError;
use crate::environment::MemberAccess;
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
            ExpressionKind::VarAssignment { assigne, value } => {
                let mut assignment_value = self.evaluate(*value, env)?;

                // If it is a structure, we have to break the link of the Rc otherwise they
                // share same data as a pointer
                if let RuntimeVal::Structure { prototype, members } = assignment_value {
                    assignment_value = RuntimeVal::Structure {
                            prototype: prototype.clone(),
                            members: Rc::new(RefCell::new(members.borrow().clone()))
                    };
                }

                match *assigne {
                    ExpressionKind::Identifier { symbol } => {
                        // We clone it to return the value too, in case of x = y = z = 5 for ex
                        env.assign_var(&symbol, AssignType::Replace(assignment_value.clone()))?;

                        Ok(assignment_value)
                    }
                    ExpressionKind::MemberCall { member, property } => {
                        let (members, proto) = self.get_member_call_last_struct(&*member, env)?;
                        
                        // Here, we clone the value, no need to return a reference
                        let assign_clone = assignment_value.clone();

                        match &*property {
                            ExpressionKind::Identifier { symbol } => {
                                match members.borrow_mut().entry(symbol.clone()) {
                                    Occupied(mut e) => { e.insert(assignment_value); },
                                    _ => return Err(InterpreterError::StructMemberNotFound(proto.name.clone(), symbol.clone()))
                                }
                            }
                            ExpressionKind::ArrayCall { name, index } => {
                                let mut tmp_bor = members.borrow_mut();
                                let rv = tmp_bor.get_mut(name).ok_or(InterpreterError::StructMemberNotFound(proto.name.clone(), name.clone()) )?;

                                let idx = self
                                    .get_array_single_index(&index, env)
                                    .map_err(|_| InterpreterError::ArraySliceAssign(name.clone()))?;

                                match rv {
                                    RuntimeVal::Array(arr) => {
                                        arr.call("set", &[RuntimeVal::Int(idx), assignment_value])?;
                                    }
                                    _ => return Err(InterpreterError::NonArrayIndexing(name.clone()))
                                }
                            },
                            _ => return Err(InterpreterError::WrongStructMemberAssignCall)
                        }

                        Ok(assign_clone)
                    }
                    ExpressionKind::ArrayCall { name, index } => {
                        let idx = self
                            .get_array_single_index(&index, env)
                            .map_err(|_| InterpreterError::ArraySliceAssign(name.clone()))?;

                        let rv = env.lookup_mut_var(&name)?;

                        match rv {
                            RuntimeVal::Array(arr) => {
                                arr.call("set", &[RuntimeVal::Int(idx), assignment_value])?;
                            }
                            _ => return Err(InterpreterError::NonArrayIndexing(name.clone()))
                        }

                        Ok(RuntimeVal::Null)
                    }
                    _ => Err(InterpreterError::WrongStructMemberAssignCall),
                }
            }
            ExpressionKind::MemberCall {
                member,
                property,
            } => {
                /*
                
                    |> Test
                
                 */
                let obj = self.evaluate(*member.clone(), env)?;

                match obj {
                    RuntimeVal::StructTmp(tmp_str) => {
                        tmp_str.members.borrow_mut().get(k)
                    }
                }

                
                /*
                
                    |> Test
                
                 */
                let foo = self.get_member_call_chain(&*member, env);
                println!("Test recurse: {:?}", foo);

                println!("Found in env: {:?}", env.get_struct_sub_member(foo.unwrap()));



                // We resolve the member call to have the last element to then manage the property
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

                        let struct_fn = struct_fn.first().ok_or(InterpreterError::StructFnNotFound(proto.name.clone(), name.clone()))?;

                        println!("\nBefore fn call on member call expr");
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
                            return Err(InterpreterError::FnCallOnNonFnMember(name.clone(), proto.name.clone()))
                        }
                    }
                    ExpressionKind::Identifier { symbol } => {
                        Ok(
                            members
                                .borrow()
                                .get(symbol)
                                .ok_or(InterpreterError::StructMemberNotFound(proto.name.clone(), symbol.clone()))?
                                .clone()
                        )
                    }
                    ExpressionKind::ArrayCall { name, index } => {
                        let rv = members
                            .borrow()
                            .get(name)
                            .ok_or(InterpreterError::StructMemberNotFound(proto.name.clone(), name.clone()))?
                            .clone();

                        match rv {
                            RuntimeVal::Array(_) => {
                                self.evaluate_array_indexing(&name, rv, index.clone(), env)
                            }
                            _ => Err(InterpreterError::NonArrayIndexing(name.clone()))
                        }
                    },
                    _ => Err(InterpreterError::WrongMemberTypeCall)
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

                /* 
                   ---------------------
                    |> Constructor call
                   ---------------------
                */

                // We check if this is a structure that we are calling
                let proto = env.lookup_struct_prototype(&name);
                if let Ok(p) = proto {
                    return self.create_structure(p.clone(), args, &mut tmp_env)
                }

                let func = tmp_env.lookup_var(&name)?.clone();

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
                    // TODO: check type returned
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

                self.evaluate_array_indexing(&name, arr, index, env)
            }
            ExpressionKind::EmptyStructLiteral { name } => {
                let proto = env.lookup_struct_prototype(&name)?;

                Ok(RuntimeVal::PlaceholderStruct { prototype: proto.clone() })
            }
        }
    }

    // Evaluates an array call
    fn evaluate_array_indexing(
        &self,
        name: &String,
        array: RuntimeVal,
        index: ArrayIndexing,
        env: &mut Env
    ) -> Result<RuntimeVal, InterpreterError>
    {
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
                            .map_err(|e| InterpreterError::ArrayGetFnCall(e))?
                        )
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
                            .map_err(|e| InterpreterError::ArrayGetFnCall(e))?
                        )
                    }
                }
            }
            // Not an array
            _ => Err(InterpreterError::NonArrayIndexing(name.clone())),
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
        // TODO: add test
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
    fn get_member_call_last_struct(
        &self,
        member: &ExpressionKind,
        env: &mut Env
    ) -> Result<(
            Rc<RefCell<HashMap<String, RuntimeVal>>>,
            Rc<StructPrototype>
        ), InterpreterError>
    {
        let first_var: &mut RuntimeVal;
        let first_var_name: String;
        let mut all_sub_mem: Vec<ExpressionKind> = vec![];

        let mut tmp_mem: ExpressionKind = member.clone();

        loop {
            match tmp_mem {
                // While the member is a sub member call, we continue
                ExpressionKind::MemberCall { member, property } => {
                    tmp_mem = *member;
                    // And we add the property to the list to resolve later
                    all_sub_mem.push(*property);
                }
                // Else, we extract the the first value and we quit the loop
                _ => {
                    (first_var, first_var_name) = self.get_mem_call_first_val(tmp_mem, env)?;
                    break
                }
            }
        }

        println!("\n\nFirst var name: {}, first var: {:?}", first_var_name, first_var);

        // Members are in reverse order   
        all_sub_mem.reverse();

        let mut final_proto;
        let mut final_members;

        match first_var {
            RuntimeVal::Structure { prototype, members } => {
                final_proto = prototype.clone();
                final_members = members.clone();
            },
            _ => return Err(InterpreterError::NonStructMemberCall(first_var_name))
        };

        println!("\nGot first var OK");

        for sub_mem in all_sub_mem {
            match sub_mem {
                ExpressionKind::Identifier { symbol } => {
                    match final_members
                            .clone()
                            .borrow()
                            .get(&symbol)
                            .ok_or(InterpreterError::StructMemberNotFound(final_proto.name.clone(), symbol.clone()))?
                    {
                        RuntimeVal::Structure { prototype, members } => {
                            final_proto = prototype.clone();
                            final_members = members.clone();
                        }
                        _ => return Err(InterpreterError::NonStructMemberCall(first_var_name))
                    }
                }
                ExpressionKind::ArrayCall { name, index } => {
                    let idx = self.get_array_single_index(&index, env)?;

                    match final_members
                            .clone()
                            .borrow_mut()
                            .get_mut(&name)
                            .ok_or(InterpreterError::StructMemberNotFound(final_proto.name.clone(), name.clone()))?
                    {
                        RuntimeVal::Array(arr) => {
                            match arr.call("get", &[RuntimeVal::Int(idx)])? {
                                RuntimeVal::Structure { prototype, members } => {
                                    final_proto = prototype.clone();
                                    final_members = members.clone();
                                },
                                _ => return Err(InterpreterError::MemberCallArrayNoStructReturn(name.clone()))
                            }
                        },
                        _ => return Err(InterpreterError::NonArrayIndexing(name.clone()))
                    }
                }
                ExpressionKind::FunctionCall { name, args } => todo!("Support function call in member call chains"),
                _ => return Err(InterpreterError::WrongMemberTypeCall)
            }
        }

        Ok((final_members, final_proto))
    }



    // Extracts the first value of a member call to start iterating over all the sub members
    fn get_mem_call_first_val<'a>(
        &self,
        first_expr: ExpressionKind,
        env: &'a mut Env
    ) -> Result<(&'a mut RuntimeVal, String), InterpreterError>
    {
        match first_expr {
            // If member is an identifier, we reached the end, ex: foo.bar....
            ExpressionKind::Identifier { ref symbol } => {
                Ok((env.lookup_mut_var(&symbol)?, symbol.clone()))
            }
            // Same for array, ex: foo[0].bar....
            ExpressionKind::ArrayCall { name, index } => {
                let idx = self.get_array_single_index(&index, env)?;

                match env.lookup_mut_var(&name)? {
                    RuntimeVal::Array(arr) => {
                        // Manages negative indexes
                        let i = arr.check_and_get_index(idx)?;

                        Ok((arr.val.get_mut(i).ok_or(InterpreterError::ArrayElemNotFound(name.clone(), i))?, name))
                    },
                    _ => Err(InterpreterError::NonArrayIndexing(name.clone()))
                }
            }
            // TODO: support first elem as a fn call
            ExpressionKind::FunctionCall { name, args } => todo!("First element of member being a function not implemented"),
            _ => Err(InterpreterError::WrongFirstMemberTypeCall)
        }
    }

    /*
    
        |> Test

    */
    fn get_member_call_chain(&self, member: &ExpressionKind, env: &mut Env) -> Result<Vec<MemberAccess>, InterpreterError> {
        match member {
            // While the member is a sub member call, we continue
            ExpressionKind::MemberCall { member, property } => {
                let mut access = self.get_member_call_chain(&*member, env)?;

                match &**property {
                    ExpressionKind::Identifier { symbol } => {
                        access.push(MemberAccess::Name(symbol.clone()));
                        Ok(access)
                    }
                    ExpressionKind::ArrayCall { name, index } => {
                        let idx = self.get_array_single_index(&index, env)?;
                        access.push(MemberAccess::ArrayIndex(name.clone(), idx));

                        Ok(access)
                    }
                    _ => panic!()
                }
            }
            ExpressionKind::Identifier { symbol } => {
                Ok(vec![MemberAccess::Name(symbol.clone())])
            }
            // Else, we extract the the first value and we quit the loop
            _ => panic!()
        }
    }




    // Get the array index asked only if it's a single value, no slice
    fn get_array_single_index(&self, index: &ArrayIndexing, env: &mut Env) -> Result<i64, InterpreterError> {
        match index {        
            ArrayIndexing::Single(idx) => {
                let index = self.evaluate(*idx.clone(), env)?;

                match index {
                    RuntimeVal::Int(i) => Ok(i),
                    _ => Err(InterpreterError::NonIntArrayIndex)
                }
            },
            _ => Err(InterpreterError::ArraySliceMemCall)
        }
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

    const MEMBER_CALL_CODE: &str = "
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

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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
            RuntimeVal::Real(17.)
        );
    }

    #[test]
    fn member_call_array_call_last() {
        let mut lexer = Lexer::default();
        let mut parser = Parser::default();

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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
            RuntimeVal::Real(5.5)
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

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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

        lexer.tokenize(MEMBER_CALL_CODE.to_string()).unwrap();
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
