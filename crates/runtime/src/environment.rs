use colored::*;
use std::rc::Rc;
use std::result::Result;
use std::{
    cell::RefCell,
    collections::{
        hash_map::Entry::{Occupied, Vacant},
        HashMap,
    },
};
use thiserror::Error;

use crate::frontend::{
    ast::StatementKind,
    parser::VarType
};

use super::native_functions::{assert_eq, assert_neq, native_cos, native_print};
use super::values::{AssignType, RuntimeVal, StructPrototype, StructMember, ValueError};


#[derive(Error, Debug, PartialEq)]
pub enum EnvError {
    #[error("Variable is already declared: {0}")]
    AlreadyDeclaredVar(String),

    #[error("Undeclared variable: {0}")]
    UndeclaredVar(String),

    #[error("Assign to undeclared variable: {0}")]
    AssignToUndeclared(String),

    #[error("Assign to {} variable: {0}", "constant".red().bold())]
    AssignToConst(String),

    #[error("Structure is already declared: {0}")]
    AlreadyDeclaredStruct(String),

    #[error("Member {0} has been declared twice in structure: {1}")]
    AlreadyDeclaredMemberInStruct(String, String),

    #[error("{} undeclared structure: {0}", "Error".red().bold())]
    UndeclaredStruct(String),

    #[error("{} during self creation: {0}", "Error".red().bold())]
    SelfCreation(String),

    #[error("{0}")]
    EnvFromValue(#[from] ValueError),

    #[error("array function call not on an array: -{0}-")]
    ArrayFnCallNotOnArr(String),

    #[error("assigning value to variable -{0}-: {1}")]
    WrongValType(String, String),
}

enum EnvElem {
    Variable,
    StructPrototype,
}

pub fn create_global_env<'a>() -> Env<'a> {
    let mut env = Env::new(None);

    // Types
    env.declare_var("null".into(), RuntimeVal::Null, true, VarType::Any)
        .unwrap();
    env.declare_var("true".into(), RuntimeVal::Bool(true), true, VarType::Bool)
        .unwrap();
    env.declare_var("false".into(), RuntimeVal::Bool(false), true, VarType::Bool)
        .unwrap();

    // Native functions
    env.declare_var(
        "echo".into(),
        RuntimeVal::NativeFunction {
            func: Rc::new(native_print),
            return_type: VarType::Real
        },
        true,
        VarType::Any,
    )
    .unwrap();

    // FIXME: give the type of the returned value?
    env.declare_var(
        "cos".into(),
        RuntimeVal::NativeFunction {
            func: Rc::new(native_cos),
            return_type: VarType::Real
        },
        true,
        VarType::Func,
    )
    .unwrap();

    env.declare_var(
        "assert_eq".into(),
        RuntimeVal::NativeFunction {
            func: Rc::new(assert_eq),
            return_type: VarType::Bool
        },
        true,
        VarType::Func,
    )
    .unwrap();

    env.declare_var(
        "assert_neq".into(),
        RuntimeVal::NativeFunction {
            func: Rc::new(assert_neq),
            return_type: VarType::Bool,
        },
        true,
        VarType::Func,
    )
    .unwrap();

    env
}

// Cannot derive Debug and PartialEq since closure aren't known at compile time
#[derive(Debug, PartialEq)]
pub struct Env<'a> {
    parent: Option<&'a Env<'a>>,
    vars: HashMap<String, RuntimeVal>,
    constants: Vec<String>,
    any_type_vars: Vec<String>,
    structs_prototypes: HashMap<String, Rc<StructPrototype>>,
}

impl<'a> Env<'a> {
    // Option allow to not have a parent (the global env)
    pub fn new(parent: Option<&'a Env>) -> Self {
        Self {
            parent,
            vars: HashMap::new(),
            constants: Vec::new(),
            any_type_vars: Vec::new(),
            structs_prototypes: HashMap::new(),
        }
    }

    pub fn declare_var(
        &mut self,
        var: String,
        assign_value: RuntimeVal,
        constant: bool,
        var_type: VarType,
    ) -> Result<(), EnvError> {
        // We check if the var is not already in the map
        if let Vacant(e) = self.vars.entry(var.clone()) {
            // We then check for type coherence if variable is typed
            // If variable is a function, the var_type is the return type
            // First checks errors like: var a: int = 5.
            // Second checks for: var a: int   -> not an error
            // Third  checks:  fn add(x, y) -> int {}
            //   where var_type == VarType::Func
            //   it's normal, juste declare the variable in this case
            let val = if assign_value.get_type() != var_type 
                        && assign_value != RuntimeVal::Null
                        && var_type != VarType::Func
            {
                assign_value.try_cast_to(&var_type).map_err(|e| EnvError::WrongValType(var.clone(), e.to_string()))?
            }
            else {
                assign_value
            };

            e.insert(val);
            // If constant, we add it too the list
            if constant {
                self.constants.push(var.clone());
            }

            // We add its type. Check for 'any' type
            if var_type == VarType::Any {
                let _ = self.any_type_vars.push(var);
            }

            Ok(())
        } else {
            Err(EnvError::AlreadyDeclaredVar(var))
        }
    }

    // Fetch the environment in which is declared the variable and return its value
    pub fn lookup_var(&self, var: &String) -> Result<&RuntimeVal, EnvError> {
        let env = self.resolve(var, EnvElem::Variable)?;

        // At this point, we know the env has the var
        Ok(env.vars.get(var).unwrap())
    }

    // We only look in this env because we can't write in parent's one
    pub fn lookup_mut_var(&mut self, var: &String) -> Result<&mut RuntimeVal, EnvError> {
        // We don't know if the env has the var
        match self.vars.get_mut(var) {
            Some(v) => Ok(v),
            None => Err(EnvError::UndeclaredVar(var.clone()))
        }
    }

    // Assign a new value to an existing var
    pub fn assign_var(&mut self, var: &String, assign_type: AssignType) -> Result<(), EnvError> {
        // If variable exists
        if let Occupied(mut e) = self.vars.entry(var.clone()) {
            // And isn't constant
            if self.constants.contains(var) {
                return Err(EnvError::AssignToConst(var.clone()));
            }

            match assign_type {
                AssignType::Replace(val) => {
                    // We get var type
                    let var_type = e.get().get_type();

                    // We then check for type coherence if variable is typed
                    // If type is 'any', we don't cast, variable can be anything
                    let value = if !self.any_type_vars.contains(&var) && var_type != val.get_type() {
                        val.try_cast_to(&var_type).map_err(|e| EnvError::WrongValType(var.clone(), e.to_string()))?
                    } else {
                        val
                    };

                    let _ = e.insert(value);
                }
                AssignType::ArrayModification { index, value } => {
                    // We call se set method on the array
                    if let RuntimeVal::Array(arr) = e.get_mut() {
                        arr.call("set", &[index, value])?;
                    }
                }
                AssignType::FunctionCall { fn_name, args } => {
                    if let RuntimeVal::Array(arr) = e.get_mut() {
                        arr.call(fn_name.as_str(), args.as_slice())?;
                    }
                }
            }

            Ok(())
        } else {
            Err(EnvError::AssignToUndeclared(var.clone()))
        }
    }

    pub fn assign_to_self(&mut self, member: &String, value: RuntimeVal) {
        if let Occupied(mut e) = self.vars.entry("self".into()) {
            if let RuntimeVal::Structure { prototype, members } = e.get_mut() {
                let _ = members.borrow_mut().insert(member.clone(), value);
            }
        }
    }

    // Assign a new value to an existing var that is a sub member (case of struct)
    pub fn assign_struct_var(
        &mut self,
        mut properties: Vec<String>,
        assign_type: AssignType,
    ) -> Result<(), EnvError> {
        // We extract variable name
        let name = properties.remove(0);

        // If it is constant, error
        if self.constants.contains(&name) {
            return Err(EnvError::AssignToConst(name));
        }

        // If variable exists
        if let Occupied(mut e) = self.vars.entry(name.clone()) {
            // We get it
            let val = e.get_mut();

            // We assign the value to it
            val.assign_sub_member(&mut properties, assign_type)?;

            Ok(())
        } else {
            Err(EnvError::AssignToUndeclared(name.clone()))
        }
    }

    // Recursivly check the parent environment to find variable declaration
    fn resolve(&self, name: &String, kind: EnvElem) -> Result<&Env, EnvError> {
        match kind {
            EnvElem::Variable => {
                match self.vars.contains_key(name) {
                    true => Ok(self),
                    false => {
                        // Recursion happens here
                        if let Some(env) = self.parent {
                            env.resolve(name, kind)
                        } else {
                            Err(EnvError::UndeclaredVar(name.to_string()))
                        }
                    }
                }
            }
            EnvElem::StructPrototype => {
                match self.structs_prototypes.contains_key(name) {
                    true => Ok(self),
                    false => {
                        // Recursion happens here
                        if let Some(env) = self.parent {
                            env.resolve(name, kind)
                        } else {
                            Err(EnvError::UndeclaredStruct(name.to_string()))
                        }
                    }
                }
            }
        }
    }

    // Structure declaration
    // TODO: use type informations
    pub fn declare_struct(
        &mut self,
        name: String,
        members: Vec<(String, VarType, bool)>,
        functions: Vec<StatementKind>,
    ) -> Result<(), EnvError> {
        // We check if the var is not already in the map
        if let Vacant(e) = self.structs_prototypes.entry(name.to_owned()) {
            let mut runtime_struct: StructPrototype = Default::default();

            // If insertion returns a value, it was already in
            for member in members {
                if runtime_struct.has_member_named(&member.0) {
                    return Err(EnvError::AlreadyDeclaredMemberInStruct(
                        member.0,
                        name,
                    ))
                }

                // We initialize all values to Null
                let struct_member = StructMember {
                    name: member.0,
                    value: RuntimeVal::Null,
                    constant: member.2,
                    member_type: member.1
                };

                runtime_struct.members.push(struct_member)
            }

            // We do the same for functions, including constructor
            for function in functions {
                if let StatementKind::FnDeclaration {
                    name,
                    args_and_type,
                    body,
                    return_stmt,
                    return_type
                } = function
                {
                    match name.as_str() {
                        "new" => {
                            runtime_struct.constructor = Some(RuntimeVal::Function {
                                args_and_type,
                                body,
                                return_stmt: None,
                                return_type: VarType::Void
                            });
                        }
                        _ => {
                            runtime_struct.members.push(
                                StructMember {
                                    name,
                                    value: RuntimeVal::Function {
                                        args_and_type,
                                        body,
                                        return_stmt,
                                        return_type
                                    },
                                    constant: true,
                                    member_type: VarType::Func
                                }
                            );
                        }
                    }
                }
            }

            // We save the name
            runtime_struct.name = name;

            e.insert(Rc::new(runtime_struct));
            Ok(())
        } else {
            Err(EnvError::AlreadyDeclaredVar(name))
        }
    }

    // Structure lookup
    pub fn lookup_struct_prototype(
        &self,
        struct_name: &String,
    ) -> Result<Rc<StructPrototype>, EnvError>
    {
        let env = self.resolve(struct_name, EnvElem::StructPrototype)?;

        Ok(env.structs_prototypes.get(struct_name).unwrap().clone())
    }

    // Creation of a 'self' object to resolve members call in struct
    // FIXME: give variable their types
    pub fn create_self(
        &mut self,
        struct_proto: Rc<StructPrototype>,
        members: Rc<RefCell<HashMap<String, RuntimeVal>>>,
    ) -> Result<(), EnvError>
    {
        // We declare the members under self object
        self.declare_var(
            "self".into(),
            RuntimeVal::Structure {
                prototype: struct_proto.clone(),
                members: members.clone(),
            },
            false,
            VarType::Any,
        )
        .map_err(|e| EnvError::SelfCreation(e.to_string()))?;

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_global_env() {
        let env = Env::new(None);
        assert!(env.parent.is_none());
    }

    #[test]
    fn create_child_env() {
        let glob_env = Env::new(None);
        let child = Env::new(Some(&glob_env));
        assert!(child.parent.is_some());
    }

    #[test]
    fn declare_var() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();
        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        assert!(env.vars.contains_key(&var));
    }

    #[test]
    fn declare_already_declared_var() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();
        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        assert!(matches!(
            env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any),
            Err(EnvError::AlreadyDeclaredVar(_))
        ));
    }

    #[test]
    fn resolve_var_in_glob_env() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();

        assert!(!env.resolve(&var, EnvElem::Variable).is_ok());

        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        assert!(env.resolve(&var, EnvElem::Variable).is_ok());
        assert_eq!(env.resolve(&var, EnvElem::Variable), Ok(&env));
    }

    #[test]
    fn resolve_var_in_child_env() {
        let env = Env::new(None);
        let mut sub_env = Env::new(Some(&env));

        let var: String = "position".to_string();

        assert!(!env.resolve(&var, EnvElem::Variable).is_ok());

        sub_env
            .declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        assert!(sub_env.resolve(&var, EnvElem::Variable).is_ok());
        assert_ne!(env.resolve(&var, EnvElem::Variable), Ok(&env));
        assert_eq!(sub_env.resolve(&var, EnvElem::Variable), Ok(&sub_env));
    }

    #[test]
    fn resolve_var_in_glob_env_from_child() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();
        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        assert_eq!(env.resolve(&var, EnvElem::Variable), Ok(&env));

        let sub_env = Env::new(Some(&env));
        assert_eq!(sub_env.resolve(&var, EnvElem::Variable), Ok(&env));
    }

    #[test]
    fn lookup_var_in_glob_env() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();
        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        assert_eq!(env.lookup_var(&var), Ok(&RuntimeVal::Null));
    }

    #[test]
    fn lookup_var_in_glob_env_from_child() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();
        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");

        let sub_env = Env::new(Some(&env));
        assert_eq!(sub_env.lookup_var(&var), Ok(&RuntimeVal::Null));
    }

    #[test]
    fn assign_var() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();
        env.declare_var(var.clone(), RuntimeVal::Null, false, VarType::Any)
            .expect("Should succeed");
        env.assign_var(&var, AssignType::Replace(RuntimeVal::Real(350.)))
            .unwrap();

        assert_eq!(env.lookup_var(&var), Ok(&RuntimeVal::Real(350.)));
    }

    #[test]
    fn assign_to_undeclared_var() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();

        assert!(matches!(
            env.assign_var(&var, AssignType::Replace(RuntimeVal::Real(350.))),
            Err(EnvError::AssignToUndeclared(_))
        ));
    }

    #[test]
    fn assign_to_const_var() {
        let mut env = Env::new(None);
        let var: String = "position".to_string();

        env.declare_var(var.clone(), RuntimeVal::Real(70.), true, VarType::Any)
            .unwrap();

        // Matches allow to test patterns, so no need for the args of enum
        assert!(matches!(
            env.assign_var(&var, AssignType::Replace(RuntimeVal::Real(350.))),
            Err(EnvError::AssignToConst(_))
        ));
    }

    #[test]
    fn declare_struct() {
        let mut env = Env::new(None);
        let struct_name: String = "planet".to_string();
        let membres: Vec<(String, VarType, bool)> = vec![
            ("position".to_string(), VarType::Any, false),
            ("radius".to_string(), VarType::Any, true),
        ];

        env.declare_struct(struct_name.clone(), membres, vec![])
            .unwrap();

        let mut struct_proto_members: Vec<StructMember> = vec![
            StructMember { 
                name: "position".into(), value: RuntimeVal::Null,
                constant: false, member_type: VarType::Any
            },
            StructMember { 
                name: "radius".into(), value: RuntimeVal::Null,
                constant: true, member_type: VarType::Any
            },
        ];

        assert_eq!(
            env.lookup_struct_prototype(&struct_name).unwrap(),
            Rc::new(StructPrototype {
                name: struct_name,
                members: struct_proto_members,
                constructor: None
            })
        );
    }

    #[test]
    fn assign_sub_member() {
        let mut env = Env::new(None);
        let struct_name: String = "planet".to_string();
        let membres: Vec<(String, VarType, bool)> = vec![
            ("position".to_string(), VarType::Any, false),
            ("radius".to_string(), VarType::Any, true),
        ];

        env.declare_struct(struct_name.clone(), membres, vec![])
            .unwrap();

        let mut struct_proto_members: Vec<StructMember> = vec![
            StructMember { 
                name: "position".into(), value: RuntimeVal::Null,
                constant: false, member_type: VarType::Any
            },
            StructMember { 
                name: "radius".into(), value: RuntimeVal::Null,
                constant: true, member_type: VarType::Any
            },
        ];

        assert_eq!(
            env.lookup_struct_prototype(&struct_name).unwrap(),
            Rc::new(StructPrototype {
                name: struct_name,
                members: struct_proto_members,
                constructor: None
            })
        );
    }
}
