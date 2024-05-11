use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::ops::{Add, Sub, Mul, Div, Rem};
use std::{cell::RefCell, collections::HashMap};

mod array;
mod value_errors;

use array::ArrayType;
pub use value_errors::ValueError;
use frontend::parser::VarType;

use super::native_functions::NativeFnError;
use super::{ASTNodeKind, ExpressionKind};


#[derive(Clone)]
pub enum RuntimeVal {
    Null,
    Int(i64),
    Real(f64),
    Bool(bool),
    Array(ArrayType),
    // Members are Rc RefCell so functions can have access to them without
    // copying, modifying and reassign by copying.
    Structure {
        prototype: Rc<StructPrototype>,
        members: Rc<RefCell<HashMap<String, RuntimeVal>>>,
    },
    PlaceholderStruct {
        prototype: Rc<StructPrototype>,
    },
    NativeFunction {
        func: Rc<dyn Fn(&[RuntimeVal]) -> Result<RuntimeVal, NativeFnError>>,
        return_type: VarType,
    },
    Function {
        args_and_type: Vec<(String, VarType)>,
        body: Vec<ASTNodeKind>,
        return_stmt: Option<ExpressionKind>,
        return_type: VarType,
    },
}

impl Debug for RuntimeVal {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            RuntimeVal::Null => write!(f, "null"),
            RuntimeVal::Int(nb) => write!(f, "{}", nb),
            RuntimeVal::Real(nb) => write!(f, "{}", nb),
            RuntimeVal::Array(arr) => write!(f, "{:?}", arr),
            RuntimeVal::Bool(b) => write!(f, "{}", b),
            RuntimeVal::Structure { prototype, members } => write!(
                f,
                "Structure of type: {} \nMembers: {:?}",
                prototype.name,
                members
            ),
            RuntimeVal::PlaceholderStruct { prototype } => write!(
                f,
                "Placeholder structure for type: {}",
                prototype.name
            ),
            RuntimeVal::NativeFunction { .. } => write!(f, "native function"),
            RuntimeVal::Function { .. } => write!(f, "function"),
        }
    }
}

impl Display for RuntimeVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeVal::Null => write!(f, "null"),
            RuntimeVal::Int(nb) => write!(f, "{}", nb),
            RuntimeVal::Real(nb) => write!(f, "{}", nb),
            RuntimeVal::Array(arr) => {
                if arr.val.len() > 0 {
                    let mut is_struct_array: bool = false;

                    write!(f, "[")?;

                    for (idx, val) in arr.val.iter().enumerate() {
                        // If it is a structure, we add a line return for readability
                        let val_fmt = match val {
                            RuntimeVal::Structure { .. } => {
                                // We mark it as struct array
                                is_struct_array = true;

                                // We find all the lines return
                                let mut tmp_fmt = format!("{}", val);

                                let indices = tmp_fmt
                                    .bytes()
                                    .enumerate()
                                    .filter(|(_, b)| *b == b'\n')
                                    .map(|(i, _)| i+1)
                                    .collect::<Vec<_>>();

                                // We add a level of identation at each end line for beautiful printing
                                // We add the number of added char because each insertion increases the
                                // string length
                                for (i, index) in indices.iter().enumerate() {
                                    tmp_fmt.insert(index + i, '\t');
                                }

                                format!("\n\t{}", tmp_fmt)
                            },
                            _ => format!("{}", val)
                        };

                        write!(f, "{}", val_fmt)?;

                        // We write a comma only if there is another value after
                        if idx != arr.val.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }

                    if is_struct_array {
                        write!(f, "\n]")
                    } else {
                        write!(f, "]")
                    }
                } else {
                    write!(f, "[]")
                }
            },
            RuntimeVal::Bool(b) => write!(f, "{}", b),
            RuntimeVal::Structure { prototype, members } => {
                write!(f, "struct: {} {{\n", prototype.name)?;

                let mut members_str:Vec<String> = vec![];
                let mut functions_str:Vec<String> = vec![];

                for (mem_name, mem_val) in members.borrow().iter() {
                    match mem_val {
                        RuntimeVal::Function { .. } => functions_str.push(mem_name.clone()),
                        _ => {
                            let const_kw = if prototype.is_member_const(mem_name) { "const" } else { "var  " };
                            let mem_type = mem_val.get_type();

                            // If structure, we format it to be able to ident it
                            let mut mem_val_fmt = format!("{}", mem_val);

                            // If this is a structure
                            if matches!(mem_type, VarType::Struct(_)) || matches!(mem_type, VarType::Array(_)) {
                                // We find all the lines return
                                let indices = mem_val_fmt
                                    .bytes()
                                    .enumerate()
                                    .filter(|(_, b)| *b == b'\n')
                                    .map(|(i, _)| i+1)
                                    .collect::<Vec<_>>();

                                // We add a level of identation at each end line for beautiful printing
                                // We add the number of added char because each insertion increases the
                                // string length
                                for (i, index) in indices.iter().enumerate() {
                                    mem_val_fmt.insert(index + i, '\t');
                                }
                            }

                            members_str.push(format!("\t{} {}: {} = {}\n", const_kw, mem_name, mem_type, mem_val_fmt));
                        }
                    }
                }

                // We write all members
                for mem_str in members_str {
                    write!(f, "{}", mem_str)?;
                }

                write!(f, "}}")
            },
            RuntimeVal::PlaceholderStruct { prototype } => {
                write!(f, "\nStructure: {}, uninitialized\n", prototype.name)
            },
            RuntimeVal::NativeFunction { .. } => write!(f, "native function"),
            RuntimeVal::Function { .. } => write!(f, "function"),
        }
    }
}

impl PartialEq for RuntimeVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeVal::Null, RuntimeVal::Null) => true,
            (RuntimeVal::Int(a), RuntimeVal::Int(b)) => a == b,
            (RuntimeVal::Int(a), RuntimeVal::Real(b)) => *a as f64 == *b,
            (RuntimeVal::Real(a), RuntimeVal::Int(b)) => *a == *b as f64,
            (RuntimeVal::Real(a), RuntimeVal::Real(b)) => a == b,
            (RuntimeVal::Array(a1), RuntimeVal::Array(a2)) => a1.val == a2.val,
            (RuntimeVal::Bool(a), RuntimeVal::Bool(b)) => a == b,
            (
                RuntimeVal::Structure {
                    prototype: p1,
                    members: m1,
                },
                RuntimeVal::Structure {
                    prototype: p2,
                    members: m2,
                },
            ) => p1 == p2 && m1 == m2,
            (RuntimeVal::NativeFunction { func: fa, .. }, RuntimeVal::NativeFunction { func: fb, .. }) => {
                let args: Vec<RuntimeVal> = vec![RuntimeVal::Real(80.)];
                fa(&args) == fb(&args)
            }
            (
                RuntimeVal::Function { args_and_type: args1, body: b1, return_stmt: ret1, return_type: ret_t1 },
                RuntimeVal::Function { args_and_type: args2, body: b2, return_stmt: ret2, return_type: ret_t2 }
            ) => {
                args1 == args2 && b1 == b2 && ret1 == ret2 && ret_t1 == ret_t2
            }
            _ => false,
        }
    }
}

pub enum AssignType {
    Replace(RuntimeVal),
    ArrayModification {
        index: RuntimeVal,
        value: RuntimeVal,
    },
    FunctionCall {
        fn_name: String,
        args: Vec<RuntimeVal>,
    },
}

impl RuntimeVal {
    // TODO: Add sub type to array
    pub fn get_type(&self) -> VarType {
        match self {
            RuntimeVal::Null => VarType::Any,
            RuntimeVal::Int(_) => VarType::Int,
            RuntimeVal::Real(_) => VarType::Real,
            RuntimeVal::Bool(_) => VarType::Bool,
            RuntimeVal::Array(_) => VarType::Array(Box::new(VarType::Any)),
            RuntimeVal::Structure { prototype, .. } => VarType::Struct(prototype.name.clone()),
            RuntimeVal::PlaceholderStruct { prototype } => VarType::Struct(prototype.name.clone()),
            RuntimeVal::Function { return_type, .. } => return_type.clone(),
            RuntimeVal::NativeFunction { return_type, .. } => return_type.clone(),
        }
    }

    // Return if variable is the type as another
    pub fn is_same_type(&self, other_type: &RuntimeVal) -> bool {
        self.get_type() == other_type.get_type()
    }

    // Allow to try to cast between types
    pub fn try_cast_to(self, to_type: &VarType) -> Result<Self, ValueError> {
        // All possible casts
        match (self, to_type) {
            // Int can be cast to real
            (RuntimeVal::Int(i), VarType::Real) => Ok(RuntimeVal::Real(i as f64)),
            // Any can accept every thing
            (v @ _, VarType::Any) => Ok(v),
            // Null value isn't castable
            // Real can't be casted to anything else
            // Bool neither. Idea, maybe to int?
            // Either struct and functions
            (v @ _, o @ _) => Err(ValueError::NotAllowedCast(v.get_type().to_string(), o.to_string()))
        }
    }

    pub fn new_array(val: Vec<RuntimeVal>) -> Self {
        RuntimeVal::Array(ArrayType { val })
    }

    // TODO: Manage operation between int and float
    pub fn calculate(self, rhs: RuntimeVal, operator: &String) -> Result<RuntimeVal, ValueError> {
        match self {
            // Values that make the operation null
            RuntimeVal::Null => Err(ValueError::UninitVarInOp),
            RuntimeVal::Bool(_) => Err(ValueError::BoolInOperation),
            // If left is a real
            RuntimeVal::Real(lhs) => match rhs {
                // We check the same for right
                RuntimeVal::Null => Err(ValueError::UninitVarInOp),
                RuntimeVal::Bool(_) => Err(ValueError::BoolInOperation),
                // And if right is a number, we compute
                RuntimeVal::Real(rhs) => Ok(RuntimeVal::Real(compute(lhs, rhs, operator)?)),
                RuntimeVal::Int(rhs) => Ok(RuntimeVal::Real(compute(lhs, f64::from(rhs as i32), operator)?)),
                // Else error because of right
                _ => Err(ValueError::BinopOnFunction),
            },
            // If left is an int
            RuntimeVal::Int(lhs) => match rhs {
                // We check the same for right
                RuntimeVal::Null => Err(ValueError::UninitVarInOp),
                RuntimeVal::Bool(_) => Err(ValueError::BoolInOperation),
                // And if right is a number, we compute
                RuntimeVal::Int(rhs) => Ok(RuntimeVal::Int(compute(lhs, rhs, operator)?)),
                RuntimeVal::Real(rhs) => Ok(RuntimeVal::Real(compute(f64::from(lhs as i32), rhs, operator)?)),
                // Else error because of right
                _ => Err(ValueError::BinopOnFunction),
            },
            // Else error because of left
            _ => Err(ValueError::BinopOnFunction),
        }
    }

    // Fetch the sub member as far as it is and assign a new value
    // TODO: remove and replace by new functions in stmt
    pub fn assign_sub_member(
        &mut self,
        members_list: &mut Vec<String>,
        assign_type: AssignType,
    ) -> Result<(), ValueError> {
        // Only structure can have sub-members
        match self {
            RuntimeVal::Structure { prototype, members } => {
                // For errors
                let last = members_list.last().unwrap().clone();

                // For each (key, val) in the members HashMap
                for (name, val) in members.borrow_mut().iter_mut() {
                    // If the key name is the first member to access, we go a level deeper
                    if name == members_list.first().unwrap() {
                        // If it was the last member name to find, it's the last one
                        if members_list.len() == 1 {
                            // We check if it isn't constant
                            if prototype.is_member_const(name) {
                                return Err(ValueError::ConstSubMemberAssignment(name.clone()))
                            }

                            // Now that we have it
                            match assign_type {
                                AssignType::Replace(value) => {
                                    let mem_type = prototype.get_member_type(name)?;

                                    // Check on wrong type if member isn't 'any' type
                                    if mem_type != &value.get_type() && mem_type != &VarType::Any {
                                        // Try to cast it
                                        *val = value
                                                .try_cast_to(mem_type)
                                                .map_err(|e| ValueError::SubMemAssignWrongType(name.clone(), e.to_string()))?
                                    } else {
                                        // If same type, just assign
                                        *val = value
                                    }
                                },
                                AssignType::ArrayModification { index, value } => {
                                    if let RuntimeVal::Array(arr) = val {
                                        arr.call("set", &vec![index, value])?;
                                    }
                                }
                                AssignType::FunctionCall { fn_name, args } => {
                                    if let RuntimeVal::Array(arr) = val {
                                        arr.call(fn_name.as_str(), args.as_slice())?;
                                    }
                                }
                            }

                            return Ok(())
                        }

                        // We remove the one we matched
                        members_list.remove(0);

                        // Else, we fetch a level deeper
                        return Ok(val.assign_sub_member(members_list, assign_type))?
                    }
                }

                Err(ValueError::SubMemberNotFound(last))
            }
            _ => Err(ValueError::NonStructSubMemberAssign),
        }
    }

    // Fetch the sub member as far as it is and return its value
    pub fn get_sub_member(&self, members_list: &mut Vec<String>) -> Result<RuntimeVal, ValueError> {
        // Only structure can have sub-members
        match self {
            RuntimeVal::Structure { members, .. } => {
                // For each (key, val) in the members HashMap
                for (name, val) in members.borrow().iter() {
                    // If the key name is the first member to access, we go a level deeper
                    if name == &members_list[0] {
                        // If it was the last member name to find, it's the last one
                        if members_list.len() == 1 {
                            return Ok(val.clone());
                        }

                        // We remove th eone we matched
                        members_list.remove(0);

                        // Else, we fetch a level deeper
                        return val.get_sub_member(members_list);
                    }
                }

                Err(ValueError::SubMemberNotFound(format!("{members_list:?}")))
            }
            // If it is an array, we return it
            RuntimeVal::Array(_) => {
                return Ok(self.clone());
            }
            _ => Err(ValueError::NonStructVarMemberCall),
        }
    }
}

// Any type that has those methods
fn compute<T>(val1: T, val2: T, operator: &String) -> Result<T, ValueError> 
where T : Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + Rem<Output = T>
{
    match operator.as_str() {
        "+" => Ok(val1 + val2),
        "-" => Ok(val1 - val2),
        "*" => Ok(val1 * val2),
        "/" => Ok(val1 / val2),
        "%" => Ok(val1 % val2),
        _ => Err(ValueError::UndefinedOperator(operator.to_string())),
    }
}


// FIXME: Instead of lots of vec containing the same strings:
//   find a way to use &'a
//    all_names: Vec<String>
//    constants: Vec<&'a String>
//
//   or
//    members HashMap<String, Struct-with-all-info>
#[derive(Debug, PartialEq, Clone, Default)]
pub struct StructPrototype {
    pub name: String,
    pub members: Vec<StructMember>,
    pub constructor: Option<RuntimeVal>
    // pub constructor_args: Option<Vec<(String, VarType)>>,
    // pub constructor_body: Option<Vec<ASTNodeKind>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructMember {
    pub name: String,
    pub value: RuntimeVal,
    pub constant: bool,
    pub member_type: VarType
}


impl StructPrototype {
    // Iterate over all members
    pub fn is_member_const(&self, member_name: &String) -> bool {
        self.members.iter().any(|m| m.constant && &m.name == member_name)
    }

    pub fn has_member_named(&self, member_name: &String) -> bool {
        self.members.iter().any(|m| m.value.get_type() != VarType::Func && &m.name == member_name)
    }

    pub fn get_member_type(&self, member_name: &String) -> Result<&VarType, ValueError> {
        let m: Vec<&StructMember> = self.members.iter().filter(|&m| &m.name == member_name).collect();

        if m.is_empty() {
            return Err(ValueError::SubMemberNotFound(member_name.into()))
        }
        Ok(&m[0].member_type)
    }

    pub fn has_fn_named(&self, fn_name: &String) -> bool {
        self.members.iter().any(|m| m.value.get_type() == VarType::Func && &m.name == fn_name)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculate() {
        let nul = RuntimeVal::Null;
        let boolean = RuntimeVal::Bool(true);
        let number1 = RuntimeVal::Real(80.);
        let number2 = RuntimeVal::Real(10.);

        assert_eq!(
            Err(ValueError::UninitVarInOp),
            nul.clone()
                .calculate(boolean.clone(), &String::from("+"))
        );
        assert_eq!(
            Err(ValueError::UninitVarInOp),
            nul.calculate(number1.clone(), &String::from("+"))
        );
        assert_eq!(
            Err(ValueError::BoolInOperation),
            boolean
                .calculate(number1.clone(), &String::from("+"))
        );
        assert_eq!(
            number1.calculate(number2, &String::from("+")).unwrap(),
            RuntimeVal::Real(90f64)
        );
    }

    #[test]
    fn get_sub_member_value() {
        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("x"), RuntimeVal::Real(10.));
        members.insert(String::from("y"), RuntimeVal::Real(5.));
        let child3 = RuntimeVal::Structure {
            prototype: Rc::new(Default::default()),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("z"), RuntimeVal::Real(42.));
        members.insert(String::from("child3"), child3);
        let child2 = RuntimeVal::Structure {
            prototype: Rc::new(Default::default()),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("a"), RuntimeVal::Real(98.));
        members.insert(String::from("child2"), child2.clone());
        let child = RuntimeVal::Structure {
            prototype: Rc::new(Default::default()),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("mass"), RuntimeVal::Real(500.));
        members.insert(String::from("child"), child);
        let parent = RuntimeVal::Structure {
            prototype: Rc::new(Default::default()),
            members: Rc::new(RefCell::new(members)),
        };

        let mut chain1 = vec!["child".to_string(), "child2".to_string()];
        let mut chain2 = vec!["child".to_string(), "child2".to_string(), "z".to_string()];
        let mut chain3 = vec![
            "child".to_string(),
            "child2".to_string(),
            "child3".to_string(),
            "x".to_string(),
        ];

        assert_eq!(parent.get_sub_member(&mut chain1), Ok(child2));
        assert_eq!(
            parent.get_sub_member(&mut chain2),
            Ok(RuntimeVal::Real(42.))
        );
        assert_eq!(
            parent.get_sub_member(&mut chain3),
            Ok(RuntimeVal::Real(10.))
        );
    }

    #[test]
    fn assign_to_sub_member() {
        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("x"), RuntimeVal::Int(10));
        members.insert(String::from("y"), RuntimeVal::Int(5));
        let child3 = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "child3".into(),
                members: vec![
                    StructMember {
                        name: "x".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Any
                    }
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("z"), RuntimeVal::Int(42));
        members.insert(String::from("child3"), child3);
        let child2 = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "child2".into(),
                members: vec![
                    StructMember {
                        name: "child3".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Any
                    },
                    StructMember {
                        name: "z".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Any
                    }
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("a"), RuntimeVal::Int(98));
        members.insert(String::from("child2"), child2);
        let child = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "child".into(),
                members: vec![
                    StructMember {
                        name: "child2".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Any
                    }
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("mass"), RuntimeVal::Int(500));
        members.insert(String::from("child"), child);
        let mut parent = RuntimeVal::Structure {
            prototype: Rc::new(Default::default()),
            members: Rc::new(RefCell::new(members)),
        };

        let mut chain1 = vec!["child".to_string(), "child2".to_string()];
        let mut chain2 = vec!["child".to_string(), "child2".to_string(), "z".to_string()];
        let mut chain3 = vec![
            "child".to_string(),
            "child2".to_string(),
            "child3".to_string(),
            "x".to_string(),
        ];

        let mut hash: HashMap<String, RuntimeVal> = HashMap::new();
        hash.insert(String::from("i"), RuntimeVal::Int(82));
        hash.insert(String::from("j"), RuntimeVal::Int(51));

        let members = Rc::new(RefCell::new(hash));
        parent
            .assign_sub_member(
                &mut chain3.clone(),
                AssignType::Replace(RuntimeVal::Structure {
                    prototype: Rc::new(Default::default()),
                    members: members.clone(),
                }),
            )
            .unwrap();

        assert_eq!(
            parent.get_sub_member(&mut chain3),
            Ok(RuntimeVal::Structure {
                prototype: Rc::new(Default::default()),
                members: members.clone()
            })
        );

        parent
            .assign_sub_member(
                &mut chain2.clone(),
                AssignType::Replace(RuntimeVal::Int(-6)),
            )
            .unwrap();
        assert_eq!(
            parent.get_sub_member(&mut chain2),
            Ok(RuntimeVal::Int(-6))
        );

        parent
            .assign_sub_member(
                &mut chain1.clone(),
                AssignType::Replace(RuntimeVal::Int(99)),
            )
            .unwrap();
        assert_eq!(
            parent.get_sub_member(&mut chain1),
            Ok(RuntimeVal::Int(99))
        );
    }

    #[test]
    fn assign_to_typed_sub_member() {
        // Here, 'x' is typed any and 'y' int
        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("x"), RuntimeVal::Int(10));
        members.insert(String::from("y"), RuntimeVal::Real(5f64));
        let child3 = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "child3".into(),
                members: vec![
                    StructMember {
                        name: "x".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Any
                    },
                    StructMember {
                        name: "y".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Real
                    },
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("z"), RuntimeVal::Bool(true));
        members.insert(String::from("child3"), child3);
        let child2 = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "child2".into(),
                members: vec![
                    StructMember {
                        name: "child3".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Struct("child".into())
                    },
                    StructMember {
                        name: "z".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Bool
                    }
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("a"), RuntimeVal::Real(98f64));
        members.insert(String::from("child2"), child2.clone());
        let child = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "child".into(),
                members: vec![
                    StructMember {
                        name: "child2".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Struct("child2".into())
                    },
                    StructMember {
                        name: "a".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Real
                    }
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let mut members: HashMap<String, RuntimeVal> = HashMap::new();
        members.insert(String::from("mass"), RuntimeVal::Int(500));
        members.insert(String::from("child"), child);
        let mut parent = RuntimeVal::Structure {
            prototype: Rc::new(StructPrototype {
                name: "parent".into(),
                members: vec![
                    StructMember {
                        name: "child".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Struct("child".into())
                    }
                ],
                ..Default::default()
            }),
            members: Rc::new(RefCell::new(members)),
        };

        let chain: Vec<String> = vec!["child".into()];
        let chain1: Vec<String> = vec!["child".into(), "child2".into()];
        let chain2: Vec<String> = vec!["child".into(), "child2".into(), "z".into()];
        let mut chain3: Vec<String> = vec!["child".into(), "child2".into(), "child3".into(), "x".into()];
        let mut chain4: Vec<String> = vec!["child".into(), "child2".into(), "child3".into(), "y".into()];

        let mut hash: HashMap<String, RuntimeVal> = HashMap::new();
        hash.insert(String::from("i"), RuntimeVal::Int(82));
        hash.insert(String::from("j"), RuntimeVal::Int(51));

        let val_real = RuntimeVal::Real(23f64);
        let val_bool = RuntimeVal::Bool(false);

        let members = Rc::new(RefCell::new(hash));
        // Replacing 'x' which is any
        parent
            .assign_sub_member(
                &mut chain3.clone(),
                AssignType::Replace(RuntimeVal::Structure {
                    prototype: Rc::new(Default::default()),
                    members: members.clone(),
                }),
            )
            .unwrap();

        assert_eq!(
            parent.get_sub_member(&mut chain3),
            Ok(RuntimeVal::Structure {
                prototype: Rc::new(Default::default()),
                members: members.clone()
            })
        );

        // Replacing 'y' which is int
        assert!(matches!(
            parent.assign_sub_member(
                    &mut chain4.clone(),
                    AssignType::Replace(RuntimeVal::Structure {
                        prototype: Rc::new(Default::default()),
                        members: members.clone()
                    })
                ),
            Err(ValueError::SubMemAssignWrongType(..))
        ));

        // Replacing 'y' which is real
        parent.assign_sub_member(
                &mut chain4.clone(),
                AssignType::Replace(val_real.clone()),
        ).unwrap();

        assert_eq!(
            parent.get_sub_member(&mut chain4),
            Ok(val_real.clone())
        );

        // Replacing 'z' which is bool
        parent.assign_sub_member(
                &mut chain2.clone(),
                AssignType::Replace(val_bool.clone()),
        ).unwrap();

        assert_eq!(
            parent.get_sub_member(&mut chain2.clone()),
            Ok(val_bool)
        );

        // Replacing 'z' which is bool
        assert!(matches!(
            parent.assign_sub_member(
                    &mut chain2.clone(),
                    AssignType::Replace(val_real.clone())
            ),
            Err(ValueError::SubMemAssignWrongType(..))
        ));

        // Replacing 'child2' which is struct type child2
        assert!(matches!(
            parent.assign_sub_member(
                    &mut chain1.clone(),
                    AssignType::Replace(RuntimeVal::Int(99)),
            ),
            Err(ValueError::SubMemAssignWrongType(..))
        ));

        // Replacing with struct of type child2
        let mut hash: HashMap<String, RuntimeVal> = HashMap::new();
        hash.insert(String::from("a"), RuntimeVal::Int(82));
        hash.insert(String::from("b"), RuntimeVal::Bool(true));

        let members = Rc::new(RefCell::new(hash));

        let replace_struct = RuntimeVal::Structure {
            prototype: StructPrototype {
                name: "child".into(),
                members: vec![
                    StructMember {
                        name: "a".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Int
                    },
                    StructMember {
                        name: "b".into(), value: RuntimeVal::Null,
                        constant: false, member_type: VarType::Bool
                    }
                ],
                ..Default::default()
            }.into(),
            members
        };

        parent.assign_sub_member(
                &mut chain.clone(),
                AssignType::Replace(replace_struct.clone()),
        ).unwrap();

        assert_eq!(
            parent.get_sub_member(&mut chain.clone()),
            Ok(replace_struct)
        );

        let mut new_chain: Vec<String> = vec!["child".into(), "a".into()];
        let new_chain1: Vec<String> = vec!["child".into(), "b".into()];

        // Act on new typed struct members
        parent.assign_sub_member(
                &mut new_chain.clone(),
                AssignType::Replace(RuntimeVal::Int(-22)),
        ).unwrap();

        assert_eq!(
            parent.get_sub_member(&mut new_chain),
            Ok(RuntimeVal::Int(-22))
        );

        assert!(matches!(
            parent.assign_sub_member(
                    &mut new_chain1.clone(),
                    AssignType::Replace(RuntimeVal::Int(99)),
            ),
            Err(ValueError::SubMemAssignWrongType(..))
        ));
    }
}
