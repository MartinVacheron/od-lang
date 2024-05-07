use thiserror::Error;

use super::values::RuntimeVal;

#[derive(Debug, PartialEq, Error)]
pub enum NativeFnError {
    #[error("Function {0}: expected {1} arguments, found {2}")]
    WrongArgNumber(String, usize, usize),

    #[error("Function {0}: argument {1} should be of type {2}")]
    WrongArgType(String, usize, String),
}

// Display the values. Can accpet any number of parameters
pub fn native_print(args: &[RuntimeVal]) -> Result<RuntimeVal, NativeFnError> {
    if args.len() > 0 {
        for arg in args {
            println!("{}", arg);    
        }
    } else {
        println!("");
    }

    Ok(RuntimeVal::Null)
}


// TODO: documenter le cast implicit
pub fn native_cos(args: &[RuntimeVal]) -> Result<RuntimeVal, NativeFnError> {
    check_args_number("cos", args, 1)?;

    match args[0] {
        RuntimeVal::Real(val) => Ok(RuntimeVal::Real(val.cos())),
        RuntimeVal::Int(val) => {
            let real_val = f64::from(val as i32);

            Ok(RuntimeVal::Real(real_val.cos()))
        },
        _ => Err(NativeFnError::WrongArgType(
                "cos".to_string(),
                1,
                "real".to_string(),
            ))
    }
}

// Test functions
pub fn assert_eq(args: &[RuntimeVal]) -> Result<RuntimeVal, NativeFnError> {
    check_args_number("assert_eq", args, 2)?;

    Ok(RuntimeVal::Bool(args[0] == args[1]))
}

pub fn assert_neq(args: &[RuntimeVal]) -> Result<RuntimeVal, NativeFnError> {
    check_args_number("assert_neq", args, 2)?;

    Ok(RuntimeVal::Bool(args[0] != args[1]))
}

// --------
// Helpers
// --------
fn check_args_number(
    fn_name: &str,
    args: &[RuntimeVal],
    nb_expected: usize,
) -> Result<(), NativeFnError> {
    if args.len() != nb_expected {
        return Err(NativeFnError::WrongArgNumber(
            fn_name.to_string(),
            nb_expected,
            args.len(),
        ));
    }

    Ok(())
}