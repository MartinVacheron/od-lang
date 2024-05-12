use super::{RuntimeVal, ValueError};

#[derive(Clone, Debug)]
pub struct ArrayType {
    pub val: Vec<RuntimeVal>,
}

impl ArrayType {
    pub fn call(&mut self, fn_name: &str, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        match fn_name {
            "set" => self.set(args),
            "get" => self.get(args),
            "get_slice" => self.get_slice(args),
            "push" => self.push(args),
            "pop" => self.pop(args),
            "clear" => self.clear(args),
            _ => Err(ValueError::NonExistentFnOnType(
                fn_name.into(),
                "array".into(),
            )),
        }
    }

    // ----------
    //  Methods
    // ----------

    // Set must have 2 args: index, value
    fn set(&mut self, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        check_nb_args(&args, 2)?;

        let mut args_vec = args.to_vec();
        let val = args_vec.pop().unwrap();

        match args_vec.pop().unwrap() {
            RuntimeVal::Int(nb1) => {
                let id = self.check_and_get_index(nb1)?;
                self.val[id] = val;

                Ok(RuntimeVal::Null)
            }
            arg @ _ => Err(ValueError::WrongArgType(
                1,
                "set".into(),
                "int".into(),
                arg.get_type().to_string()
            )),
        }
    }

    // Get must have 1 argument: index
    fn get(&self, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        check_nb_args(args, 1)?;

        match args.to_vec().pop().unwrap() {
            RuntimeVal::Int(nb) => {
                let id = self.check_and_get_index(nb)?;

                Ok(self.val[id].clone())
            }
            arg @ _ => Err(ValueError::WrongArgType(
                0,
                "get".into(),
                "int".into(),
                arg.get_type().to_string()
            )),
        }
    }

    // Get_slice has 2 optional argument: start, finish
    // Start can't be negative
    fn get_slice(&self, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        check_nb_args(args, 2)?;

        match (&args[0], &args[1]) {
            // Case [:x]
            (RuntimeVal::Null, RuntimeVal::Int(nb)) => {
                if *nb < 0 {
                    Err(ValueError::ArrSliceIdxNeg(*nb))
                } else {
                    let id = self.check_and_get_index(*nb)?;

                    Ok(RuntimeVal::Array(ArrayType {
                        val: self.val[..=id].to_vec(),
                    }))
                }
            }
            // Case [x:]
            (RuntimeVal::Int(nb), RuntimeVal::Null) => {
                if *nb < 0 {
                    Err(ValueError::ArrSliceIdxNeg(*nb))
                } else {
                    let id = self.check_and_get_index(*nb)?;

                    Ok(RuntimeVal::Array(ArrayType {
                        val: self.val[id..].to_vec(),
                    }))
                }
            }
            (RuntimeVal::Int(nb1), RuntimeVal::Int(nb2)) => {
                if *nb1 < 0 {
                    Err(ValueError::ArrSliceIdxNeg(*nb1))
                } else if *nb2 < 0 {
                    Err(ValueError::ArrSliceIdxNeg(*nb2))
                } else {
                    let id1 = self.check_and_get_index(*nb1)?;
                    let id2 = self.check_and_get_index(*nb2)?;

                    Ok(RuntimeVal::Array(ArrayType {
                        val: self.val[id1..=id2].to_vec(),
                    }))
                }
            }
            arg @ _ => {
                // We have to know which one of the two arg is wrong
                // If first one is a int, it's the second one the wrong
                let id_wrong = if let RuntimeVal::Int(_) = arg.0 { 2 } else { 1 };

                Err(ValueError::WrongArgType(
                    id_wrong,
                    "get_slice".into(),
                    "int".into(),
                    args[id_wrong as usize].get_type().to_string()
                ))
            },
        }
    }

    fn push(&mut self, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        check_nb_args(args, 1)?;

        self.val.push(args.to_vec().pop().unwrap());
                
        Ok(RuntimeVal::Null)
    }

    fn pop(&mut self, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        check_nb_args(args, 0)?;

        match self.val.pop() {
            Some(v) => Ok(v),
            None => Err(ValueError::EmptyArrayPop),
        }
    }

    fn clear(&mut self, args: &[RuntimeVal]) -> Result<RuntimeVal, ValueError> {
        check_nb_args(args, 0)?;

        self.val.clear();

        Ok(RuntimeVal::Null)
    }

    // Checks the value of index and convert the < 0 ones
    pub fn check_and_get_index(&self, idx: i64) -> Result<usize, ValueError> {
        // If index is < 0, we convert it to fetch from the bottom
        // Ex: a = [1, 2, 3, 4]     len() = 4
        // a[-1] <=> a[3] <=> a[len() + -1]
        // a[-2] <=> a[2] <=> a[len() + -1]
        let id = if idx >= 0 { idx } else { self.val.len() as i64 + idx };

        // Out of bound array access. If still < 0  after remaping, out too
        if id < 0 || id as usize > self.val.len() - 1 {
            return Err(ValueError::ArrayOverIndexing(idx as isize, self.val.len()));
        }

        Ok(id as usize)
    }
}

fn check_nb_args(args: &[RuntimeVal], nb_expected: usize) -> Result<(), ValueError> {
    if args.len() == nb_expected {
        Ok(())
    } else {
        Err(ValueError::WrongArgsSize(nb_expected, args.len()))
    }
}
