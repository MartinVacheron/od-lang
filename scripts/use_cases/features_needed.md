# Core features

## Morpion
- Enum:
  - Can contain data
  - Exhaustive case patterns
  - Definition of traits for enum
- Structure constructors shorthand for all non priv variables '(..)'           &&
  - Variables will be assigned in the order of declaration                     &&
- Power with '**' operator                                                     &&
- Array creation like [elem; N]                                                &&
- Array var type and subtype for elems                                         &&
- 'err' type with String contained in it                                       &&
- '_' for all other cases in case statements                                   &&
- 'echo' without new line insertion                                            --
- Default value for strcture members                                           &&
- String type                                                                  &&
- String format with '%s', '%d'                                                &&
- Infinite loop with 'loop'                                                    &&
  - Loops must have a 'return' or 'break' statement                            &&
  - Repeat loop with 'reloop'                                                  &&
- Tuples                                                                       &&
  - Constant                                                                   &&
  - Any number of element                                                      &&
  - Access with '[..]'                                                         &&
  - Decomposition: var a, b = (1, 2)  -> a = 1, b = 2                          &&
- Return result, '<type>?'                                                     &&
  - Return an err like 'return err("...")'                                     &&
- Error managment                                                              &&
  - 'try' and 'then' mechanisme                                                &&
  - 'then' is a closure with the variable being the error msg                  &&
- Closures with syntax '<var> {}' or just '<a> stmt'                           &&
- Private stuff (being not the defaut state) with keyword 'priv'               &&
- Loop                                                                         &&
  - for loop creer une copie d'un element                                      &&
- Main function to start program                                               &&
  - Implicit type not required 'int?'                                          &&
                                                                        
## Cloth                                                                      
- References with '&'                                                          &&
- Dereference with '*'                                                         &&
- Iterator                                                                     &&
  - Method                                                                     &&
    - Map pour acceder a une copie de l'element pour retourner un autre array  &&
    - Self map pour avoir acces a l'element lui meme: self_map(<a> *a..)       &&
    - Filter to collect a new array of same type                               &&
  - Loop                                                                       &&
    - 'for ... in &...' cree un element reference de l'array itere             &&
- Module                                                                       &&

## File                                                                        
- Error :                                                                      &&
  - 'try'                                                                      &&
    -  accepts function that can and cannot fail (with a warning when using try with can't fail function)              &&
    -  return the type if didn't fail                                                                                  &&
    -  if fail, return the error from current scope                                                                    &&
   -  'else'                                                                                                           &&
      -  Allow to give a defaut value if 'try' fails                                                                   &&
      -  Value in 'else' arm must not be a failable statement to ensure we are working with a correct variable after   &&
   -  'then'                                                                                                           &&
    -  Before function exits, allow to execute statements. But function exits                                          &&
    -  Allow to modify the error message, so 'then' closure must have a reference to the error as argument             &&
    -  If there is an *else* arm, dosen't quit the function, just execute statement                                    &&
                                                                          
## Iterator                                                                        
- Role                                                                        
  - Can impose to redefine method and var with 'redef'                                                                 &&
  - Can impose to bind a variable to another one in the structure with 'bind'                                          &&
                                                                        
## Mean square                                                                        
- Iterator                                                                        
  - Add 'zip' to iterate over 2 elems                                                                                  &&
  - Add 'sum' to sum all elements                                                                                      &&
- Arrays                                                                                                               &&
  - Support infinite dimensions arrays: [][]real                                                                       &&
                                                                        
## Interpreter                                                                        
- Import                                                                                                               &&
  - Use all and keep prefix: 'use lexer:.*'                                                                            &&
  - Use all without prefix: 'use lexer:*'                                                                              &&
  - Use specific: 'use interpreter:Interpreter'                                                                        &&
  - Use from file not at the same location: 'use ast:ASTNode from '../../module/ast'                                   &&
- Auto dereference for read only access:                                                                               &&
  - for example, in 'then <e> echo(e)' instead of 'then <e> echo(*e)'                                                  --
- Iterator                                                                                                             &&
  - Possibility to get the iterator with call to 'iter()'                                                              &&
  - Iterator object has methods like 'next', 'peek'                                                                    &&
- Try                                                                        
  - The 'else' statement can break a loop instead of using giving a default value                                      &&                                            
- While:                                                                        
  - While can handle boolean and error type like: while fn() {}  with fn -> any?                                       &&
  - 'while try fn()' exits if fail                                                                                     &&
  - 'while try fn() {} then <e> { echo(e) }' exits if fail with error use                                              &&
  - 'while fn() {}' exits while loop if an error is encountered                                                        &&
  - 'while fn() {} then <e> { echo(e) }' exits while loop if an error is encountered and error use                     &&
  - While can have an alias for current value returned                                                                 &&
    - 'while fn() @ foo { foo... }' can use its value inside loop                                                      &&
  - Or just var like 'while a = iter.next() {}'                                                                        --
- Constructor                                                                                                          &&
  - Do we need to write new(..) if there is no additional arg and no constructor body?                                 &&
-/!\ To think about /!\ Trait                                                                        
  - Name it interface? pact?                                                                                           --
  - Trait can extend other trait                                                                                       --
- Type unions?                                                                                                         &&
- Trait ToString for enum                                                                        
  - Can use 'self' for case pattern                                                                        
  - If only simple fields (no enum or struct included), print the name of enum field?                                                                        
- Ternary operator                                                                        
  - Inline if else for var assignment, or with '?' and ':' ?                                                                        
- Case:                                                                        
  - Returns the value of the arm but all arms have to return same type                                                                        
  - Can return a 'case' statment directlt like 'return case ...'                                                                        
  - A return in a 'case' statment exits scope                                                                          &&
- Function return                                                                                                      &&
  - If a function returns 'void?' and dosen't have a return statment, we assume to get 'null' value                    &&
                                                                        
                                                                        
# V2                                                                        
- Morpion                                                                        
  - Not used here finally but struct with not named fields: struct Foo(int, real)                                                                        
  - Acces with: var a = Foo(4, 4.)     a.0                                                                        