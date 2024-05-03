use colored::*;
use std::{
    fs,
    io::{self, Write},
    process,
};
use clap::Parser as ClapParser;

extern crate frontend;
extern crate runtime;

use frontend::lexer::Lexer;
use frontend::parser::Parser;
use runtime::{
    environment::{
        create_global_env,
        Env
    },
    interpreter::Interpreter
};


// --------
//   CLI
// --------

#[derive(ClapParser)]
#[command(version)]
#[command(about="Interpreter for ODScript")]
struct CLI {
    #[arg(short, long)]
    /// Path to the file to parse
    file: Option<String>,

    /// Interactive mode after interpreting a file
    #[arg(short, long)]
    inter: bool,

    /// Prints the AST tree
    #[arg(short, long)]
    ast_print: bool,

    /// Test mode. If argument is 'all', it will test all files in test directory
    #[arg(short, long)]
    test: Option<String>,
}


fn open_file(file_path: &str) -> String {
    match fs::read_to_string(file_path) {
        Ok(code) => code,
        Err(e) => {
            panic!("Error opening script file, {e}");
        }
    }
}

fn interpret_file(file_name: String, env: &mut Env, cli: &CLI) {
    println!("\nReading source file {}...", file_name.green());
    let source_code = open_file(file_name.as_str());

    interpretation_sequence(source_code, env, cli);
}

fn interpretation_sequence(code: String, env: &mut Env, cli: &CLI) {
    let mut lexer: Lexer = Default::default();
    let mut parser: Parser = Default::default();
    let interp = Interpreter {};

    match lexer.tokenize(String::from(code)) {
        Ok(_) => {}
        Err(e) => println!("{e}"),
    };

    // FIXME: instead of clone, use mem::replace
    let _ = parser
        .build_ast(lexer.tokens.clone())
        .map_err(|e| println!("{e}"));

    if cli.ast_print {
        println!("\nProgram output:\n{:#?}", parser.ast_nodes);
    }

    
    // FIXME: instead of clone, use mem::replace
    match interp.execute_program(parser.ast_nodes.clone(), env) {
        Ok(_) => {},
        Err(e) => println!("{e}"),
    }
}

// REPL
fn repl(env: &mut Env, cli: &CLI) {
    println!("\n{} mode started", "Interactive".yellow().bold());

    // Local variables
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();

    loop {
        input.clear();
        print!("\n> ");
        stdout.flush().unwrap();

        match stdin.read_line(&mut input) {
            Ok(_) => {
                let trimmed_input = input.trim();

                if trimmed_input == "quit" {
                    process::exit(0);
                }

                // Execute interpreter
                interpretation_sequence(trimmed_input.to_string(), env, &cli);
            }
            Err(e) => panic!("Error reading from terminal: {e}"),
        }
    }
}

fn main() {
    // Manage command line args
    let cli = CLI::parse();

    // Beginning of program
    println!("\n       --- {} langage v0.1 ---", "Odyn".cyan().bold());
    io::stdout().flush().unwrap();

    // If file input activated
    if cli.file.is_some() {
        let file_name = cli.file.as_ref().unwrap();

        // Create global environment
        let mut env = create_global_env();

        interpret_file(file_name.to_owned(), &mut env, &cli);

        // If interactive mode, we use the previous env
        if cli.inter {
            repl(&mut env, &cli);
        }
    } else if cli.test.is_some() {
        match cli.test.as_ref().unwrap().as_ref() {
            "all" => {
                // We get all the files
                let paths = fs::read_dir("tests").expect("Can't find test directory");

                for file_path in paths {
                    // Create an environment environment for each test. No shared data
                    let mut env = create_global_env();

                    if let Ok(path) = file_path {
                        interpret_file(path.path().display().to_string(), &mut env, &cli);
                    }
                }
            }
            path @ _ => {
                // Create global environment
                let mut env = create_global_env();

                interpret_file(path.to_string(), &mut env, &cli)
            }
        }

        // Exit after test
        println!("");
    } else {
        // Create global environment
        let mut env = create_global_env();

        repl(&mut env, &cli);
    }
}
