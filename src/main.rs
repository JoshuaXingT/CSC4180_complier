mod dfa;
mod grammar;
mod parser;
mod scanner;
mod symbol;

pub use grammar::Grammar;
pub use grammar::NONTERMINAL_TABLE;
pub use grammar::TERMINAL_TABLE;
use parser::items;
pub use parser::Parser;
pub use parser::Production;
use std::io::Result;
pub use symbol::Action;
pub use symbol::NonTerminalSymbol;
pub use symbol::Symbol;
pub use symbol::TerminalSymbol;
pub use symbol::Token;
pub use symbol::TokenType;

use crate::dfa::DfaEdges;
use crate::scanner::Scanner;
use std::env;

fn main() -> Result<()> {
    // scanner
    let mut program_tokens: Vec<&str> = Vec::new();
    let mut scanner_tokens: Vec<String> = Vec::new();

    let mut dfa_edges = DfaEdges::default();
    dfa_edges.init();

    let scanner = &mut Scanner::default();
    let args: Vec<String> = env::args().collect();

    let source_string: Vec<char> = read_file_string(&args[1]).unwrap().chars().collect();

    let mut ptr: i32 = 0;
    let len: i32 = source_string.len() as i32;
    // println!("Scanned Tokens:");
    loop {
        if ptr >= len {
            scanner.clear_and_output(&mut program_tokens, &mut scanner_tokens);
            break;
        }

        let current_char = source_string[ptr as usize];
        let next_state = dfa_edges.check_state(current_char, scanner.get_state());

        if next_state == 0 {
            // Terminal
            //clear and output
            scanner.clear_and_output(&mut program_tokens, &mut scanner_tokens);
            ptr -= 1;
        } else {
            scanner.update_state(next_state);
            scanner.push_char(current_char);
        }
        ptr += 1;
    }
    // parse
    program_tokens.push("EOF");
    // println!("EOF\n\nParsing Process:");
    let mut grammar = Grammar::default();
    let mut parser = Parser::default();
    if let Err(error) = grammar.init_grammar("./input_files/grammar.md") {
        panic!("{}", error);
    }
    grammar.construct_grammar(&mut parser);
    // for production in &grammar.grammar_rules {
    //     println!("{:?}", production);
    // }
    parser.init_first_set(&grammar.grammar_rules);
    // for (key, val) in &parser.first_set {
    //     println!("{:?}: {:?}", key, val);
    // }
    parser.lr1_states = items(&grammar.grammar_rules, &parser.first_set, &parser);

    parser.construct_parsing_table(&grammar.grammar_rules);
    // for state in &parser.action_table {
    //     println!("{:?}", state);
    // }
    if let Option::None = parser.parse(&program_tokens, &scanner_tokens, &grammar.grammar_rules) {
        println!("Reject!");
    }
    Ok(())
}

fn read_file_string(filepath: &str) -> Result<String> {
    let data = std::fs::read_to_string(filepath)?;
    Ok(data)
}
