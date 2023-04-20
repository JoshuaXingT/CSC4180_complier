use crate::NonTerminalSymbol;
use crate::Parser;
use crate::Production;
use crate::Symbol;
use crate::TerminalSymbol;
use std::fs::File;
use std::io::{BufRead, BufReader, Result};

pub const TERMINAL_TABLE: [&str; 41] = [
    "INT", "MAIN", "VOID", "BREAK", "DO", "ELSE", "IF", "WHILE", "RETURN", "READ", "WRITE",
    "LBRACE", "RBRACE", "LSQUARE", "RSQUARE", "LPAR", "RPAR", "SEMI", "PLUS", "MINUS", "MUL_OP",
    "DIV_OP", "AND_OP", "OR_OP", "NOT_OP", "ASSIGN", "LT", "GT", "SHL_OP", "SHR_OP", "EQ", "NOTEQ",
    "LTEQ", "GTEQ", "ANDAND", "OROR", "COMMA", "ID", "INT_NUM", "EOF", "EMPTY",
];

pub const NONTERMINAL_TABLE: [&str; 40] = [
    "S",
    "program",
    "statements",
    "var_declarations",
    "var_declaration",
    "declaration_list",
    "declaration_list_00",
    "declaration",
    "code_block",
    "statement",
    "control_statement",
    "read_write_statement",
    "assign_statement",
    "if_statement",
    "if_stmt",
    "while_statement",
    "do_while_statement",
    "return_statement",
    "read_statement",
    "write_statement",
    "exp",
    "exp_00",
    "exp_1",
    "exp_11",
    "exp_2",
    "exp_22",
    "exp_3",
    "exp_33",
    "exp_4",
    "exp_44",
    "exp_5",
    "exp_55",
    "exp_6",
    "exp_66",
    "exp_7",
    "exp_77",
    "exp_8",
    "exp_88",
    "exp_9",
    "exp_10",
];

pub struct Grammar {
    pub grammar_lines: Vec<String>,
    pub grammar_rules: Vec<Production>,
}

impl Grammar {
    pub fn default() -> Self {
        Self {
            grammar_lines: Vec::new(),
            grammar_rules: Vec::new(),
        }
    }
    pub fn init_grammar(&mut self, file_path: &str) -> Result<()> {
        let file = File::open(file_path)?;
        let reader = BufReader::new(file);

        for line in reader.lines() {
            let line = line?;
            if line.is_empty() || line.starts_with("<!") {
                continue;
            }
            self.grammar_lines.push(line);
        }

        Ok(())
    }
    pub fn construct_grammar(&mut self, parser: &mut Parser) {
        //init all symbol
        parser
            .all_symbol
            .insert(Symbol::Terminal(TerminalSymbol::EOF));

        for grammar in &self.grammar_lines {
            let mut parts = grammar.splitn(2, ". ");
            let digit_str = parts.next().unwrap_or("");
            let digit = digit_str.parse().unwrap_or(0);
            let rest = parts.next().unwrap_or("?");
            let mut tokens: Vec<&str> = rest.split(" -> ").collect();
            let last_token = tokens.pop().unwrap_or("");
            let subtokens: Vec<&str> = last_token.split(" ").collect();

            let mut production = Production::default();
            production.line = digit;
            production.lhs = convert_to_symbol(tokens[0], parser);
            for token in &subtokens {
                production.rhs.push(convert_to_symbol(token, parser));
            }
            self.grammar_rules.push(production);
        }
    }
}

fn convert_to_symbol(token: &str, parser: &mut Parser) -> Symbol {
    if NONTERMINAL_TABLE.contains(&token) {
        let index = NONTERMINAL_TABLE.iter().position(|&x| x == token).unwrap();
        parser
            .all_symbol
            .insert(Symbol::NonTerminal(NonTerminalSymbol::get(index)));
        Symbol::NonTerminal(NonTerminalSymbol::get(index))
    } else {
        let index = TERMINAL_TABLE.iter().position(|&x| x == token).unwrap();
        parser
            .all_symbol
            .insert(Symbol::Terminal(TerminalSymbol::get(index)));
        Symbol::Terminal(TerminalSymbol::get(index))
    }
}
