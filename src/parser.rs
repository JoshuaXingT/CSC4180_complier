use crate::symbol::NonTerminalSymbolTable;
use crate::symbol::TerminalSymbolTable;
use crate::Action;
use crate::NonTerminalSymbol;
use crate::Symbol;
use crate::TerminalSymbol;
use crate::Token;
use crate::TokenType;
use crate::TERMINAL_TABLE;
use std::collections::BTreeSet;
use std::collections::{HashMap, HashSet};

pub const TEMPER_REGISTER_TABLE: [&str; 8] =
    ["$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7"];

pub const TEMPER_REGISTER_CHECK: [bool; 8] = [false; 8];

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Production {
    pub line: usize,
    pub lhs: Symbol,
    pub rhs: Vec<Symbol>,
}

impl Production {
    pub fn default() -> Self {
        Self {
            line: 0,
            lhs: Symbol::NonTerminal(NonTerminalSymbol::S),
            rhs: Vec::new(),
        }
    }
}

impl ToString for Production {
    fn to_string(&self) -> String {
        let mut production_str = String::new();
        for symbol in &self.rhs {
            production_str.push_str(&format!("{} ", symbol.to_string()));
        }
        format!(
            "{}: {}->{}",
            self.line,
            self.lhs.to_string(),
            production_str
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LR1Item {
    pub production: Production,
    pub dot: usize,
    pub lookahead: Symbol,
}

impl LR1Item {
    pub fn default() -> Self {
        Self {
            production: Production::default(),
            dot: 0,
            lookahead: Symbol::Terminal(TerminalSymbol::EOF),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LR1State {
    //----------------------------------BTree set-----------------------
    pub items: BTreeSet<LR1Item>,
}

impl LR1State {
    pub fn default() -> Self {
        Self {
            items: BTreeSet::new(),
        }
    }
}

pub fn items(
    productions: &Vec<Production>,
    first_sets: &HashMap<Symbol, HashSet<Symbol>>,
    parser: &Parser,
) -> BTreeSet<LR1State> {
    // ---------------------------------hash set-------------------------
    let mut return_lr1state: BTreeSet<LR1State> = BTreeSet::new();
    let mut temp_sons: Vec<Symbol> = vec![];
    // temp_sons.push("program".to_string());
    temp_sons.push(Symbol::NonTerminal(NonTerminalSymbol::program));
    let origin = Production {
        line: 0,
        lhs: Symbol::NonTerminal(NonTerminalSymbol::S),
        rhs: temp_sons,
    };
    let expand = LR1Item {
        production: origin,
        lookahead: Symbol::Terminal(TerminalSymbol::EOF),
        dot: 0,
    };
    let mut init_set: LR1State = LR1State::default();
    init_set.items.insert(expand);
    return_lr1state.insert(closure(&mut init_set, productions, first_sets));

    let mut last_len = return_lr1state.len();

    let mut count = 0;
    loop {
        count = count + 1;

        let mut temp: BTreeSet<LR1State> = BTreeSet::new();
        let mut lr1state_iter = return_lr1state.iter();
        for _ in 0..return_lr1state.len() {
            let lr1_next = lr1state_iter.next();
            for x in &parser.all_symbol {
                if lr1_next.is_none() {
                    // println!("is none, sad");
                    break;
                }
                // println!("not none, great");

                let j_set = goto(lr1_next.unwrap(), x, productions, first_sets);

                if !j_set.items.is_empty() && !return_lr1state.contains(&j_set) {
                    temp.insert(j_set);
                }
            }
        }

        return_lr1state.extend(temp.into_iter().collect::<BTreeSet<LR1State>>());

        let new_len = return_lr1state.len();
        // println!("new:{}  old:{}", new_len, last_len);

        if new_len == last_len {
            break;
        } else {
            last_len = new_len;
        }

        // println!(
        //     "count:{}, lr1state_len:{}; symbol_count = {}  Nsize = {}",
        //     count,
        //     return_lr1state.len(),
        //     &parser.all_symbol.len(),
        //     return_lr1state.len() * &parser.all_symbol.len()
        // );
    }
    return_lr1state
}

fn closure(
    I: &mut LR1State,
    productions: &Vec<Production>,
    first_sets: &HashMap<Symbol, HashSet<Symbol>>,
) -> LR1State {
    // let re_lr1state = LR1State::default();
    let mut last_len = I.items.len();
    loop {
        for item in I.items.clone() {
            // let temp = &Symbol::None;
            let B = if item.dot != item.production.rhs.len() {
                &item.production.rhs[item.dot]
            } else {
                &Symbol::None
            };

            // B is the symbol after the dot
            for production in productions {
                if production.lhs == *B {
                    // for each production B->r in G'
                    let mut beta_a = Vec::new();
                    for j in (item.dot + 1)..item.production.rhs.len() {
                        beta_a.push(item.production.rhs[j]);
                    }
                    beta_a.push(item.lookahead);
                    let mut first: HashSet<Symbol> = HashSet::new(); // compute first(beta_a)
                    for word in &beta_a {
                        let temp = first_sets.get(word).unwrap();
                        first.extend(temp.iter().cloned());

                        // TODO: whether we should add an "E"
                        if !temp.contains(&Symbol::Terminal(TerminalSymbol::EMPTY)) {
                            break;
                        }
                    }
                    for b in &first {
                        if let Symbol::Terminal(_) = b {
                            // for each terminal b in first(beta_a)
                            let new_item = LR1Item {
                                production: production.clone(),
                                dot: 0,
                                lookahead: *b,
                            };
                            if !I.items.contains(&new_item) {
                                I.items.insert(new_item);
                            }
                        }
                    }
                }
            }
        }
        let new_len = I.items.len();

        // TODO something wrong here
        if new_len == last_len {
            break;
        } else {
            last_len = new_len;
        }
    }
    // println!("finish closure");
    I.clone()
}

fn goto(
    lr1state: &LR1State,
    symbol: &Symbol,
    productions: &Vec<Production>,
    first_sets: &HashMap<Symbol, HashSet<Symbol>>,
) -> LR1State {
    let mut new_items = LR1State::default();

    for item in &lr1state.items {
        if item.dot >= item.production.rhs.len() {
            continue;
        }

        if item.production.rhs[item.dot] == *symbol {
            let next_p = item.dot + 1;
            let mut newitem = LR1Item::default();
            newitem.production = item.production.clone();
            newitem.dot = next_p;
            newitem.lookahead = item.lookahead;
            new_items.items.insert(newitem);
        }
    }

    let a = closure(&mut new_items, productions, first_sets);
    // println!("finish goto");
    a
}

pub struct Parser {
    pub all_symbol: HashSet<Symbol>,
    pub first_set: HashMap<Symbol, HashSet<Symbol>>,

    //BTreeSet
    pub lr1_states: BTreeSet<LR1State>,
    pub action_table: Vec<HashMap<Symbol, Action>>,
    pub goto_table: Vec<HashMap<Symbol, usize>>,
}

impl Parser {
    pub fn default() -> Self {
        Self {
            all_symbol: HashSet::new(),
            first_set: HashMap::new(),
            lr1_states: BTreeSet::new(),
            action_table: Vec::new(),
            goto_table: Vec::new(),
        }
    }
    pub fn init_first_set(&mut self, productions: &Vec<Production>) {
        for terminal in &TerminalSymbolTable {
            let mut temp = HashSet::new();
            temp.insert(Symbol::Terminal(*terminal));
            self.first_set.insert(Symbol::Terminal(*terminal), temp);
        }
        for non_terminal in &NonTerminalSymbolTable {
            self.construct_first_set(&Symbol::NonTerminal(*non_terminal), &productions);
        }
    }

    fn construct_first_set(&mut self, symbol: &Symbol, productions: &Vec<Production>) {
        if !self.first_set.get(symbol).is_none() {
            return;
        }
        let mut lhs_first = self
            .first_set
            .get(symbol)
            .cloned()
            .unwrap_or_else(HashSet::new);
        for production in productions {
            if production.lhs == *symbol {
                for token in &production.rhs {
                    self.construct_first_set(token, productions);
                    let rhs_first = self.first_set.get(token).unwrap();
                    lhs_first.extend(rhs_first);
                    if !rhs_first.contains(&Symbol::Terminal(TerminalSymbol::EMPTY)) {
                        break;
                    }
                }
                self.first_set.insert(*symbol, lhs_first.clone());
            }
        }
    }
    pub fn construct_parsing_table(&mut self, productions: &Vec<Production>) {
        let state_num = self.lr1_states.len();
        // println!("{}", state_num);
        self.action_table = Vec::with_capacity(state_num);
        self.goto_table = Vec::with_capacity(state_num);
        for _ in 0..state_num {
            self.action_table.push(HashMap::new());
            self.goto_table.push(HashMap::new());
        }
        let mut i = 0;
        for lr1_state in &self.lr1_states {
            for lr1_item in &lr1_state.items {
                let rhs = &lr1_item.production.rhs;
                let lhs = lr1_item.production.lhs;
                let cur_str = if lr1_item.dot == rhs.len() {
                    Symbol::None
                } else {
                    rhs[lr1_item.dot]
                };
                if let Symbol::Terminal(_) = cur_str {
                    let target = &goto(lr1_state, &cur_str, productions, &self.first_set);
                    let mut index = 0;
                    for temp_state in &self.lr1_states {
                        if temp_state == target {
                            break;
                        }
                        index = index + 1;
                    }
                    self.action_table[i].insert(cur_str, Action::Shift(index));
                }
                if cur_str == Symbol::None && lhs != Symbol::NonTerminal(NonTerminalSymbol::S) {
                    let mut index = 0;
                    for i in 0..productions.len() {
                        if lr1_item.production == productions[i] {
                            index = i;
                        }
                    }
                    self.action_table[i].insert(lr1_item.lookahead, Action::Reduce(index));
                }
                if lr1_item.production.lhs == Symbol::NonTerminal(NonTerminalSymbol::S)
                    && cur_str == Symbol::None
                {
                    self.action_table[i]
                        .insert(Symbol::Terminal(TerminalSymbol::EOF), Action::Accept);
                }
                if let Symbol::NonTerminal(_) = cur_str {
                    let target = &goto(lr1_state, &cur_str, productions, &self.first_set);
                    let mut index = 0;
                    for temp_state in &self.lr1_states {
                        if temp_state == target {
                            break;
                        }
                        index = index + 1;
                    }
                    self.goto_table[i].insert(cur_str, index);
                }
            }
            i = i + 1;
        }
    }

    pub fn parse(
        &mut self,
        tokens: &Vec<&str>,
        scanner_str: &Vec<String>,
        productions: &Vec<Production>,
    ) -> Option<()> {
        let mut ptr = 0;
        let mut state_stack: Vec<usize> = Vec::with_capacity(10);
        let mut symbol_stack: Vec<Token> = Vec::with_capacity(10);
        state_stack.push(0);
        loop {
            let state = state_stack.last().unwrap();
            let token = tokens[ptr];
            let index = TERMINAL_TABLE.iter().position(|&x| x == token).unwrap();
            let mut symbol_token = Symbol::Terminal(TerminalSymbol::get(index));
            // println!("{}: {:?}", state, symbol_token);
            let mut action = self.action_table[*state].get(&symbol_token);
            if action.is_none() {
                symbol_token = Symbol::Terminal(TerminalSymbol::EMPTY);
                action = self.action_table[*state].get(&symbol_token);
                if action.is_none() {
                    return Option::None;
                }
                ptr = ptr - 1;
            }
            let action = action.unwrap();
            if let Action::Shift(state_index) = action {
                // print!("state: {}\tnext type: {}\t\t", state, token);
                // symbol_stack.push(symbol_token);
                if symbol_token == Symbol::Terminal(TerminalSymbol::INT_NUM) {
                    symbol_stack.push(Token {
                        symbol: symbol_token,
                        int_val: scanner_str[ptr].parse::<usize>().unwrap(),
                        id: "".to_string(),
                        len: 0,
                        mem_addr: 0,
                        token_type: TokenType::IntExpr,
                        temp_reg_index: 0,
                    });
                } else {
                    symbol_stack.push(Token {
                        symbol: symbol_token,
                        int_val: 0,
                        id: scanner_str[ptr].clone(),
                        len: 0,
                        mem_addr: 0,
                        token_type: TokenType::IDExpr,
                        temp_reg_index: 0,
                    });
                }
                state_stack.push(*state_index);
                ptr = ptr + 1;
                // println!("shift to state {}", state_index);
            } else if let Action::Reduce(state_index) = action {
                let grammar_line = state_index;
                let production = &productions[*grammar_line];
                let mut len = production.rhs.len();
                // print!("state: {}\tnext type: {}\t\t", state, token);
                // println!("reduce by grammer {}", production.to_string());
                let mut rhs_token: Vec<Token> = Vec::with_capacity(10);
                while len > 0 {
                    rhs_token.push(symbol_stack.pop().unwrap());
                    state_stack.pop();
                    len = len - 1;
                }
                // symbol_stack.push(production.lhs);
                let mut target_token = Token {
                    symbol: production.lhs,
                    int_val: 0,
                    id: "".to_string(),
                    len: 0,
                    mem_addr: 0,
                    token_type: TokenType::CombinedExpr,
                    temp_reg_index: 0,
                };
                handle(production.line, &mut target_token, rhs_token);
                symbol_stack.push(target_token);
                let state_temp = state_stack.last().unwrap();
                state_stack.push(*self.goto_table[*state_temp].get(&production.lhs).unwrap());
            } else if *action == Action::Accept {
                // println!("Accept!");
                break;
            }
            // print!("current situation: ");
            // for stack_symbol in &symbol_stack {
            //     print!(
            //         "{} {} {},",
            //         stack_symbol.symbol.to_string(),
            //         stack_symbol.int_val,
            //         stack_symbol.id
            //     );
            // }
            // println!("|\n")
        }
        Some(())
    }
}

fn handle(line: usize, lhs_pos: &mut Token, rhs_pos: Vec<Token>) {
    match line {
        // 9. declaration -> ID ASSIGN exp
        9 => println!(),
        // 10. declaration -> ID LSQUARE exp RSQUARE
        10 => println!(),
        // 11. declaration -> ID
        11 => println!(),
        // 26. assign_statement -> ID LSQUARE exp RSQUARE ASSIGN exp
        26 => println!(),
        // 27. assign_statement -> ID ASSIGN exp
        27 => println!(),
        // 29. if_statement -> if_stmt ELSE code_block
        29 => println!(),
        // 30. if_stmt -> IF LPAR exp RPAR code_block
        30 => println!(),
        // 31. while_statement -> WHILE LPAR exp RPAR code_block
        31 => println!(),
        // 32. do_while_statement -> DO code_block WHILE LPAR exp RPAR
        32 => println!(),
        // 34. read_statement -> READ LPAR ID RPAR
        34 => read_func(&rhs_pos[2]),
        // 35. write_statement -> WRITE LPAR exp RPAR
        35 => write_func(&rhs_pos[2]),
        // generate_1: exp_i -> exp_j exp_ii
        36 | 39 | 42 | 45 | 48 | 52 | 58 | 62 | 66 => println!(),
        // generate_2: exp_ii -> OP exp_j exp_ii
        37 | 40 | 43 | 46 | 49 | 50 | 53 | 54 | 55 | 56 | 59 | 60 | 63 | 64 | 67 | 68 => println!(),
        // generate_3: exp_ii -> EMPTY
        38 | 41 | 44 | 47 | 51 | 57 | 61 | 65 | 69 => println!(),
        // generate_4: exp_i -> OP exp_j
        70 | 71 => println!(),
        // 72. exp_9 -> exp_10
        72 => println!(),
        // 73. exp_10 -> ID LSQUARE exp RSQUARE
        73 => println!(),
        // 74. exp_10 -> ID
        74 => println!(),
        // 75. exp_10 -> INT_NUM
        75 => println!(),
        // 76. exp_10 -> LPAR exp RPAR
        76 => println!(),
        _ => {}
    }
}

fn read_func(id: &Token) {
    println!("addi $v0, $zero, 5");
    println!("syscall");
    println!("add $t8, $v0, $zero");
    println!("sw $t8, {}($sp)", -4 * id.mem_addr);
    // print \n
    println!("addi $v0, $zero, 11");
    println!("addi $a0, $zero, 10");
    println!("syscall");
}

fn write_func(exp: &Token) {
    println!("addi $v0, $zero, 1");
    if exp.token_type == TokenType::IntExpr {
        println!("addi $a0, $zero, {}", exp.int_val);
    } else {
        println!("lw $t8, {}($sp)", -4 * exp.mem_addr);
        println!("add $a0, $t8, $zero");
    }
    println!("syscall");

    // print \n
    println!("addi $v0, $zero, 11");
    println!("addi $a0, $zero, 10");
    println!("syscall");
}
