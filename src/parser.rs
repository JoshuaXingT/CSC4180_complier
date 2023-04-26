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

pub static mut TEMPER_REGISTER_CHECK: [bool; 8] = [false; 8];
pub static mut MEMORY_PTR: i32 = 0;

// flag stack: containing flag like "L1" "L2" and "b" reminder
pub static mut FLAG_STACK: Vec<String> = Vec::new();

// flag pointer: ref to current unused flag
pub static mut FLAG_PTR: i32 = 1;
pub static mut IF_FLAG: i32 = 0;
pub static mut WHILE_FLAG: i32 = 0;
pub static mut DO_FLAG: Vec<i32> = Vec::new();
pub static mut CANCEL_WHILE_FLAG: i32 = 0;
pub static mut INSIDE_DO_TEMP_FLAG: Vec<i32> = Vec::new();

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
    pub token_table: Vec<Token>, //store all ID and
}

impl Parser {
    pub fn default() -> Self {
        Self {
            all_symbol: HashSet::new(),
            first_set: HashMap::new(),
            lr1_states: BTreeSet::new(),
            action_table: Vec::new(),
            goto_table: Vec::new(),
            token_table: Vec::with_capacity(100),
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
            if (ptr >= 1) {
                let pre_token = tokens[ptr - 1];
                let pre_index = TERMINAL_TABLE.iter().position(|&x| x == pre_token).unwrap();
                let pre_symbol_token = Symbol::Terminal(TerminalSymbol::get(pre_index));
                Self::handle_condition(pre_symbol_token);
            }

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
                // handle if/else/while condition

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
                self.handle(production.line, &mut target_token, &mut rhs_token);

                // handle if else
                if production.line == 30 {
                    if tokens[ptr] == Symbol::Terminal(TerminalSymbol::ELSE).to_string() {
                        unsafe {
                            // pop here
                            let s1 = FLAG_STACK.pop();
                            if s1.is_none() {
                                panic!("condition haven't push flag but require one");
                            }

                            let mut s2 = "L".to_string();
                            let n2 = FLAG_PTR.to_string();
                            s2 += &n2;
                            FLAG_PTR += 1;

                            println!("  b {}", s2);
                            println!("{}:", s1.unwrap());

                            FLAG_STACK.push(s2);
                            FLAG_STACK.push("out".to_string());
                        }
                    } else {
                        println!("not else but: {}", tokens[ptr + 1]);
                        unsafe {
                            // pop here
                            println!("not else~~~");
                            let s1 = FLAG_STACK.pop();
                            if s1.is_none() {
                                panic!("condition haven't push flag but require one");
                            }
                            println!("{}:", s1.unwrap());
                        }
                    }
                }

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

    fn handle_condition(token: Symbol) {
        match token {
            Symbol::Terminal(TerminalSymbol::IF) => unsafe {
                IF_FLAG = 1;
                if !DO_FLAG.is_empty() {
                    INSIDE_DO_TEMP_FLAG.push(1);
                }
            },
            Symbol::Terminal(TerminalSymbol::WHILE) => unsafe {
                if WHILE_FLAG == 1 {
                    return;
                }

                if CANCEL_WHILE_FLAG == 1 {
                    CANCEL_WHILE_FLAG = 0;
                    return;
                }

                // println!("handle once");
                WHILE_FLAG = 1;
                if !DO_FLAG.is_empty() {
                    INSIDE_DO_TEMP_FLAG.push(1);
                }

                unsafe {
                    let mut s1 = "L".to_string();
                    let n1 = FLAG_PTR.to_string();
                    s1 += &n1;
                    FLAG_PTR += 1;

                    let mut s2 = "L".to_string();
                    let n2 = FLAG_PTR.to_string();
                    s2 += &n2;
                    FLAG_PTR += 1;

                    println!("WHILE start {}:", &s1);
                    FLAG_STACK.push(s2.clone());
                    FLAG_STACK.push(s1.clone());
                    FLAG_STACK.push(s2.clone());
                }
            },
            Symbol::Terminal(TerminalSymbol::DO) => unsafe {
                DO_FLAG.push(1);
                unsafe {
                    let mut s1 = "L".to_string();
                    let n1 = FLAG_PTR.to_string();
                    s1 += &n1;
                    FLAG_PTR += 1;

                    println!("DO start {}:", &s1);
                    FLAG_STACK.push(s1.clone());
                }
            },
            _ => return,
        }
    }

    fn handle(&mut self, line: usize, lhs_pos: &mut Token, rhs_pos: &mut Vec<Token>) {
        match line {
            // 9. declaration -> ID ASSIGN exp
            9 => self.assign(),
            // 10. declaration -> ID LSQUARE exp RSQUARE
            10 => self.pass_id_ls_exp_rs_to_exp(),
            // 11. declaration -> ID
            // 11 => self.pass_id_to_exp(),
            12 | 13 => self.end_of_code_block(),

            // 26. assign_statement -> ID LSQUARE exp RSQUARE ASSIGN exp
            26 => self.assign_with_index(),
            // 27. assign_statement -> ID ASSIGN exp
            27 => self.assign(),

            // 29. if_statement -> if_stmt ELSE code_block
            29 => self.else_condition(),

            // 30. if_stmt -> IF LPAR exp RPAR code_block
            30 => self.if_condition(&rhs_pos[2]),
            // 31. while_statement -> WHILE LPAR exp RPAR code_block
            31 => self.while_condition(),
            // 32. do_while_statement -> DO code_block WHILE LPAR exp RPAR
            32 => self.do_while_condition(&rhs_pos[1]),
            // 34. read_statement -> READ LPAR ID RPAR
            34 => self.read_func(&mut rhs_pos[1]),
            // 35. write_statement -> WRITE LPAR exp RPAR
            35 => self.write_func(&rhs_pos[1]),
            //determine if flag
            36 => self.pass_exp1_to_exp(&rhs_pos[1], lhs_pos),

            // generate_1: exp_i -> exp_j exp_ii
            39 | 42 | 45 | 48 | 52 | 58 | 62 | 66 => self.pass_exp_exp_to_exp(&rhs_pos[1], lhs_pos),

            // generate_2: exp_ii -> OP exp_j exp_ii
            37 | 40 | 43 | 46 | 49 | 50 | 53 | 54 | 55 | 56 | 59 | 60 | 63 | 64 | 67 | 68 => {
                // println!("{}", rhs_pos.len());
                self.pass_op_exp_exp_to_exp(&rhs_pos[2], lhs_pos)
            }

            // generate_3: exp_ii -> EMPTY
            // 38 | 41 | 44 | 47 | 51 | 57 | 61 | 65 | 69 => println!()

            // generate_4: exp_i -> OP exp_j
            70 => self.pass_not_exp_to_exp(&rhs_pos[0], lhs_pos),
            71 => self.pass_minus_exp_to_exp(&rhs_pos[0], lhs_pos),
            // 72. exp_9 -> exp_10
            72 => self.pass_exp_to_exp(&rhs_pos[0], lhs_pos),
            // 73. exp_10 -> ID LSQUARE exp RSQUARE
            73 => self.pass_id_ls_exp_rs_to_exp(),
            // 74. exp_10 -> ID
            74 => self.pass_id_to_exp(&rhs_pos[0], lhs_pos),
            // 75. exp_10 -> INT_NUM
            75 => self.pass_integer_to_exp(rhs_pos[0].int_val, lhs_pos),
            // 76. exp_10 -> LPAR exp RPAR
            76 => self.pass_l_exp_r_to_exp(&rhs_pos[1], lhs_pos),
            _ => {}
        }
    }

    fn look_up_id(&self, id: &String) -> Option<usize> {
        for i in 0..self.token_table.len() {
            if self.token_table[i].id == *id {
                return Some(i);
            }
        }
        None
    }

    fn look_up_unused_temp_reg(&self) -> Option<usize> {
        unsafe {
            for i in 0..TEMPER_REGISTER_CHECK.len() {
                if TEMPER_REGISTER_CHECK[i] {
                    return Some(i);
                }
            }
            panic!("temp regitser overflow");
            // Some(0)
        }
    }

    // 34
    fn read_func(&mut self, id: &mut Token) {
        println!("addi $v0, $zero, 5");
        println!("syscall");
        // println!("add $t8, $v0, $zero");
        println!("move $t8, $v0");

        let index = self.look_up_id(&id.id);
        if index.is_none() {
            unsafe {
                id.mem_addr = MEMORY_PTR;
                self.token_table.push(id.clone());
                println!("  sw $t8, {}($sp)", -4 * MEMORY_PTR);
                MEMORY_PTR += 1;
            }
        } else {
            println!(
                "   sw $t8, {}($sp)",
                -4 * self.token_table[index.unwrap()].mem_addr
            );
        }
        // print \n
        println!("  addi $v0, $zero, 11");
        println!("  addi $a0, $zero, 10");
        println!("  syscall");
    }

    //35
    fn write_func(&mut self, exp: &Token) {
        println!("  addi $v0, $zero, 1");
        if exp.token_type == TokenType::IntExpr {
            println!("  addi $a0, $zero, {}", exp.int_val);
        } else if exp.token_type == TokenType::IDExpr {
            println!("  lw $t8, {}($sp)", -4 * exp.mem_addr);
            println!("  add $a0, $t8, $zero");
        } else {
            println!(
                "   add $a0, {}, $zero",
                TEMPER_REGISTER_TABLE[exp.temp_reg_index]
            );
            unsafe {
                TEMPER_REGISTER_CHECK[exp.temp_reg_index] = false;
            }
        }
        println!("  syscall");

        // print \n
        println!("  addi $v0, $zero, 11");
        println!("  addi $a0, $zero, 10");
        println!("  syscall");
    }

    fn assign(&mut self) {}

    // id[index]
    fn pass_id_ls_exp_rs_to_exp(&mut self) {}

    // id[index] = exp
    fn assign_with_index(&mut self) {}

    // ------------if else output logic---------------:
    // 1: condition output, for example:
    //         if(a == 1) -> lw a, lw 1, bne   //depending on condition
    // 2: (if part code block)
    //          {...,
    //           b $L3}
    // 3: (else part label)
    //          $L2:
    // 4: (else part code block)
    //          {...}
    // 5: if part jump out label
    //          $L3:

    //In this function we only need to implement logic 1 above
    //TO BE SOLVED: wrong logic
    //30. if_stmt -> IF LPAR exp RPAR code_block
    fn if_condition(&mut self, condition: &Token) {}

    // TODO sequence???
    fn else_condition(&mut self) {
        unsafe {
            let peek = &FLAG_STACK[FLAG_STACK.len() - 1];
            // println!("print value: {}", peek);
            if peek == "out" {
                // println!("else correct");
                FLAG_STACK.pop();
                let else_label = FLAG_STACK.pop();
                if else_label.is_none() {
                    panic!("else label stack overflow");
                } else {
                    println!("{}:", else_label.unwrap());
                }
            }
        }
    }

    //----------------while logic-----------------:
    // 1: condition label
    //      $L1
    // 2: condition code block:
    //      {...
    //       b L2}  // some judgement to jump out of while logic
    // 3: code block:
    //      {...
    //       b L1}  // back to judgement
    // 4: $L2 jump out label

    //In this funciton we need to implement logic 1 & 2
    //this function will also push a L2(jump out flag) into the flag stack
    //when the logic 3(code block end), the corresponding flag should also be outputed
    fn while_condition(&mut self) {
        //logic 1 L1 flag
        unsafe {
            let s1 = FLAG_STACK.pop();
            if s1.is_none() {
                panic!("while back to L1 missing label but require one");
            }

            let s2 = FLAG_STACK.pop();
            if s2.is_none() {
                panic!("while leave missing label but require one");
            }

            println!("  b {}", s1.unwrap());
            println!("{}:", s2.unwrap());
        }
    }

    //do while logic:
    //1: do while start flag:  //TODO: when scanner encounter "DO", push flag into stack
    //      $L1:
    //2: code block
    //      {...}
    //3: condition block
    //      {...
    //      beq L1}  // depend on condition  // pop corresponding flag out

    // 32. do_while_statement -> DO code_block WHILE LPAR exp RPAR
    // this function need to implement logic 3 above
    fn do_while_condition(&mut self, condition: &Token) {
        unsafe {
            let s1 = FLAG_STACK.pop();
            if s1.is_none() {
                panic!("do while missing leave label but require one");
            }

            match condition.token_type {
                TokenType::IntExpr => {
                    println!("  addi $t8, $zero, {}", condition.int_val);
                    println!("  bne $zero, $t8, {}", s1.unwrap());

                    // FLAG_STACK.push(s1);
                }
                TokenType::IDExpr => {
                    let index = self.look_up_id(&condition.id);
                    println!(
                        "  lw $t8, {}($sp)",
                        -4 * self.token_table[index.unwrap()].mem_addr
                    );
                    println!("  bne $zero, $t8, {}", s1.unwrap());
                }
                TokenType::CombinedExpr => {
                    println!(
                        "   bne $zero, {}, {}",
                        TEMPER_REGISTER_TABLE[condition.temp_reg_index],
                        s1.unwrap()
                    );
                }
            }
        }
    }

    // TO BE CHECKED
    // this function will be call at the end of a code block
    // used to output certain flag label used in the if/while/else/dowhile statement
    fn end_of_code_block(&mut self) {
        unsafe {
            let do_item = DO_FLAG.pop();
            if !do_item.is_none() {
                if INSIDE_DO_TEMP_FLAG.is_empty() {
                    CANCEL_WHILE_FLAG = 1;
                } else {
                    INSIDE_DO_TEMP_FLAG.pop();
                    DO_FLAG.push(1);
                }
            }
        }
    }

    // handle conditions, push flag stack
    fn pass_exp1_to_exp(&mut self, rhs: &Token, target: &mut Token) {
        let temp: Symbol = target.symbol;
        *target = rhs.clone();
        target.symbol = temp;

        unsafe {
            if IF_FLAG == 1 {
                // should output condition and push flag stack
                // output condition
                let mut s1 = "L".to_string();
                let n1 = FLAG_PTR.to_string();
                s1 += &n1;
                FLAG_PTR += 1;

                match target.token_type {
                    TokenType::IntExpr => {
                        println!("  addi $t8, $zero, {}", target.int_val);
                        println!("  beq $zero, $t8, {}", &s1);
                        FLAG_STACK.push(s1.clone());
                        // FLAG_STACK.push(s1);
                    }
                    TokenType::IDExpr => {
                        let index = self.look_up_id(&target.id);
                        println!(
                            "  lw $t8, {}($sp)",
                            -4 * self.token_table[index.unwrap()].mem_addr
                        );
                        println!("  beq $zero, $t8, {}", &s1);
                        FLAG_STACK.push(s1.clone());
                    }
                    TokenType::CombinedExpr => {
                        println!(
                            "   beq $zero, {}, {}",
                            TEMPER_REGISTER_TABLE[target.temp_reg_index], &s1
                        );
                        FLAG_STACK.push(s1.clone());
                    }
                }

                IF_FLAG = 0;
            } else if WHILE_FLAG == 1 {
                let s2 = FLAG_STACK.pop();
                if s2.is_none() {
                    panic!("while condition missing flag but require one");
                }

                match target.token_type {
                    TokenType::IDExpr => {
                        println!("  addi $t8, $zero, {}", target.int_val);
                        println!("  beq $zero, $t8, {}", s2.unwrap());
                    }

                    TokenType::IntExpr => {
                        let index = self.look_up_id(&target.id);
                        println!(
                            "   lw $t8, {}($sp)",
                            -4 * self.token_table[index.unwrap()].mem_addr
                        );
                        println!("  beq $zero, $t8, {}", s2.unwrap());
                    }

                    TokenType::CombinedExpr => {
                        println!(
                            "   beq $zero, {}, {}",
                            TEMPER_REGISTER_TABLE[target.temp_reg_index],
                            s2.unwrap()
                        );
                    }
                }

                // jump to L2
                WHILE_FLAG = 0;
            }
        }
    }

    // generate_1: exp_i -> exp_j exp_ii
    fn pass_exp_exp_to_exp(&mut self, rhs: &Token, target: &mut Token) {
        let temp: Symbol = target.symbol;
        *target = rhs.clone();
        target.symbol = temp;
    }

    //TODO
    // a op b
    fn pass_op_exp_exp_to_exp(&mut self, operation: &Token, target: &mut Token) {
        // the targe token type should be "CombinedExpr" with it's temp_register index assigned with content
        // for example:
        // suppose a is ID , b is combinedExpr, 1 is int, + is operation
        // target = x + x should conisder x situation:
        // 1 + 1
        // 1 + a
        // 1 + b
        // a + 1
        // b + 1
        // a + b
        // b + a
        // a + a
        // b + b
        match operation.symbol {
            Symbol::Terminal(TerminalSymbol::OROR) => {
                //situations:
                // int||int
                // temp_register.int_val = int || int
                let index = self.look_up_unused_temp_reg();

                // a||int or b||int
                // temp_register.int_val = a/b.int_val || int

                // a||a or a||b or b||a or b||b
                // temp_register.intval = a/b.int_val || a/b.int_val
            }
            Symbol::Terminal(TerminalSymbol::ANDAND) => {}
            Symbol::Terminal(TerminalSymbol::OR_OP) => {}
            Symbol::Terminal(TerminalSymbol::AND_OP) => {}
            Symbol::Terminal(TerminalSymbol::EQ) => {}
            Symbol::Terminal(TerminalSymbol::NOTEQ) => {}
            Symbol::Terminal(TerminalSymbol::LT) => {}
            Symbol::Terminal(TerminalSymbol::GT) => {}
            Symbol::Terminal(TerminalSymbol::LTEQ) => {}
            Symbol::Terminal(TerminalSymbol::GTEQ) => {}
            Symbol::Terminal(TerminalSymbol::SHL_OP) => {}
            Symbol::Terminal(TerminalSymbol::SHR_OP) => {}
            Symbol::Terminal(TerminalSymbol::PLUS) => {}
            Symbol::Terminal(TerminalSymbol::MINUS) => {}
            Symbol::Terminal(TerminalSymbol::MUL_OP) => {}
            Symbol::Terminal(TerminalSymbol::DIV_OP) => {}
            _ => return,
        }
    }

    fn pass_minus_exp_to_exp(&mut self, rhs: &Token, target: &mut Token) {
        match rhs.token_type {
            //TODO
            // minus ID -> 提取出ID中的值并返回 -usize
            TokenType::IDExpr => {
                target.int_val = -rhs.int_val; // why usize instead of i32?

                println!("  lw $t8,{}($fp)", -4 * rhs.mem_addr);
                println!("  subu $t8,$0,$t8");
                println!("  sw $t8,{}($fp)", -4 * rhs.mem_addr);
            }

            // minus int -> return -usize
            // 这里是否输出mips代码， 取决于编译器的偏好
            // 对于一些编译器来说，if(-1)的情况应该在编译器里直接判断，不需要再额外输出mips代码
            TokenType::IntExpr => target.int_val = (rhs.int_val.clone() as i32) as usize,

            //TODO:
            TokenType::CombinedExpr => {
                target.int_val = -rhs.int_val; // why usize instead of i32?

                println!("  lw $t8,{}", TEMPER_REGISTER_TABLE[rhs.temp_reg_index]);
                println!("  subu $t8,$0,$t8");
                println!("  sw $t8,{}", TEMPER_REGISTER_TABLE[rhs.temp_reg_index]);
            }
        }
    }

    fn pass_not_exp_to_exp(&mut self, rhs: &Token, target: &mut Token) {
        // target.int_val = !rhs.int_val.clone();
        match rhs.token_type {
            //TODO
            // minus ID -> 提取出ID中的值并返回 !usize
            TokenType::IDExpr => {
                target.int_val = !rhs.int_val;

                println!("  lw $t8,{}($fp)", -4 * rhs.mem_addr);
                println!("  sltu $t8,$t8,1");
                println!("  andi $t8,$t8,0x00ff");
                println!("  sw $t8,{}($fp)", -4 * rhs.mem_addr);
            }

            // minus int -> return -usize
            TokenType::IntExpr => target.int_val = !rhs.int_val,

            //TODO:
            TokenType::CombinedExpr => {
                // -combine should change the value in temp register
                target.int_val = !rhs.int_val;

                println!(
                    "  lw $t8,{}($fp)",
                    TEMPER_REGISTER_TABLE[rhs.temp_reg_index]
                );
                println!("  sltu $t8,$t8,1");
                println!("  andi $t8,$t8,0x00ff");
                println!(
                    "  sw $t8,{}($fp)",
                    TEMPER_REGISTER_TABLE[rhs.temp_reg_index]
                );
            }
        }
    }

    fn pass_exp_to_exp(&mut self, rhs: &Token, target: &mut Token) {
        // TODO: change token?
        *target = rhs.clone();
    }

    fn pass_id_to_exp(&mut self, id: &Token, target: &mut Token) {
        target.id = id.id.clone();
        target.token_type = TokenType::IDExpr;
        if self.look_up_id(&id.id).is_none() {
            unsafe {
                // id.mem_addr = MEMORY_PTR;
                self.token_table.push(id.clone());
                println!("  sw $t8, {}($sp)", -4 * MEMORY_PTR);
                MEMORY_PTR += 1;
            }
        }
    }

    fn pass_integer_to_exp(&mut self, int_val: usize, target: &mut Token) {
        target.int_val = int_val;
        target.token_type = TokenType::IntExpr;
    }

    fn pass_l_exp_r_to_exp(&mut self, rhs_exp: &Token, target: &mut Token) {
        *target = rhs_exp.clone();
    }
}
