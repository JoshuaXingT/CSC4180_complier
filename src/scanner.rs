const SYMBOL_LIST: [&str; 76] = [
    "TERMINAL",    // state 0
    "START/ERROR", // state 1
    "ID",          // state 2
    "ID",          // state 3
    "ID",          // state 4
    "ID",          // state 5
    "BREAK",       // state 6
    "ID",          // state 7
    "ID",          // state 8
    "DO",          // state 9
    "ID",          // state 10
    "ID",          // state 11
    "ID",          // state 12
    "ELSE",        // state 13
    "ID",          // state 14
    "ID",          // state 15
    "INT",         // state 16
    "IF",          // state 17
    "ID",          // state 18
    "ID",          // state 19
    "ID",          // state 20
    "MAIN",        // state 21
    "ID",          // state 22
    "ID",          // state 23
    "DO",          // state 24
    "ID",          // state 25
    "ID",          // state 26
    "WRITE",       // state 27
    "ID",          // state 28
    "ID",          // state 29
    "ID",          // state 30
    "ID",          // state 31
    "ID",          // state 32
    "RETURN",      // state 33
    "ID",          // state 34
    "ID",          // state 35
    "ID",          // state 36
    "ID",          // state 37
    "READ",        // state 38
    "ID",          // state 39
    "ID",          // state 40
    "ID",          // state 41
    "VOID",        // state 42
    "ID",          // state 43
    "ID",          // state 44
    "ID",          // state 45
    "ID",          // state 46
    "WHILE",       // state 47
    "INT_NUM",     // state 48
    "LBRACE",      // state 49
    "RBRACE",      // state 50
    "LSQUARE",     // state 51
    "RSQUARE",     // state 52
    "LPAR",        // state 53
    "RPAR",        // state 54
    "SEMI",        // state 55
    "PLUS",        // state 56
    "MINUS",       // state 57
    "MUL_OP",      // state 58
    "DIV_OP",      // state 59
    "AND_OP",      // state 60
    "ANDAND",      // state 61
    "OR_OP",       // state 62
    "OROR",        // state 63
    "NOT_OP",      // state 64
    "NOTEQ",       // state 65
    "ASSIGN",      // state 66
    "EQ",          // state 67
    "LT",          // state 68
    "SHL_OP",      // state 69
    "LTEQ",        // state 70
    "GT",          // state 71
    "SHR_OP",      // state 72
    "GTEQ",        // state 73
    "COMMA",       // state 74
    "ERROR",       // state 75
];

pub struct Scanner {
    current_state: u32,
    current_str: String,
}

impl Scanner {
    pub fn default() -> Self {
        Self {
            current_state: 1,
            current_str: String::new(),
        }
    }

    pub fn clear_and_output(&mut self, tokens: &mut Vec<&str>, scanner_str: &mut Vec<String>) {
        // println!(
        //     "Token: {} <-- Str: {}",
        //     SYMBOL_LIST[self.current_state as usize], self.current_str
        // );

        if self.current_state != 75 {
            // println!("Token: {}", SYMBOL_LIST[self.current_state as usize]);
            // print!("{} ", SYMBOL_LIST[self.current_state as usize]);
            tokens.push(SYMBOL_LIST[self.current_state as usize]);
            scanner_str.push(self.current_str.clone());
        }

        //init scanner
        self.current_state = 1;
        self.current_str = String::new();
    }

    pub fn push_char(&mut self, c: char) {
        self.current_str.push(c);
    }

    pub fn get_state(&self) -> u32 {
        self.current_state
    }

    pub fn update_state(&mut self, new_state: u32) {
        self.current_state = new_state;
    }
}
