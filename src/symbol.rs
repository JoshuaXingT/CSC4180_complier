use crate::NONTERMINAL_TABLE;
use crate::TERMINAL_TABLE;

pub static TerminalSymbolTable: [TerminalSymbol; 41] = [
    TerminalSymbol::INT,
    TerminalSymbol::MAIN,
    TerminalSymbol::VOID,
    TerminalSymbol::BREAK,
    TerminalSymbol::DO,
    TerminalSymbol::ELSE,
    TerminalSymbol::IF,
    TerminalSymbol::WHILE,
    TerminalSymbol::RETURN,
    TerminalSymbol::READ,
    TerminalSymbol::WRITE,
    TerminalSymbol::LBRACE,
    TerminalSymbol::RBRACE,
    TerminalSymbol::LSQUARE,
    TerminalSymbol::RSQUARE,
    TerminalSymbol::LPAR,
    TerminalSymbol::RPAR,
    TerminalSymbol::SEMI,
    TerminalSymbol::PLUS,
    TerminalSymbol::MINUS,
    TerminalSymbol::MUL_OP,
    TerminalSymbol::DIV_OP,
    TerminalSymbol::AND_OP,
    TerminalSymbol::OR_OP,
    TerminalSymbol::NOT_OP,
    TerminalSymbol::ASSIGN,
    TerminalSymbol::LT,
    TerminalSymbol::GT,
    TerminalSymbol::SHL_OP,
    TerminalSymbol::SHR_OP,
    TerminalSymbol::EQ,
    TerminalSymbol::NOTEQ,
    TerminalSymbol::LTEQ,
    TerminalSymbol::GTEQ,
    TerminalSymbol::ANDAND,
    TerminalSymbol::OROR,
    TerminalSymbol::COMMA,
    TerminalSymbol::ID,
    TerminalSymbol::INT_NUM,
    TerminalSymbol::EOF,
    TerminalSymbol::EMPTY,
];

pub static NonTerminalSymbolTable: [NonTerminalSymbol; 40] = [
    NonTerminalSymbol::S,
    NonTerminalSymbol::program,
    NonTerminalSymbol::statements,
    NonTerminalSymbol::var_declarations,
    NonTerminalSymbol::var_declaration,
    NonTerminalSymbol::declaration_list,
    NonTerminalSymbol::declaration_list_00,
    NonTerminalSymbol::declaration,
    NonTerminalSymbol::code_block,
    NonTerminalSymbol::statement,
    NonTerminalSymbol::control_statement,
    NonTerminalSymbol::read_write_statement,
    NonTerminalSymbol::assign_statement,
    NonTerminalSymbol::if_statement,
    NonTerminalSymbol::if_stmt,
    NonTerminalSymbol::while_statement,
    NonTerminalSymbol::do_while_statement,
    NonTerminalSymbol::return_statement,
    NonTerminalSymbol::read_statement,
    NonTerminalSymbol::write_statement,
    NonTerminalSymbol::exp,
    NonTerminalSymbol::exp_00,
    NonTerminalSymbol::exp_1,
    NonTerminalSymbol::exp_11,
    NonTerminalSymbol::exp_2,
    NonTerminalSymbol::exp_22,
    NonTerminalSymbol::exp_3,
    NonTerminalSymbol::exp_33,
    NonTerminalSymbol::exp_4,
    NonTerminalSymbol::exp_44,
    NonTerminalSymbol::exp_5,
    NonTerminalSymbol::exp_55,
    NonTerminalSymbol::exp_6,
    NonTerminalSymbol::exp_66,
    NonTerminalSymbol::exp_7,
    NonTerminalSymbol::exp_77,
    NonTerminalSymbol::exp_8,
    NonTerminalSymbol::exp_88,
    NonTerminalSymbol::exp_9,
    NonTerminalSymbol::exp_10,
];

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Symbol {
    Terminal(TerminalSymbol),
    NonTerminal(NonTerminalSymbol),
    None,
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        match self {
            Symbol::NonTerminal(index) => format!("{}", NONTERMINAL_TABLE[*index as usize]),
            Symbol::Terminal(index) => format!("{}", TERMINAL_TABLE[*index as usize]),
            Symbol::None => format!(""),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TerminalSymbol {
    INT,
    MAIN,
    VOID,
    BREAK,
    DO,
    ELSE,
    IF,
    WHILE,
    RETURN,
    READ,
    WRITE,
    LBRACE,
    RBRACE,
    LSQUARE,
    RSQUARE,
    LPAR,
    RPAR,
    SEMI,
    PLUS,
    MINUS,
    MUL_OP,
    DIV_OP,
    AND_OP,
    OR_OP,
    NOT_OP,
    ASSIGN,
    LT,
    GT,
    SHL_OP,
    SHR_OP,
    EQ,
    NOTEQ,
    LTEQ,
    GTEQ,
    ANDAND,
    OROR,
    COMMA,
    ID,
    INT_NUM,
    EOF,
    EMPTY,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum NonTerminalSymbol {
    S,
    program,
    statements,
    var_declarations,
    var_declaration,
    declaration_list,
    declaration_list_00,
    declaration,
    code_block,
    statement,
    control_statement,
    read_write_statement,
    assign_statement,
    if_statement,
    if_stmt,
    while_statement,
    do_while_statement,
    return_statement,
    read_statement,
    write_statement,
    exp,
    exp_00,
    exp_1,
    exp_11,
    exp_2,
    exp_22,
    exp_3,
    exp_33,
    exp_4,
    exp_44,
    exp_5,
    exp_55,
    exp_6,
    exp_66,
    exp_7,
    exp_77,
    exp_8,
    exp_88,
    exp_9,
    exp_10,
}

impl NonTerminalSymbol {
    pub fn get(index: usize) -> Self {
        NonTerminalSymbolTable[index]
    }
}

impl TerminalSymbol {
    pub fn get(index: usize) -> Self {
        TerminalSymbolTable[index]
    }
}
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub symbol: Symbol,
    pub int_val: i32,
    pub id: String,
    pub len: usize,    // array size
    pub mem_addr: i32, // memory_address
    pub token_type: TokenType,
    pub temp_reg_index: usize,
    pub op: TerminalSymbol,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenType {
    IntExpr,
    IDExpr,
    CombinedExpr,
    EMPTY,
}
