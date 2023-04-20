pub struct DfaEdges {
    pub arr: [[u32; 256]; 76],
}

impl DfaEdges {
    pub fn default() -> Self {
        Self {
            arr: [[0; 256]; 76],
        }
    }

    pub fn init(&mut self) {
        // state 0 terminal state

        // state 1 (start state)
        //ERROR
        self.arr[1] = [75; 256];

        //Keywords
        self.single_assign_with_char(1, 2, 'b'); // b
        self.single_assign_with_char(1, 8, 'd'); // d
        self.single_assign_with_char(1, 10, 'e'); // e
        self.single_assign_with_char(1, 14, 'i'); // i
        self.single_assign_with_char(1, 18, 'm'); // m
        self.single_assign_with_char(1, 22, 'p'); // p
        self.single_assign_with_char(1, 28, 'r'); // r
        self.single_assign_with_char(1, 34, 's'); // s
        self.single_assign_with_char(1, 39, 'v'); // v
        self.single_assign_with_char(1, 43, 'w'); // w

        //ID
        self.single_assign_with_char(1, 7, 'a');
        self.single_assign_with_char(1, 7, 'c');
        self.mass_assign_with_chars(1, 7, 'f', 'h');
        self.mass_assign_with_chars(1, 7, 'j', 'l');
        self.mass_assign_with_chars(1, 7, 'n', 'o');
        self.single_assign_with_char(1, 7, 'q');
        self.mass_assign_with_chars(1, 7, 't', 'u');
        self.mass_assign_with_chars(1, 7, 'x', 'z');
        self.mass_assign_with_chars(1, 7, 'A', 'Z');

        //INT_NUM
        self.mass_assign_with_chars(1, 48, '0', '9');

        //Special Symbols
        self.single_assign_with_char(1, 49, '{');
        self.single_assign_with_char(1, 50, '}');
        self.single_assign_with_char(1, 51, '[');
        self.single_assign_with_char(1, 52, ']');
        self.single_assign_with_char(1, 53, '(');
        self.single_assign_with_char(1, 54, ')');
        self.single_assign_with_char(1, 55, ';');
        self.single_assign_with_char(1, 56, '+');
        self.single_assign_with_char(1, 57, '-');
        self.single_assign_with_char(1, 58, '*');
        self.single_assign_with_char(1, 59, '/');
        self.single_assign_with_char(1, 60, '&');
        self.single_assign_with_char(1, 62, '|');
        self.single_assign_with_char(1, 64, '!');
        self.single_assign_with_char(1, 66, '=');
        self.single_assign_with_char(1, 68, '<');
        self.single_assign_with_char(1, 71, '>');
        self.single_assign_with_char(1, 74, ',');

        // state 2 (ID)
        self.single_assign_with_char(2, 3, 'r'); // br

        self.mass_assign_with_chars(2, 7, 'a', 'q');
        self.mass_assign_with_chars(2, 7, 's', 'z');
        self.mass_assign_with_chars(2, 7, '0', '9');
        self.mass_assign_with_chars(2, 7, 'A', 'Z');
        self.single_assign_with_char(2, 7, '_');

        // state 3 (ID)
        self.single_assign_with_char(3, 4, 'e'); // bre

        self.mass_assign_with_chars(3, 7, 'a', 'd');
        self.mass_assign_with_chars(3, 7, 'f', 'z');
        self.mass_assign_with_chars(3, 7, '0', '9');
        self.mass_assign_with_chars(3, 7, 'A', 'Z');
        self.single_assign_with_char(3, 7, '_');

        // state 4 (ID)
        self.single_assign_with_char(4, 5, 'a'); // brea

        self.mass_assign_with_chars(4, 7, 'b', 'z');
        self.mass_assign_with_chars(4, 7, '0', '9');
        self.mass_assign_with_chars(4, 7, 'A', 'Z');
        self.single_assign_with_char(4, 7, '_');

        // state 5 (ID)
        self.single_assign_with_char(5, 6, 'k'); // break

        self.mass_assign_with_chars(5, 7, 'a', 'j');
        self.mass_assign_with_chars(5, 7, 'l', 'z');
        self.mass_assign_with_chars(5, 7, '0', '9');
        self.mass_assign_with_chars(5, 7, 'A', 'Z');
        self.single_assign_with_char(5, 7, '_');

        // state 6 (BREAK)
        self.mass_assign_with_chars(6, 7, 'a', 'z');
        self.mass_assign_with_chars(6, 7, '0', '9');
        self.mass_assign_with_chars(6, 7, 'A', 'Z');
        self.single_assign_with_char(6, 7, '_');

        // state 7 (ID Terminal)
        self.mass_assign_with_chars(7, 7, 'a', 'z');
        self.mass_assign_with_chars(7, 7, '0', '9');
        self.mass_assign_with_chars(7, 7, 'A', 'Z');
        self.single_assign_with_char(7, 7, '_');

        // state 8 (ID)
        self.single_assign_with_char(8, 9, 'o'); // do

        self.mass_assign_with_chars(8, 7, 'a', 'n');
        self.mass_assign_with_chars(8, 7, 'p', 'z');
        self.mass_assign_with_chars(8, 7, '0', '9');
        self.mass_assign_with_chars(8, 7, 'A', 'Z');
        self.single_assign_with_char(8, 7, '_');

        // state 9 (DO)
        self.mass_assign_with_chars(9, 7, 'a', 'z');
        self.mass_assign_with_chars(9, 7, '0', '9');
        self.mass_assign_with_chars(9, 7, 'A', 'Z');
        self.single_assign_with_char(9, 7, '_');

        // state 10 (ID)
        self.single_assign_with_char(10, 11, 'l'); // el

        self.mass_assign_with_chars(10, 7, 'a', 'k');
        self.mass_assign_with_chars(10, 7, 'm', 'z');
        self.mass_assign_with_chars(10, 7, '0', '9');
        self.mass_assign_with_chars(10, 7, 'A', 'Z');
        self.single_assign_with_char(10, 7, '_');

        // state 11 (ID)
        self.single_assign_with_char(11, 12, 's'); // els

        self.mass_assign_with_chars(11, 7, 'a', 'r');
        self.mass_assign_with_chars(11, 7, 't', 'z');
        self.mass_assign_with_chars(11, 7, '0', '9');
        self.mass_assign_with_chars(11, 7, 'A', 'Z');
        self.single_assign_with_char(11, 7, '_');

        // state 12 (ID)
        self.single_assign_with_char(12, 13, 'e'); // else

        self.mass_assign_with_chars(12, 7, 'a', 'd');
        self.mass_assign_with_chars(12, 7, 'f', 'z');
        self.mass_assign_with_chars(12, 7, '0', '9');
        self.mass_assign_with_chars(12, 7, 'A', 'Z');
        self.single_assign_with_char(12, 7, '_');

        // state 13 (ELSE)
        self.mass_assign_with_chars(13, 7, 'a', 'z');
        self.mass_assign_with_chars(13, 7, '0', '9');
        self.mass_assign_with_chars(13, 7, 'A', 'Z');
        self.single_assign_with_char(13, 7, '_');

        // state 14 (ID)
        self.single_assign_with_char(14, 17, 'f'); // if
        self.single_assign_with_char(14, 15, 'n'); // in

        self.mass_assign_with_chars(14, 7, 'a', 'e');
        self.mass_assign_with_chars(14, 7, 'g', 'm');
        self.mass_assign_with_chars(14, 7, 'o', 'z');
        self.mass_assign_with_chars(14, 7, '0', '9');
        self.mass_assign_with_chars(14, 7, 'A', 'Z');
        self.single_assign_with_char(14, 7, '_');

        // state 15 (ID)
        self.single_assign_with_char(15, 16, 't'); // int

        self.mass_assign_with_chars(15, 7, 'a', 's');
        self.mass_assign_with_chars(15, 7, 'u', 'z');
        self.mass_assign_with_chars(15, 7, '0', '9');
        self.mass_assign_with_chars(15, 7, 'A', 'Z');
        self.single_assign_with_char(15, 7, '_');

        // state 16 (INT)
        self.mass_assign_with_chars(16, 7, 'a', 'z');
        self.mass_assign_with_chars(16, 7, '0', '9');
        self.mass_assign_with_chars(16, 7, 'A', 'Z');
        self.single_assign_with_char(16, 7, '_');

        // state 17 (IF)
        self.mass_assign_with_chars(17, 7, 'a', 'z');
        self.mass_assign_with_chars(17, 7, '0', '9');
        self.mass_assign_with_chars(17, 7, 'A', 'Z');
        self.single_assign_with_char(17, 7, '_');

        // state 18 (ID)
        self.single_assign_with_char(18, 19, 'a'); // ma

        self.mass_assign_with_chars(18, 7, 'b', 'z');
        self.mass_assign_with_chars(18, 7, '0', '9');
        self.mass_assign_with_chars(18, 7, 'A', 'Z');
        self.single_assign_with_char(18, 7, '_');

        // state 19 (ID)
        self.single_assign_with_char(19, 20, 'i'); // mai

        self.mass_assign_with_chars(19, 7, 'a', 'h');
        self.mass_assign_with_chars(19, 7, 'j', 'z');
        self.mass_assign_with_chars(19, 7, '0', '9');
        self.mass_assign_with_chars(19, 7, 'A', 'Z');
        self.single_assign_with_char(19, 7, '_');

        // state 20 (ID)
        self.single_assign_with_char(20, 21, 'n'); // main

        self.mass_assign_with_chars(20, 7, 'a', 'm');
        self.mass_assign_with_chars(20, 7, 'o', 'z');
        self.mass_assign_with_chars(20, 7, '0', '9');
        self.mass_assign_with_chars(20, 7, 'A', 'Z');
        self.single_assign_with_char(20, 7, '_');

        // state 21 (MAIN)
        self.mass_assign_with_chars(21, 7, 'a', 'z');
        self.mass_assign_with_chars(21, 7, '0', '9');
        self.mass_assign_with_chars(21, 7, 'A', 'Z');
        self.single_assign_with_char(21, 7, '_');

        // state 22 (ID)
        self.single_assign_with_char(22, 23, 'r'); // pr

        self.mass_assign_with_chars(22, 7, 'a', 'q');
        self.mass_assign_with_chars(22, 7, 's', 'z');
        self.mass_assign_with_chars(22, 7, '0', '9');
        self.mass_assign_with_chars(22, 7, 'A', 'Z');
        self.single_assign_with_char(22, 7, '_');

        // state 23 (ID)
        self.single_assign_with_char(23, 24, 'i'); // pri

        self.mass_assign_with_chars(23, 7, 'a', 'h');
        self.mass_assign_with_chars(23, 7, 'j', 'z');
        self.mass_assign_with_chars(23, 7, '0', '9');
        self.mass_assign_with_chars(23, 7, 'A', 'Z');
        self.single_assign_with_char(23, 7, '_');

        // state 24 (ID)
        self.single_assign_with_char(24, 25, 'n'); // prin

        self.mass_assign_with_chars(24, 7, 'a', 'm');
        self.mass_assign_with_chars(24, 7, 'o', 'z');
        self.mass_assign_with_chars(24, 7, '0', '9');
        self.mass_assign_with_chars(24, 7, 'A', 'Z');
        self.single_assign_with_char(24, 7, '_');

        // state 25 (ID)
        self.single_assign_with_char(25, 26, 't'); // print

        self.mass_assign_with_chars(25, 7, 'a', 's');
        self.mass_assign_with_chars(25, 7, 'u', 'z');
        self.mass_assign_with_chars(25, 7, '0', '9');
        self.mass_assign_with_chars(25, 7, 'A', 'Z');
        self.single_assign_with_char(25, 7, '_');

        // state 26 (ID)
        self.single_assign_with_char(26, 27, 'f'); // printf

        self.mass_assign_with_chars(26, 7, 'a', 'e');
        self.mass_assign_with_chars(26, 7, 'g', 'z');
        self.mass_assign_with_chars(26, 7, '0', '9');
        self.mass_assign_with_chars(26, 7, 'A', 'Z');
        self.single_assign_with_char(26, 7, '_');

        // state 27 (WRITE)
        self.mass_assign_with_chars(27, 7, 'a', 'z');
        self.mass_assign_with_chars(27, 7, '0', '9');
        self.mass_assign_with_chars(27, 7, 'A', 'Z');
        self.single_assign_with_char(27, 7, '_');

        // state 28 (ID)
        self.single_assign_with_char(28, 29, 'e'); // re

        self.mass_assign_with_chars(28, 7, 'a', 'd');
        self.mass_assign_with_chars(28, 7, 'f', 'z');
        self.mass_assign_with_chars(28, 7, '0', '9');
        self.mass_assign_with_chars(28, 7, 'A', 'Z');
        self.single_assign_with_char(28, 7, '_');

        // state 29 (ID)
        self.single_assign_with_char(29, 30, 't'); // ret

        self.mass_assign_with_chars(29, 7, 'a', 's');
        self.mass_assign_with_chars(29, 7, 'u', 'z');
        self.mass_assign_with_chars(29, 7, '0', '9');
        self.mass_assign_with_chars(29, 7, 'A', 'Z');
        self.single_assign_with_char(29, 7, '_');

        // state 30 (ID)
        self.single_assign_with_char(30, 31, 'u'); // retu

        self.mass_assign_with_chars(30, 7, 'a', 't');
        self.mass_assign_with_chars(30, 7, 'v', 'z');
        self.mass_assign_with_chars(30, 7, '0', '9');
        self.mass_assign_with_chars(30, 7, 'A', 'Z');
        self.single_assign_with_char(30, 7, '_');

        // state 31 (ID)
        self.single_assign_with_char(31, 32, 'r'); // retur

        self.mass_assign_with_chars(31, 7, 'a', 'q');
        self.mass_assign_with_chars(31, 7, 's', 'z');
        self.mass_assign_with_chars(31, 7, '0', '9');
        self.mass_assign_with_chars(31, 7, 'A', 'Z');
        self.single_assign_with_char(31, 7, '_');

        // state 32 (ID)
        self.single_assign_with_char(32, 33, 'n'); // return

        self.mass_assign_with_chars(32, 7, 'a', 'm');
        self.mass_assign_with_chars(32, 7, 'o', 'z');
        self.mass_assign_with_chars(32, 7, '0', '9');
        self.mass_assign_with_chars(32, 7, 'A', 'Z');
        self.single_assign_with_char(32, 7, '_');

        // state 33 (RETURN)
        self.mass_assign_with_chars(33, 7, 'a', 'z');
        self.mass_assign_with_chars(33, 7, '0', '9');
        self.mass_assign_with_chars(33, 7, 'A', 'Z');
        self.single_assign_with_char(33, 7, '_');

        // state 34 (ID)
        self.single_assign_with_char(34, 35, 'c'); // sc

        self.mass_assign_with_chars(34, 7, 'a', 'b');
        self.mass_assign_with_chars(34, 7, 'd', 'z');
        self.mass_assign_with_chars(34, 7, '0', '9');
        self.mass_assign_with_chars(34, 7, 'A', 'Z');
        self.single_assign_with_char(34, 7, '_');

        // state 35 (ID)
        self.single_assign_with_char(35, 36, 'a'); // sca

        self.mass_assign_with_chars(35, 7, 'b', 'z');
        self.mass_assign_with_chars(35, 7, '0', '9');
        self.mass_assign_with_chars(35, 7, 'A', 'Z');
        self.single_assign_with_char(35, 7, '_');

        // state 36 (ID)
        self.single_assign_with_char(36, 37, 'n'); // scan

        self.mass_assign_with_chars(36, 7, 'a', 'm');
        self.mass_assign_with_chars(36, 7, 'o', 'z');
        self.mass_assign_with_chars(36, 7, '0', '9');
        self.mass_assign_with_chars(36, 7, 'A', 'Z');
        self.single_assign_with_char(36, 7, '_');

        // state 37 (ID)
        self.single_assign_with_char(37, 38, 'f'); // scanf

        self.mass_assign_with_chars(37, 7, 'a', 'e');
        self.mass_assign_with_chars(37, 7, 'g', 'z');
        self.mass_assign_with_chars(37, 7, '0', '9');
        self.mass_assign_with_chars(37, 7, 'A', 'Z');
        self.single_assign_with_char(37, 7, '_');

        // state 38 (READ)
        self.mass_assign_with_chars(38, 7, 'a', 'z');
        self.mass_assign_with_chars(38, 7, '0', '9');
        self.mass_assign_with_chars(38, 7, 'A', 'Z');
        self.single_assign_with_char(38, 7, '_');

        // state 39 (ID)
        self.single_assign_with_char(39, 40, 'o'); // vo

        self.mass_assign_with_chars(39, 7, 'a', 'n');
        self.mass_assign_with_chars(39, 7, 'p', 'z');
        self.mass_assign_with_chars(39, 7, '0', '9');
        self.mass_assign_with_chars(39, 7, 'A', 'Z');
        self.single_assign_with_char(39, 7, '_');

        // state 40 (ID)
        self.single_assign_with_char(40, 41, 'i'); // voi

        self.mass_assign_with_chars(40, 7, 'a', 'h');
        self.mass_assign_with_chars(40, 7, 'j', 'z');
        self.mass_assign_with_chars(40, 7, '0', '9');
        self.mass_assign_with_chars(40, 7, 'A', 'Z');
        self.single_assign_with_char(40, 7, '_');

        // state 41 (ID)
        self.single_assign_with_char(41, 42, 'd'); // void

        self.mass_assign_with_chars(41, 7, 'a', 'c');
        self.mass_assign_with_chars(41, 7, 'e', 'z');
        self.mass_assign_with_chars(41, 7, '0', '9');
        self.mass_assign_with_chars(41, 7, 'A', 'Z');
        self.single_assign_with_char(41, 7, '_');

        // state 42 (VOID)
        self.mass_assign_with_chars(42, 7, 'a', 'z');
        self.mass_assign_with_chars(42, 7, '0', '9');
        self.mass_assign_with_chars(42, 7, 'A', 'Z');
        self.single_assign_with_char(42, 7, '_');

        // state 43 (ID)
        self.single_assign_with_char(43, 44, 'h'); // wh

        self.mass_assign_with_chars(43, 7, 'a', 'g');
        self.mass_assign_with_chars(43, 7, 'i', 'z');
        self.mass_assign_with_chars(43, 7, '0', '9');
        self.mass_assign_with_chars(43, 7, 'A', 'Z');
        self.single_assign_with_char(43, 7, '_');

        // state 44 (ID)
        self.single_assign_with_char(44, 45, 'i'); // whi

        self.mass_assign_with_chars(44, 7, 'a', 'h');
        self.mass_assign_with_chars(44, 7, 'j', 'z');
        self.mass_assign_with_chars(44, 7, '0', '9');
        self.mass_assign_with_chars(44, 7, 'A', 'Z');
        self.single_assign_with_char(44, 7, '_');

        // state 45 (ID)
        self.single_assign_with_char(45, 46, 'l'); // whil

        self.mass_assign_with_chars(45, 7, 'a', 'k');
        self.mass_assign_with_chars(45, 7, 'm', 'z');
        self.mass_assign_with_chars(45, 7, '0', '9');
        self.mass_assign_with_chars(45, 7, 'A', 'Z');
        self.single_assign_with_char(45, 7, '_');

        // state 46 (ID)
        self.single_assign_with_char(46, 47, 'e'); // while

        self.mass_assign_with_chars(46, 7, 'a', 'd');
        self.mass_assign_with_chars(46, 7, 'f', 'z');
        self.mass_assign_with_chars(46, 7, '0', '9');
        self.mass_assign_with_chars(46, 7, 'A', 'Z');
        self.single_assign_with_char(46, 7, '_');

        // state 47 (WHILE)
        self.mass_assign_with_chars(47, 7, 'a', 'z');
        self.mass_assign_with_chars(47, 7, '0', '9');
        self.mass_assign_with_chars(47, 7, 'A', 'Z');
        self.single_assign_with_char(47, 7, '_');

        // state 48 (INT_NUM)
        self.mass_assign_with_chars(48, 48, '0', '9');

        // state 49 (LBRACE)
        // state 50 (RBRACE)
        // state 51 (LSQUARE)
        // state 52 (RSQUARE)
        // state 53 (LPAR)
        // state 54 (RPAR)
        // state 55 (SEMI)
        // state 56 (PLUS)
        // state 57 (MINUS)
        // state 58 (MUL_OP)
        // state 59 (DIV_OP)
        // state 60 (AND_OP)
        self.single_assign_with_char(60, 61, '&'); // &&

        // state 61 (ANDAND)
        // state 62 (OR_OP)
        self.single_assign_with_char(62, 63, '|'); // ||

        // state 63 (OROR)
        // state 64 (NOT_OP)
        self.single_assign_with_char(64, 65, '='); // !=

        // state 65 (NOTEQ)
        // state 66 (ASSIGN)
        self.single_assign_with_char(66, 67, '='); // ==

        // state 67 (EQ)
        // state 68 (LT)
        self.single_assign_with_char(68, 69, '<'); // <<
        self.single_assign_with_char(68, 70, '='); // <=

        // state 69 (SHL_OP)
        // state 70 (LTEQ)
        // state 71 (GT)
        self.single_assign_with_char(71, 72, '>'); // >>
        self.single_assign_with_char(71, 73, '='); // >=

        // state 72 (SHR_OP)
        // state 73 (GTEQ)
        // state 74 (COMMA)
        // state 75 (ERROR)
    }

    pub fn check_state(&self, c: char, state: u32) -> u32 {
        self.arr[state as usize][c as usize]
    }

    fn mass_assign_with_chars(
        &mut self,
        cur_state: usize,
        next_state: u32,
        c_start: char,
        c_end: char,
    ) {
        let range_start = c_start as usize;
        let range_end = (c_end as usize) + 1;
        for i in range_start..range_end {
            self.arr[cur_state][i] = next_state;
        }
    }

    fn single_assign_with_char(&mut self, cur_state: usize, next_state: u32, c: char) {
        self.arr[cur_state][c as usize] = next_state;
    }
}
