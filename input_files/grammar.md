<!-- Grammar Rule -->
<!-- 0. S' -> program EOF -->
1. program -> statements
2. program -> var_declarations statements

3. var_declarations -> var_declaration var_declarations
4. var_declarations -> EMPTY

5. var_declaration -> INT declaration_list SEMI

6. declaration_list -> declaration declaration_list_00
7. declaration_list_00 -> COMMA declaration declaration_list_00
8. declaration_list_00 -> EMPTY

9. declaration -> ID ASSIGN exp
10. declaration -> ID LSQUARE exp RSQUARE
11. declaration -> ID

12. code_block -> statement
13. code_block -> LBRACE statements RBRACE

14. statements -> statement statements
15. statements -> EMPTY

16. statement -> assign_statement SEMI
17. statement -> control_statement
18. statement -> read_write_statement SEMI
19. statement -> SEMI

20. control_statement -> if_statement
21. control_statement -> while_statement
22. control_statement -> do_while_statement SEMI
23. control_statement -> return_statement SEMI

24. read_write_statement -> read_statement
25. read_write_statement -> write_statement

26. assign_statement -> ID LSQUARE exp RSQUARE ASSIGN exp
27. assign_statement -> ID ASSIGN exp

28. if_statement -> if_stmt
29. if_statement -> if_stmt ELSE code_block

30. if_stmt -> IF LPAR exp RPAR code_block

31. while_statement -> WHILE LPAR exp RPAR code_block

32. do_while_statement -> DO code_block WHILE LPAR exp RPAR

33. return_statement -> RETURN

34. read_statement -> READ LPAR ID RPAR

35. write_statement -> WRITE LPAR exp RPAR

36. exp -> exp_1 exp_00
37. exp_00 -> OROR exp_1 exp_00
38. exp_00 -> EMPTY

39. exp_1 -> exp_2 exp_11
40. exp_11 -> ANDAND exp_2 exp_11
41. exp_11 -> EMPTY

42. exp_2 -> exp_3 exp_22
43. exp_22 -> OR_OP exp_3 exp_22
44. exp_22 -> EMPTY

45. exp_3 -> exp_4 exp_33
46. exp_33 -> AND_OP exp_4 exp_33
47. exp_33 -> EMPTY

48. exp_4 -> exp_5 exp_44
49. exp_44 -> EQ exp_5 exp_44
50. exp_44 -> NOTEQ exp_5 exp_44
51. exp_44 -> EMPTY

52. exp_5 -> exp_6 exp_55
53. exp_55 -> LT exp_6 exp_55
54. exp_55 -> GT exp_6 exp_55
55. exp_55 -> LTEQ exp_6 exp_55
56. exp_55 -> GTEQ exp_6 exp_55
57. exp_55 -> EMPTY

58. exp_6 -> exp_7 exp_66
59. exp_66 -> SHL_OP exp_7 exp_66
60. exp_66 -> SHR_OP exp_7 exp_66
61. exp_66 -> EMPTY

62. exp_7 -> exp_8 exp_77
63. exp_77 -> PLUS exp_8 exp_77
64. exp_77 -> MINUS exp_8 exp_77
65. exp_77 -> EMPTY

66. exp_8 -> exp_9 exp_88
67. exp_88 -> MUL_OP exp_9 exp_88
68. exp_88 -> DIV_OP exp_9 exp_88
69. exp_88 -> EMPTY

70. exp_9 -> NOT_OP exp_10
71. exp_9 -> MINUS exp_10
72. exp_9 -> exp_10

73. exp_10 -> ID LSQUARE exp RSQUARE
74. exp_10 -> ID
75. exp_10 -> INT_NUM
76. exp_10 -> LPAR exp RPAR