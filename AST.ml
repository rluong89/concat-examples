type program = expression list
                                            
and expression =
  Const of string | Extract of pos_expression * pos_expression
                                                  
and pos_expression =
  Forward of int | Backward of int | After of regexp | Before of regexp

and ini =
  Epsilon_d | Start

and fin =
  Epsilon_f | End
                                                                   
and regexp =
  ini * token list * fin
                                  
and token =
  Plus of class_ast | PlusComp of class_ast

and class_ast =
  Alphanumeric | Numeric | Alpha | Lower | Upper | Special of char
