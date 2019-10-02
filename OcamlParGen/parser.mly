%token <int> INT
%token <string> ID
%token LPAREN RPAREN
%token TRUE FALSE
%token IF THEN ELSE
%token LET IN
%token EMPTYLIST LSQUARE RSQUARE CONS COMMA
%token EQUALS
%token MATCH WITH BAR 
%token APP FUN RIGHTARROW
%token PLUS MINUS TIMES DIV
%token EOF
%nonassoc statement
%left CONS
%left PLUS MINUS
%left TIMES DIV
%start <Ast.expr option> prog
%%

prog : 
    | e = expr EOF { Some e }
    ;

expr :
    | LPAREN MINUS i = INT RPAREN     { Int (-i) } 
    | LPAREN e = expr RPAREN { e }
    | TRUE              { Bool true  }
    | FALSE             { Bool false }
    | i = INT           { Int i  }
    | x = ID            { Var x  }
    | c = cond          { c           }
    | l = letin         { l           }
    | lst = listVal     { lst         }
    | a = app           { a           }
    | c = cons          { c           }
    | m = matchExp      { m           }
    | l = lambdaExp     { l           }
    | o = op            { o           }
    ;
    
cond :
    | IF; ifE = expr; THEN; thenE = expr; ELSE; elseE = expr 
                                          {  IfWithElse (ifE, thenE, elseE) } %prec statement
    ;

bind  :
    |   x = ID; EQUALS; e = expr { Binding(x, e)  }
    ;

letin :
    | LET; b = bind; IN; inExp = expr { Ast.LetIn (b, inExp) } %prec statement
    ;

listVal : 
      | EMPTYLIST   { Ast.List [] }
      | LSQUARE; vl = list_fields; RSQUARE { Ast.List vl }
      ;

list_fields :
    | vl = separated_list(COMMA, expr) { vl }
    ;

cons:
    | x = expr; CONS; l = expr { Cons(x, l) }
    ;

op :
   | x = expr; PLUS; y = expr   { Ast.Op(x, "+", y) } 
   | x = expr; TIMES; y = expr  { Ast.Op(x, "-", y) } 
   | x = expr; MINUS; y = expr  { Ast.Op(x, "*", y) } 
   | x = expr; DIV; y = expr    { Ast.Op(x, "/", y) } 
   ;

matchExp :
    | MATCH; x = expr; WITH; EMPTYLIST; RIGHTARROW; y = expr; BAR; a = ID; 
      CONS; b = ID; RIGHTARROW; c = expr   { Ast.Match(x, y, a, b, c)  } %prec statement
    ;  


app :
    |  APP; e1 = expr; e2 = expr { Ast.App(e1, e2)  } %prec statement
    ;

id_list:
    | x = ID { Ast.Single(x) }
    | x = ID; l = id_list { Ast.Curry(x, l) } 
    ;

lambdaExp :
    | FUN;  args = id_list; RIGHTARROW; body = expr 
                                        { Ast.Lambda(args, body)  } %prec statement
    ;
