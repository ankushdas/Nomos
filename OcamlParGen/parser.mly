%token <int> INT
%token <string> ID
%token APP
%token LPAREN RPAREN
%token TRUE FALSE
%token IF THEN ELSE
%token LET IN
%token COLON
%token EMPTYLIST LSQUARE RSQUARE CONS COMMA
%token EQUALS
%token INTEGER BOOLEAN LIST
%token MATCH WITH BAR 
%token FUN RIGHTARROW
%token PLUS MINUS TIMES DIV
%token EOF
%token NEQ GREATER LESS GREATEREQ LESSEQ
%token ANDALSO ORELSE
%token DOUBLESEMI
%right RIGHTARROW
%nonassoc LIST
%nonassoc statement
%right ANDALSO ORELSE
%left EQUALS NEQ GREATER LESS GREATEREQ LESSEQ
%right CONS
%left PLUS MINUS
%left TIMES DIV
%start <Ast.programList option> file
%%


file :
     | vl = separated_list(DOUBLESEMI, prog) EOF { Some(Ast.PL vl) }
     ;

prog : 
    | e = expr COLON typ = typeVal { Ast.Program (e, typ) }
    ;

typeVal :
    | INTEGER { Ast.Integer }
    | BOOLEAN { Ast.Boolean }
    | t1 =  typeVal RIGHTARROW t2 = typeVal { Ast.Arrow(t1, t2) }
    | t1 = typeVal LIST { Ast.ListTP(t1) }
    
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
    | c = compOp        { c           }
    | r = relOp         { r           }
    ;

cond :
    | IF; ifE = expr; THEN; thenE = expr; ELSE; elseE = expr 
                                          {  If (ifE, thenE, elseE) } 
                                          %prec statement
    ;

bind  :
    |   x = ID; COLON; t = typeVal; EQUALS; e = expr { Binding(x, e, t)  }
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
   | x = expr; TIMES; y = expr  { Ast.Op(x, "*", y) } 
   | x = expr; MINUS; y = expr  { Ast.Op(x, "-", y) } 
   | x = expr; DIV; y = expr    { Ast.Op(x, "/", y) } 
   ;


compOp :
   | x = expr; EQUALS; y = expr   { Ast.CompOp(x, "=", y) } 
   | x = expr; NEQ; y = expr  { Ast.CompOp(x, "<>", y) } 
   | x = expr; GREATER; y = expr  { Ast.CompOp(x, ">", y) } 
   | x = expr; LESS; y = expr    { Ast.CompOp(x, "<", y) } 
   | x = expr; GREATEREQ; y = expr  { Ast.CompOp(x, ">=", y) } 
   | x = expr; LESSEQ; y = expr    { Ast.CompOp(x, "<=", y) } 
   ;


relOp :
   | x = expr; ANDALSO; y = expr   { Ast.RelOp(x, "&&", y) } 
   | x = expr; ORELSE; y = expr  { Ast.RelOp(x, "||", y) } 
   ;


matchExp :
    | MATCH; x = expr; COLON; t = typeVal; WITH; EMPTYLIST; RIGHTARROW; y = expr; BAR; a = ID; 
      CONS; b = ID; RIGHTARROW; c = expr   { Ast.Match((x,t), y, a, b, c)  } %prec statement
    ;  


app :
    |  APP LPAREN e1 = expr COLON t1 = typeVal COMMA e2 = expr COLON t2 = typeVal RPAREN { Ast.App((e1, t1), (e2, t2))  }
    ;

id_list:
    | LPAREN; x = ID; COLON; t = typeVal RPAREN { Ast.Single(x,t) }
    | LPAREN; x = ID; COLON; t = typeVal; RPAREN; l = id_list { Ast.Curry((x,t), l) } 
    ;

lambdaExp :
    | FUN;  args = id_list; RIGHTARROW; body = expr 
                                        { Ast.Lambda(args, body)  } %prec statement
    ;
