%token <int> INT
%token <string> ID
%token LPAREN RPAREN
%token TRUE FALSE
%token IF THEN ELSE
%token LET IN
%token EMPTYLIST LSQUARE RSQUARE CONS COMMA
%token EQUALS
%token MATCH WITH BAR 
%token FUN RIGHTARROW
%token PLUS MINUS TIMES DIV
%token EOF
%token NEQ GREATER LESS GREATEREQ LESSEQ
%token ANDALSO ORELSE
%token DOUBLESEMI
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
    | e = expr { Ast.Program (e) }
    ;
    
expr :
    | LPAREN MINUS i = INT RPAREN     { {structure = Int (-i); data = ()} } 
    | LPAREN e = expr RPAREN          { e }
    | TRUE                            { {structure = Bool true; data = ()}  }
    | FALSE                           { {structure = Bool false; data = ()} }
    | i = INT                         { {structure = Int i; data = ()}  }
    | x = ID                          { {structure = Var x; data = ()}  }
    | c = cond                        { {structure = c; data = ()} }
    | l = letin                       { {structure = l; data = ()} }
    | lst = listVal                   { {structure = lst; data = ()} }
    | a = app                         { {structure = a; data = ()} }
    | c = cons                        { {structure = c; data = ()} }
    | m = matchExp                    { {structure = m; data = ()} }
    | l = lambdaExp                   { {structure = l; data = ()} }
    | o = op                          { {structure = o; data = ()} }
    | c = compOp                      { {structure = c; data = ()} }
    | r = relOp                       { {structure = r; data = ()} }
    ;

cond :
    | IF; ifE = expr; THEN; thenE = expr; ELSE; elseE = expr 
                                          {  If (ifE, thenE, elseE) } 
                                          %prec statement
    ;

func :
    | args = id_list; EQUALS; e = expr { {structure = Ast.Lambda(args, e); data = ()} }
    ;

letin :
    | LET; x = ID; EQUALS; e = expr; IN; inExp = expr { Ast.LetIn (x, e, inExp) } %prec statement
    | LET; FUN; x = ID; f = func; IN; inExp = expr { Ast.LetIn (x, f, inExp) } %prec statement
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
    | MATCH; x = expr; WITH; EMPTYLIST; RIGHTARROW; y = expr; BAR; a = ID; 
      CONS; b = ID; RIGHTARROW; c = expr   { Ast.Match(x, y, a, b, c)  } %prec statement
    ;  


arg :
    | LPAREN e = expr RPAREN      { e          }    
    | x = ID                      { {structure = Var x; data = ()} }     
    | TRUE                        { {structure = Bool true; data = ()}  }   
    | FALSE                       { {structure = Bool false; data = ()} }   
    | i = INT                     { {Ast.structure = Int i; data = ()}  }
    ;

app :
    | x = ID; l = nonempty_list(arg)   { Ast.App({structure = Ast.Var(x); data = ()}::l) }
    | LPAREN e = expr RPAREN l = nonempty_list(arg) { Ast.App(e::l) }
    ;

id_list:
    | x = ID;                   { Ast.Single(x) }
    | x = ID;  l = id_list      { Ast.Curry(x, l) } 
    ;

lambdaExp :
    | FUN;  args = id_list; RIGHTARROW; body = expr 
                                        { Ast.Lambda(args, body)  } %prec statement
    ;
