(* functional layer *)
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
(* session type layer *)
%token LBRACE RBRACE
%token HASH DOLLAR
%token LARROW SEMI RRARROW
%token RECV SEND CASE DOT CLOSE WAIT WORK PAY GET ACQUIRE ACCEPT RELEASE DETACH
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
    | LPAREN MINUS i = INT RPAREN     { {func_structure = Int (-i); func_data = ()} } 
    | LPAREN e = expr RPAREN          { e }
    | TRUE                            { {func_structure = Bool true; func_data = ()}  }
    | FALSE                           { {func_structure = Bool false; func_data = ()} }
    | i = INT                         { {func_structure = Int i; func_data = ()}  }
    | x = ID                          { {func_structure = Var x; func_data = ()}  }
    | c = cond                        { {func_structure = c; func_data = ()} }
    | l = letin                       { {func_structure = l; func_data = ()} }
    | lst = listVal                   { {func_structure = lst; func_data = ()} }
    | a = app                         { {func_structure = a; func_data = ()} }
    | c = cons                        { {func_structure = c; func_data = ()} }
    | m = matchExp                    { {func_structure = m; func_data = ()} }
    | l = lambdaExp                   { {func_structure = l; func_data = ()} }
    | o = op                          { {func_structure = o; func_data = ()} }
    | c = compOp                      { {func_structure = c; func_data = ()} }
    | r = relOp                       { {func_structure = r; func_data = ()} }
    | LBRACE; s = st; RBRACE          { {func_structure = Ast.Command(s); func_data = ()} } 
    ;

cond :
    | IF; ifE = expr; THEN; thenE = expr; ELSE; elseE = expr 
                                          {  If (ifE, thenE, elseE) } 
                                          %prec statement
    ;

func :
    | args = id_list; EQUALS; e = expr { {func_structure = Ast.Lambda(args, e); func_data = ()} }
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
   | x = expr; PLUS; y = expr   { Ast.Op(x, Add, y) } 
   | x = expr; TIMES; y = expr  { Ast.Op(x, Sub, y) } 
   | x = expr; MINUS; y = expr  { Ast.Op(x, Mult, y) } 
   | x = expr; DIV; y = expr    { Ast.Op(x, Div, y) } 
   ;


compOp :
   | x = expr; EQUALS; y = expr   { Ast.CompOp(x, Eq, y) } 
   | x = expr; NEQ; y = expr  { Ast.CompOp(x, Neq, y) } 
   | x = expr; GREATER; y = expr  { Ast.CompOp(x, Gt, y) } 
   | x = expr; LESS; y = expr    { Ast.CompOp(x, Lt, y) } 
   | x = expr; GREATEREQ; y = expr  { Ast.CompOp(x, Geq, y) } 
   | x = expr; LESSEQ; y = expr    { Ast.CompOp(x, Leq, y) } 
   ;


relOp :
   | x = expr; ANDALSO; y = expr   { Ast.RelOp(x, And, y) } 
   | x = expr; ORELSE; y = expr  { Ast.RelOp(x, Or, y) } 
   ;


matchExp :
    | MATCH; x = expr; WITH; EMPTYLIST; RIGHTARROW; y = expr; BAR; a = ID; 
      CONS; b = ID; RIGHTARROW; c = expr   { Ast.Match(x, y, a, b, c)  } %prec statement
    ;  


arg :
    | LPAREN e = expr RPAREN      { e          }    
    | x = ID                      { {func_structure = Var x; func_data = ()} }     
    | TRUE                        { {func_structure = Bool true; func_data = ()}  }   
    | FALSE                       { {func_structure = Bool false; func_data = ()} }   
    | i = INT                     { {Ast.func_structure = Int i; func_data = ()}  }
    ;

app :
    | x = ID; l = nonempty_list(arg)   { Ast.App({func_structure = Ast.Var(x); func_data = ()}::l) }
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

m :
    | DOLLAR { Ast.Dollar }
    | HASH   { Ast.Hash   }
    ;


mid:
    | str = m; x = ID { (str, x, A.Unknown) }
    ;

linid:
    | DOLLAR; x = ID  { (Ast.Dollar, x, A.Unknown) }
    ;


sharedid:
    | HASH; x = ID  { (Ast.Hash, x, A.Unknown) }
    ;

branches :
    | k = ID; RRARROW; p = st; b = branches2 { (k, p)::b }    

branches2 :
    | BAR; b = branches { b }
    | RPAREN { [] }
    ;

potential :
    | LPAREN; i = INT; RPAREN { Ast.Arith(Arith.Int(i)) }
    | LPAREN; TIMES; RPAREN   { Ast.Star }
    ;

st:
    |  x = mid; LARROW; f = ID; LARROW; xs = list(mid); SEMI; p = st 
    { {st_structure = Ast.Spawn(x, f, xs, p); st_data = ()} }
    |  x = mid; LARROW; f = ID; LARROW; xs = list(mid)
    { {st_structure = Ast.ExpName(x, f, xs); st_data = ()} }
    |  x = mid; LARROW; y = mid 
    { {st_structure = Ast.Fwd(x, y); st_data = ()} }
    |  SEND; x = linid; w = mid; SEMI; p = st
    { {st_structure = Ast.Send(x, w, p); st_data = ()} }
    |  y = mid; LARROW; RECV; x = linid; SEMI; p = st
    { {st_structure = Ast.Recv(x, y, p); st_data = ()} }
    |  x = linid; DOT; k = ID; SEMI; p = st
    { {st_structure = Ast.Lab(x, k, p); st_data = ()} }
    |  CASE; x = linid; LPAREN; b = branches 
    { {st_structure = Ast.Case(x, b); st_data = ()} }
    |  CLOSE; x = linid 
    { {st_structure = Ast.Close(x); st_data = ()} }
    |  WAIT; x = linid; p = st 
    { {st_structure = Ast.Wait(x, p); st_data = ()} }
    |  WORK; pot = potential; SEMI; p = st
    { {st_structure = Ast.Work(pot, p); st_data = ()} }
    |  PAY; x = linid; pot = potential; SEMI; p = st
    { {st_structure = Ast.Pay(x, pot, p); st_data = ()} }
    |  GET; x = linid; pot = potential; SEMI; p = st
    { {st_structure = Ast.Get(x, pot, p); st_data = ()} }
    |  y = linid; LARROW; ACQUIRE; x = sharedid; SEMI; p = st
    { {st_structure = Ast.Acquire(x,y,p); st_data = ()} }
    |  y = linid; LARROW; ACCEPT; x = sharedid; SEMI; p = st
    { {st_structure = Ast.Accept(x,y,p); st_data = ()} }
    |  y = sharedid; LARROW; RELEASE; x = linid; SEMI; p = st
    { {st_structure = Ast.Release(x,y,p); st_data = ()} }
    |  y = sharedid; LARROW; DETACH; x = linid; SEMI; p = st
    { {st_structure = Ast.Detach(x,y,p); st_data = ()} }
    |  SEND; x = linid; LPAREN; e = expr; RPAREN; SEMI; p = st
    { {st_structure = Ast.SendF(x, e, p); st_data = ()} }
    |  y = ID; EQUALS; RECV; x = linid; SEMI; p = st
    { {st_structure = Ast.RecvF(x, y, p); st_data = ()} }
    |  LET; x = ID; EQUALS; e = expr; SEMI; p = st
    { {st_structure = Ast.Let(x, e, p); st_data = ()} }
    ;

