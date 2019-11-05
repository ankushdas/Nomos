(* functional layer *)
%token <int> INT
%token <string> ID
%token INTEGER BOOLEAN LIST
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
%token TYPE PROC ASSET CONTRACT TRANSACTION TURNSTILE EXEC COLON
(* session type layer *)
%token LOLLI AMPERSAND UP DOWN PRODUCT
%token LBRACE RBRACE
%token HASH DOLLAR
%token LARROW SEMI RRARROW
%token RECV SEND CASE DOT CLOSE WAIT WORK PAY GET ACQUIRE ACCEPT RELEASE DETACH
%right ANDALSO ORELSE
%left EQUALS NEQ GREATER LESS GREATEREQ LESSEQ
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc statement
%start <Ast.program> file
%%


file :
     | vl = list(decl) EOF { vl }
     ;

mode :
    | ASSET         { Ast.Pure }
    | CONTRACT      { Ast.Shared }
    | TRANSACTION   { Ast.Transaction }
    ;

context_opt :
    | DOT                                   { [] }
    | l = separated_list(COMMA, argument)   { l }
    ;

label_stype :
    | l = ID; t = stype         { (l,t) }
    ;

sp_stype:
    | x = ID                    { Ast.TpName(x) }
    | LPAREN; s = stype; RPAREN { s             }
    ;

sp_ftype:
    | x = ID                    { Ast.FTpName(x)    }
    | LPAREN; f = ftype; RPAREN { f             }
    ;

stype :
    | PLUS; LBRACE; choices = separated_list(COMMA, label_stype); RBRACE        { Ast.Plus(choices) }
    | AMPERSAND; LBRACE; choices = separated_list(COMMA, label_stype); RBRACE   { Ast.With(choices) }
    | s = sp_stype; TIMES; t = stype                                            { Ast.Tensor(s,t,Ast.Unknown) }
    | s = sp_stype; LOLLI; t = stype                                             { Ast.Lolli(s,t,Ast.Unknown) }
    | INT                                                                       { Ast.One }
    | BAR; pot = potential; GREATER; t = stype                                  { Ast.PayPot(pot,t) }
    | LESS; pot = potential; BAR; t = stype                                     { Ast.GetPot(pot,t) }
    | UP; t = stype                                                             { Ast.Up(t) }
    | DOWN; t = stype                                                           { Ast.Down(t) }
    | a = sp_ftype; RIGHTARROW; t = stype                                          { Ast.FArrow(a,t) }
    | a = sp_ftype; PRODUCT; t = stype                                             { Ast.FProduct(a,t) }
    | x = ID                                                                    { Ast.TpName(x) }
    ;

ftype :
    | INTEGER                                               { Ast.Integer }
    | BOOLEAN                                               { Ast.Boolean }
    | t = sp_ftype; LIST; pot = potential      { Ast.ListTP(t,pot) }
    | a = sp_ftype; RIGHTARROW; b = ftype      { Ast.Arrow(a,b) }
    ;

argument :
    | LPAREN; a = mid; COLON; t = stype     { Ast.STyped(a, t) }
    | LPAREN; a = ID; COLON; ft = ftype     { Ast.Functional(a, ft) }
    ;

decl : 
    | TYPE; x = ID; EQUALS; t = stype   { Ast.TpDef (x,t) }
    | PROC; m = mode; f = ID; COLON; ctx = context_opt; TURNSTILE; c = mid; COLON; t = stype; EQUALS; e = expr    { Ast.ExpDecDef(f, m, (ctx, Ast.Arith(Arith.Int(0)), (c,t)), e) }
    | PROC; m = mode; f = ID; COLON; ctx = context_opt; BAR; pot = potential; MINUS; c = mid; COLON; t = stype; EQUALS; e = expr    { Ast.ExpDecDef(f, m, (ctx, pot, (c,t)), e) }
    | EXEC; f = ID                      { Ast.Exec(f) }
    ;

expr :

    | LPAREN MINUS i = INT RPAREN     { {Ast.func_structure = Ast.Int (-i); func_data = ()} } 
    | LPAREN e = expr RPAREN          { e }
    | TRUE                            { {func_structure = Ast.Bool(true); func_data = ()}  }
    | FALSE                           { {func_structure = Ast.Bool(false); func_data = ()} }
    | i = INT                         { {func_structure = Ast.Int(i); func_data = ()}  }
    | x = ID                          { {func_structure = Ast.Var(x); func_data = ()}  }
    | c = cond                        { {func_structure = c; func_data = ()} }
    | l = letin                       { {func_structure = l; func_data = ()} }
    | lst = listVal                   { {func_structure = lst; func_data = ()} }
    | a = app                         { {func_structure = a; func_data = ()} }
    | c = cons                        { {func_structure = c; func_data = ()} }
    | m = matchExp                    { {func_structure = m; func_data = ()} }
    | l = lambdaExp                   { {func_structure = l; func_data = ()} }
    | o = op                          { {func_structure = o; func_data = ()} }
    | c = compOp                      { {func_structure = c; func_data = ()} }
    | r = relOp                       { {Ast.func_structure = r; Ast.func_data = ()} }
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
    | vl = separated_list(SEMI, expr) { vl }
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
   | x = expr; EQUALS; y = expr         { Ast.CompOp(x, Eq, y) } 
   | x = expr; NEQ; y = expr            { Ast.CompOp(x, Neq, y) } 
   | x = expr; GREATER; y = expr        { Ast.CompOp(x, Gt, y) } 
   | x = expr; LESS; y = expr           { Ast.CompOp(x, Lt, y) } 
   | x = expr; GREATEREQ; y = expr      { Ast.CompOp(x, Geq, y) } 
   | x = expr; LESSEQ; y = expr         { Ast.CompOp(x, Leq, y) } 
   ;


relOp :
   | x = expr; ANDALSO; y = expr    { Ast.RelOp(x, Ast.And, y) } 
   | x = expr; ORELSE; y = expr     { Ast.RelOp(x, Ast.Or, y) } 
   ;


matchExp :
    | MATCH; x = expr; WITH; EMPTYLIST; RIGHTARROW; y = expr; BAR; a = ID; 
      CONS; b = ID; RIGHTARROW; c = expr   { Ast.Match(x, y, a, b, c)  } %prec statement
    ;  


arg :
    | LPAREN e = expr RPAREN      { e          }    
    | x = ID                      { {func_structure = Var(x); func_data = ()} }     
    | TRUE                        { {func_structure = Bool(true); func_data = ()}  }   
    | FALSE                       { {func_structure = Bool(false); func_data = ()} }   
    | i = INT                     { {Ast.func_structure = Int(i); func_data = ()}  }
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
    | str = m; x = ID { (str, x, Ast.Unknown) }
    ;

linid:
    | DOLLAR; x = ID  { (Ast.Dollar, x, Ast.Unknown) }
    ;


sharedid:
    | HASH; x = ID  { (Ast.Hash, x, Ast.Unknown) }
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

st_struct:
    |  x = mid; LARROW; f = ID; LARROW; xs = list(mid); SEMI; p = st { Ast.Spawn(x, f, xs, p) }
    |  x = mid; LARROW; f = ID; LARROW; xs = list(mid)               { Ast.ExpName(x, f, xs) }
    |  x = mid; LARROW; y = mid                                      { Ast.Fwd(x,y) }
    |  SEND; x = linid; w = mid; SEMI; p = st                        { Ast.Send(x,w,p) }
    |  y = mid; LARROW; RECV; x = linid; SEMI; p = st                { Ast.Recv(x,y,p) }
    |  x = mid; DOT; k = ID; SEMI; p = st                            { Ast.Lab(x,k,p)  }
    |  CASE; x = linid; LPAREN; b = branches                         { Ast.Case(x,b) }
    |  CLOSE; x = linid                                              { Ast.Close(x)  } 
    |  WAIT; x = linid; p = st                                       { Ast.Wait(x,p) } 
    |  WORK; pot = potential; SEMI; p = st                           { Ast.Work(pot, p) }
    |  PAY; x = linid; pot = potential; SEMI; p = st                 { Ast.Pay(x, pot, p) }
    |  GET; x = linid; pot = potential; SEMI; p = st                 { Ast.Get(x, pot, p) }
    |  y = mid; LARROW; ACQUIRE; x = sharedid; SEMI; p = st          { Ast.Acquire(x,y,p) }
    |  y = mid; LARROW; ACCEPT; x = sharedid; SEMI; p = st           { Ast.Accept(x,y,p) }
    |  y = mid; LARROW; RELEASE; x = linid; SEMI; p = st             { Ast.Release(x,y,p) }
    |  y = mid; LARROW; DETACH; x = linid; SEMI; p = st              { Ast.Detach(x,y,p)  }
    |  SEND; x = linid; LPAREN; e = expr; RPAREN; SEMI; p = st       { Ast.SendF(x,e,p)   }
    |  y = ID; EQUALS; RECV; x = linid; SEMI; p = st                 { Ast.RecvF(x,y,p)   }
    |  LET; x = ID; EQUALS; e = expr; SEMI; p = st                   { Ast.Let(x,e,p)  }
    ;

st :
    | s = st_struct { {st_structure = s; st_data = () } }
    ;
