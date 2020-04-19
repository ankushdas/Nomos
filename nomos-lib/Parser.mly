(* functional layer *)
%token <int> INT
%token <string> ID
%token INTEGER BOOLEAN ADDRESS LIST
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
(* printing *)
%token PINT PBOOL PSTR PADDR PCHAN
%token NEWLINE LQUOTE PRINT
(* Nomos specific *)
%token GETTXNNUM GETTXNSENDER MAKECHAN
(* session type layer *)
%token LOLLI AMPERSAND UP DOWN PRODUCT
%token LBRACE RBRACE
%token HASH DOLLAR
%token LARROW SEMI RRARROW
%token RECV SEND CASE DOT CLOSE WAIT WORK PAY GET ACQUIRE ACCEPT RELEASE DETACH ABORT
%right ANDALSO ORELSE
%left EQUALS NEQ GREATER LESS GREATEREQ LESSEQ
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc statement
%start <Ast.program> file
%%


file :
     | vl = list(decl) EOF { (vl, Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)) }
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
    | l = ID; COLON; t = stype         { (l,t) }
    ;

sp_stype:
    | x = ID                    { Ast.TpName(x) }
    | INT                       { Ast.One }
    | LPAREN; s = stype; RPAREN { s             }
    ;

sp_ftype:
    | INTEGER                       { Ast.Integer }
    | BOOLEAN                       { Ast.Boolean }
    | ADDRESS                       { Ast.Address }
    | LPAREN; f = ftype; RPAREN     { f }
    ;

stype :
    | PLUS; LBRACE; choices = separated_list(COMMA, label_stype); RBRACE        { Ast.Plus(choices) }
    | AMPERSAND; LBRACE; choices = separated_list(COMMA, label_stype); RBRACE   { Ast.With(choices) }
    | s = sp_stype; TIMES; t = stype                                            { Ast.Tensor(s,t,Ast.Unknown) }
    | s = sp_stype; LOLLI; t = stype                                            { Ast.Lolli(s,t,Ast.Unknown) }
    | INT                                                                       { Ast.One }
    | BAR; pot = potential; GREATER; t = stype                                  { Ast.PayPot(pot,t) }
    | LESS; pot = potential; BAR; t = stype                                     { Ast.GetPot(pot,t) }
    | UP; t = stype                                                             { Ast.Up(t) }
    | DOWN; t = stype                                                           { Ast.Down(t) }
    | a = sp_ftype; RIGHTARROW; t = stype                                       { Ast.FArrow(a,t) }
    | a = sp_ftype; PRODUCT; t = stype                                          { Ast.FProduct(a,t) }
    | x = ID                                                                    { Ast.TpName(x) }
    | LPAREN; s = stype; RPAREN                                                 { s }
    ;

ftype :
    | INTEGER                                               { Ast.Integer }
    | BOOLEAN                                               { Ast.Boolean }
    | ADDRESS                                               { Ast.Address }
    | a = sp_ftype; LIST; pot = potential                   { Ast.ListTP(a,pot) }
    | a = sp_ftype; RIGHTARROW; b = ftype                   { Ast.Arrow(a,b) }
    | LPAREN; a = ftype; RPAREN                             { a }
    ;

argument :
    | LPAREN; a = mid; COLON; t = stype; RPAREN     { Ast.STyped(a, t) }
    | LPAREN; a = ID; COLON; ft = ftype; RPAREN     { Ast.Functional(a, ft) }
    ;

decl : 
    | TYPE; x = ID; EQUALS; t = stype   { (Ast.TpDef (x,t), Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)) }
    | PROC; m = mode; f = ID; COLON; ctx = context_opt; TURNSTILE; LPAREN; c = mid; COLON; t = stype; RPAREN; EQUALS; e = expr                      { (Ast.ExpDecDef(f, m, ({Ast.shared = []; Ast.linear = []; Ast.ordered = ctx}, Ast.Arith(Arith.Int(0)), (c,t)), e),
                                                                                                                                                      Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                                                      ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                                                      $startpos.Lexing.pos_fname)) }
    | PROC; m = mode; f = ID; COLON; ctx = context_opt; BAR; pot = potential; MINUS; LPAREN; c = mid; COLON; t = stype; RPAREN; EQUALS; e = expr    { (Ast.ExpDecDef(f, m, ({Ast.shared = []; Ast.linear = []; Ast.ordered = ctx}, pot, (c,t)), e),
                                                                                                                                                      Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                                                      ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                                                      $startpos.Lexing.pos_fname)) }
    | EXEC; f = ID                      { (Ast.Exec(f), Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                        ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                        $startpos.Lexing.pos_fname)) }
    ;

expr :
    | LPAREN MINUS i = INT RPAREN     { {Ast.func_structure = Ast.Int (-i); func_data = 
                                        Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | LPAREN e = expr RPAREN          { e }
    | TRUE                            { {func_structure = Ast.Bool(true); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | FALSE                           { {func_structure = Ast.Bool(false); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} }
    | GETTXNNUM                       { {func_structure = Ast.GetTxnNum; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} }
    | GETTXNSENDER                    { {func_structure = Ast.GetTxnSender; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} }
    | i = INT                         { {func_structure = Ast.Int(i); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | x = ID                          { {func_structure = Ast.Var(x); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | c = cond                        { {func_structure = c; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | l = letin                       { {func_structure = l; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | lst = listVal                   { {func_structure = lst; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | a = app                         { {func_structure = a; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | c = cons                        { {func_structure = c; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | m = matchExp                    { {func_structure = m; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | l = lambdaExp                   { {func_structure = l; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | o = op                          { {func_structure = o; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | c = compOp                      { {func_structure = c; func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | r = relOp                       { {Ast.func_structure = r; Ast.func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | LBRACE; s = st; RBRACE          { {func_structure = Ast.Command(s); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} }
    ;


cond :
    | IF; ifE = expr; THEN; thenE = expr; ELSE; elseE = expr 
                                          {  If (ifE, thenE, elseE) } 
                                          %prec statement
    ;

func :
    | args = id_list; EQUALS; e = expr { {func_structure = Ast.Lambda(args, e); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    ;

letin :
    | LET; x = ID; EQUALS; e = expr; IN; inExp = expr { Ast.LetIn (x, e, inExp) } %prec statement
    | LET; FUN; x = ID; f = func; IN; inExp = expr { Ast.LetIn (x, f, inExp) } %prec statement
    ;

listVal : 
      | EMPTYLIST   { Ast.ListE [] }
      | LSQUARE; vl = list_fields; RSQUARE { Ast.ListE vl }
      ;

list_fields :
    | vl = separated_list(SEMI, expr) { vl }
    ;

cons:
    | x = expr; CONS; l = expr { Cons(x, l) }
    ;

op :
   | x = expr; PLUS; y = expr   { Ast.Op(x, Add, y) } 
   | x = expr; TIMES; y = expr  { Ast.Op(x, Mult, y) } 
   | x = expr; MINUS; y = expr  { Ast.Op(x, Sub, y) } 
   | x = expr; DIV; y = expr    { Ast.Op(x, Div, y) } 
   ;


compOp :
   | x = expr; EQUALS; y = expr             { Ast.CompOp(x, Eq, y) }
   | x = expr; EQUALS; EQUALS; y = expr     { Ast.EqAddr(x, y)}
   | x = expr; NEQ; y = expr                { Ast.CompOp(x, Neq, y) } 
   | x = expr; GREATER; y = expr            { Ast.CompOp(x, Gt, y) } 
   | x = expr; LESS; y = expr               { Ast.CompOp(x, Lt, y) } 
   | x = expr; GREATEREQ; y = expr          { Ast.CompOp(x, Geq, y) } 
   | x = expr; LESSEQ; y = expr             { Ast.CompOp(x, Leq, y) } 
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
    | LPAREN e = expr RPAREN      { e }    
    | x = ID                      { {func_structure = Var(x); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | TRUE                        { {func_structure = Bool(true); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | FALSE                       { {func_structure = Bool(false); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    | i = INT                     { {Ast.func_structure = Int(i); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)} } 
    ;

app :
    | x = ID; l = nonempty_list(arg)   { Ast.App({func_structure = Ast.Var(x); func_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)}::l) }
    | LPAREN; e = expr; RPAREN; l = nonempty_list(arg) { Ast.App(e::l) }
    ;

id_list:
    | x = ID;                   { Ast.Single(x, Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)) }
    | x = ID;  l = id_list      { Ast.Curry((x, Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                             ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                             $startpos.Lexing.pos_fname)), l) } 
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
    | LBRACE; i = INT; RBRACE { Ast.Arith(Arith.Int(i)) }
    | LBRACE; TIMES; RBRACE   { Ast.Star }
    ;

app_arg :
    | x = mid           { Ast.STArg(x) }
    | x = ID            { Ast.FArg(Ast.Var(x)) }
    ;

print_id:
    | x = ID                    { Ast.Word(x) }
    | PINT                      { Ast.PInt }
    | PBOOL                     { Ast.PBool }
    | PSTR                      { Ast.PStr }
    | PADDR                     { Ast.PAddr }
    | PCHAN                     { Ast.PChan }
    | NEWLINE                   { Ast.PNewline }
    ;


st:
    |  x = mid; LARROW; f = ID; LARROW; xs = list(app_arg); SEMI; p = st { {st_structure = Ast.Spawn(x, f, xs, p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(xs).Lexing.pos_lnum, $endpos(xs).Lexing.pos_cnum - $endpos(xs).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }
    |  x = mid; LARROW; f = ID; LARROW; xs = list(app_arg)               { {st_structure = Ast.ExpName(x, f, xs); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }  
    |  x = mid; LARROW; y = mid                                          { {st_structure = Ast.Fwd(x,y); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }
    |  SEND; x = linid; w = mid; SEMI; p = st                            { {st_structure = Ast.Send(x,w,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(w).Lexing.pos_lnum, $endpos(w).Lexing.pos_cnum - $endpos(w).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }      
    |  y = mid; LARROW; RECV; x = linid; SEMI; p = st                    { {st_structure = Ast.Recv(x,y,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }   
    |  x = mid; DOT; k = ID; SEMI; p = st                                { {st_structure = Ast.Lab(x,k,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(k).Lexing.pos_lnum, $endpos(k).Lexing.pos_cnum - $endpos(k).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }   
    |  CASE; x = linid; LPAREN; b = branches                            { {st_structure = Ast.Case(x,b); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                    ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                    $startpos.Lexing.pos_fname)} }
    |  CLOSE; x = linid                                                  { {st_structure = Ast.Close(x); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                    ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                    $startpos.Lexing.pos_fname)} }
    |  ABORT                                                            { {st_structure = Ast.Abort; st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                    ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                    $startpos.Lexing.pos_fname)} } 
    |  WAIT; x = linid; SEMI; p = st                                     { {st_structure = Ast.Wait(x,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }   
    |  WORK; pot = potential; SEMI; p = st                               { {st_structure = Ast.Work(pot, p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(pot).Lexing.pos_lnum, $endpos(pot).Lexing.pos_cnum - $endpos(pot).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} } 
    |  PAY; x = linid; pot = potential; SEMI; p = st                     { {st_structure = Ast.Pay(x, pot, p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(pot).Lexing.pos_lnum, $endpos(pot).Lexing.pos_cnum - $endpos(pot).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} } 
    |  GET; x = linid; pot = potential; SEMI; p = st                     { {st_structure = Ast.Get(x, pot, p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(pot).Lexing.pos_lnum, $endpos(pot).Lexing.pos_cnum - $endpos(pot).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }       
    |  y = mid; LARROW; ACQUIRE; x = sharedid; SEMI; p = st              { {st_structure = Ast.Acquire(x,y,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }  
    |  y = mid; LARROW; ACCEPT; x = sharedid; SEMI; p = st               { {st_structure = Ast.Accept(x,y,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} } 
    |  y = mid; LARROW; RELEASE; x = linid; SEMI; p = st                 { {st_structure = Ast.Release(x,y,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} } 
    |  y = mid; LARROW; DETACH; x = linid; SEMI; p = st                  { {st_structure = Ast.Detach(x,y,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }
    |  SEND; x = linid; e = arg; SEMI; p = st                            { {st_structure = Ast.SendF(x,e,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(e).Lexing.pos_lnum, $endpos(e).Lexing.pos_cnum - $endpos(e).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} } 
    |  y = ID; EQUALS; RECV; x = linid; SEMI; p = st                     { {st_structure = Ast.RecvF(x,y,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(x).Lexing.pos_lnum, $endpos(x).Lexing.pos_cnum - $endpos(x).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }   
    |  LET; x = ID; EQUALS; e = expr; SEMI; p = st                       { {st_structure = Ast.Let(x,e,p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(e).Lexing.pos_lnum, $endpos(e).Lexing.pos_cnum - $endpos(e).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }   
    |  IF; ifE = expr; THEN; thenE = st; ELSE; elseE = st                    { {Ast.st_structure = Ast.IfS(ifE, thenE, elseE); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }
    |  x = mid; COLON; t = stype; LARROW; MAKECHAN; n = INT; SEMI; p = st     { {Ast.st_structure = Ast.MakeChan(x, t, n, p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos.Lexing.pos_lnum, $endpos.Lexing.pos_cnum - $endpos.Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }
    |  PRINT; LPAREN; l = list(print_id); RPAREN; SEMI; p = st    { {Ast.st_structure = Ast.Print(l, [], p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(l).Lexing.pos_lnum, $endpos(l).Lexing.pos_cnum - $endpos(l).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }            
    |  PRINT; LPAREN; l = list(print_id); COMMA; args = separated_list(COMMA, app_arg); RPAREN; SEMI; p = st         { {Ast.st_structure = Ast.Print(l, args, p); st_data = Some(($startpos.Lexing.pos_lnum, $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1),
                                                                                                                   ($endpos(args).Lexing.pos_lnum, $endpos(args).Lexing.pos_cnum - $endpos(args).Lexing.pos_bol + 1),
                                                                                                                   $startpos.Lexing.pos_fname)} }
    ;
