# Grammar 

### Arithmetic Expressions

In arithmetic expressions, operator precedence is as follows.

```
* > + = -
```

All operators are left associative.

Note: right now, the order of branches must match
the order of the alternatives in the external or
internal choice type

### Comment syntax

```
% ... \n
(* ... *)  (multiline, properly nested)
```

### Identifiers

```
<id_start> = [a-zA-Z_$?!']

<id> = <id_start> (<id_start> | [0-9])*
         
<nat> = ([0-9])*
         
<binop> = + | - | *
         
<arith> = <nat> | <arith> <binop> <arith> | (<arith>)

<idx> ::= { <arith> }

<type> ::= 1
         | + { <choices> }
         | & { <choices> }
         | <type> * <type>
         | <type> -o <type>
         | | [<idx>] > <type>   % Provide potential <arith> (default: 1)
         | < [<idx>] | <type>   % Obtain potential <arith> (default: 1)

<choices> ::= <label> : <type>
            | <label> : <type>, <choices>

<context> ::= <nil>
            | <id> : <type>, <context>

<turnstile> ::= |-              % zero potential
              | | <idx> -       % with potential <arith>

<id_list> ::= .
            | <id> <id_list>

<exp> ::= <id> <- <id>
        | <id> <- <id> <- <id_list>
        | <id> <- <id> <- <id_list> ; <exp>
        | <id>.<label> ; <exp>
        | case <id> ( <branches> )
        | close <id>
        | wait <id> ; <exp>
        | send <id> <id> ; <exp>
        | <id> <- recv <id> ; <exp>
        | ( <exp> )
        | work [<idx>] ; <exp>          % spend one token
        | get <id> [<idx>] ; <exp>      % receive one token
        | pay <id> [<idx>] ; <exp>      % send one token


<branches> ::= <label> => <exp>
             | <label> => <exp> | <branches>

<var> ::= _ | <id>

<context_opt> ::= . | <context>

<context> ::= <id> : <type> | <id> : <type>, <context>

<decl> ::= type <id> = <type>
         | eqtype <id> = <id>
         | proc <id> : <context_opt> <turnstile> <id> : <type> = <exp>
         | exec <id>

<outcome> ::= error
            | success

<pragma> ::= #options <command line option>\n
           | #test <outcome>\n'

<prog> ::= <pragma>* <decl>*
```
