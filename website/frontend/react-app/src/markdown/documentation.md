## Writing Nomos programs
Writing session-typed programs needs some guidance. First, I will introduce the basic declarations. There are three forms of declarations:

#### Type Definitions
New type names can be defined using the following syntax `type v = A` where type name `v` has definition `A`. As an example, the `auction` type is defined as follows:
```
type auction = /\ <{*}| +{running : &{bid : int -> money -o |{*}> \/ auction,
                                      cancel : |{*}> \/ auction},
                          ended : &{collect : int -> +{won : lot * |{*}> \/ auction,
                                                       lost : money * |{*}> \/ auction},
                                    cancel : |{*}> \/ auction}}
```
Here, `/\` and `\/` are used to denote up-shift and down-shift, `<{q}|` and `|{q}>` are used to receive and send potential resp., `->` and `^` are used to receive and send functional data, `-o` and `*` are used to receive and send channels, `+` and `&` denote internal and external choice, and `1` indicates termination. Note that `*` can be used in place of `q` to denote unknown potential, which is later inferred by the compiler.

Formally, the grammar for session types is as follows:
```
<A> ::= +{l1 : <A1>, ..., ln : <An>}       // internal choice
      | &{l1 : <A1>, ..., ln : <An>}       // external choice
      | <A> * <A>                          // tensor
      | <A> -o <A>                         // lolli
      | 1                                  // one
      | |{q}> <A>                          // paypot (send q units of potential, continue with A)
      | <{q}| <A>                          // getpot (receive q units of potential, continue with A)
      | /\ <A>                             // up (linear to shared)
      | \/ <A>                             // down (shared to linear)
      | <t> ^ <A>                          // send functional value of type t
      | <t> -> <A>                         // receive functional value of type t
      | <id>                               // type name (e.g. auction)
      | ( <A> )
    
<t> ::= int           // integer
      | bool          // boolean
      | address       // built-in address type (internally a string)
      | <t> list{q}   // list of type t with potential q
      | <t> -> <t>    // arrow type
      | ( t )

<q> ::= *           // unknown potential
      | n           // positive integer
```

#### Process Definitions
New processes are defined using the syntax `proc <mode> f : (x1 : t1), (x2 : t2), ..., (xn : tn), ($c1 : A1), ... ($cm : Am) |{q}- ($c : A) = M` where the process name is `f`, its context is a sequence of functional arguments `xi` with types `Ai` and channel arguments `$ci` (shared channels are denoted by `#ci`) of type `Ai`, the potential stored is `q`, and the offered channel is `x` of type `A`. The definition is denoted by the expression `P`. An empty context is described using `.`. The `mode` can be either `asset`, `contract` or `transaction` depending on the role of the process.

Formally, the context is denoted with
```
<context> ::= .     // empty context
            | ctx   // non-empty context  

<ctx> ::= ($x : <A>), ctx
        | (#x : <A>), ctx
        | (x : <t>), ctx
```


#### Process Execution
A process `f` can be executed using the syntax `exec f`. Note that since Nomos only allows closed processes to execute, I require that `f` is defined with an empty context (this is checked by the type checker).

### Process Syntax
The channels in Nomos are prefixed with either a `#` or `$` character. Thus, shared channel `c` is denoted using `#c` while linear channel `c` is denoted using `$c`. This is required to visually separate functional variables from session-typed channels.

Formally, the syntax for processes is below.
```
M ::= { P }               // session-typed monad

<ch> ::= #<id> | $<id>   // channel
<ch-list> ::= <ch> | <ch> <ch-list>

<P> ::= <ch> <- f <- <ch-list> ; <P>    // spawn process f
      | <ch> <- f <- <ch-list>          // tail call f
      | <ch> <- <ch>                    // forwarding
      | send <ch1> <ch2> ; <P>          // send ch2 on ch1
      | <ch2> <- recv <ch1> ; <P>       // receive ch2 on ch1
      | <ch>.k ; <P>                    // send label k on ch
      | case <ch> ( <branches> )        // case analyze on label received on <ch>
      | close <ch>                      // close channel <ch>
      | wait <ch> ; <P>                 // wait for <ch> to close
      | work {q} ; <P>                  // work q units
      | pay <ch> {q} ; <P>              // pay q units on <ch>
      | get <ch> {q} ; <P>              // get q units on <ch>
      | <ch> <- acquire <ch> ; <P>      // acquire channel
      | <ch> <- accept <ch> ; <P>       // accept acquire request
      | <ch> <- release <ch> ; <P>      // release channel
      | <ch> <- detach <ch> ; <P>       // detach from release requ
      | send <ch> <e> ; <P>             // send functional expression e
      | <id> = recv <ch> ; <P>          // receive functional value
      | let <id> = <e> ; <P>            // define new variable
      | if <e> then <P> else <P>        // if expression

<e> ::= n | true | false                // primitive values
      | <id>                            // variable
      | if <e> then <e> else <e>        // if expression
      | let <id> = <e> in <e>           // let expression
      | <e> <op> <e>                    // boolean, comparison and arithmetic operations
```
