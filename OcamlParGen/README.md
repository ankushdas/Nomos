# NomosGrammar
This folder contains files for the lexing and parsing rules for a subset of Ocaml as a part of the Nomos project. This grammar is inputted to the Menhir parser generator (http://gallium.inria.fr/~fpottier/menhir/) which will output a parser in Ocaml. The AST generated will be traversed and outputted on `stdout`.

### Prerequisites

### Installing Menhir
```
$ opam install menhir          
```

## Building the Parser
To build the parser, run `make` from the current directory.


To run the parser, run `./main.native`. Then type your input as a single line and press Enter followed by Ctrl-D to indicate the end of your input.
The expressions currently supported by the parser are:
- booleans
- integers
- `if-then-else` statements
- `let-in` statements
- `match-with` expressions
- the following binary operations: `+, -, *, /`
- list declaration separated by commas (`[5,6,7]`) or as `x::xs` (`5::[6,7]`)
- function application, under the constraint that it be declared as `app function_name arg_list` where `arg_list` consists of the arguments separated by spaces.
For example, to apply a function `f` to arguments `4` and `5`, type `app app f 4 5`
- binding variables and functions under the constraint that a function binding be declared as follows
``let x = fun n -> n+1 in ``
- following Ocaml convention, negative integers are represented as `(-4)` (surrounded by parentheses)

## Troubleshooting
If running the `make` command gives the error
```Sys_error("../Nomos/OcamlParGen/main.native: No such file or directory").
Compilation unsuccessful after building 0 targets (0 cached) in 00:00:00.
```
please delete the `main.native` file and rerun the `make` command.
