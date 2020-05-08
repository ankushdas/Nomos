# Nomos
## Resource-Aware Session Types for Digital Contracts
This repository contains an implementation of Nomos, a programming language for smart contracts based on resource-aware session types.

## Installation

### Prerequisites

#### Opam
The instructions for installing opam are available on the [opam installation page](https://opam.ocaml.org/doc/Install.html). Please install the latest version of opam.

##### Configuring Opam
To configure opam for the first time and install the latest version of OCaml, please run the following (skip if you have already configured opam)
```
$ opam init
```

### Installing Dependencies
Clone the repository and obtain the source code, and install the necessary libraries to build.
```
$ git clone https://github.com/ankushdas/Nomos.git
$ cd Nomos
$ opam update
$ opam pin add -y nomos-rast .    # only the first time you build
$ opam upgrade                    # after packages upgrade
```
The next step is installing the [Coin-Or LP solver](https://projects.coin-or.org/Clp). Use the instructions below.
```
$ cd clp
$ svn co https://projects.coin-or.org/svn/Clp/stable/1.16 coin-Clp
$ cd coin-Clp
$ ./configure -C
$ make
$ make test
$ make install
$ cd ../..
```
The next step is setting up the environment variable `CLP_PATH` to the directory where the Coin-Or LP solver is installed. Use the command below.
```
$ export CLP_PATH=<absolute-path-to-'coin-Clp'-folder>
```

### Building
Simply run the make command at the top-level.
```
$ make
```

### Executing
The make command creates an executable for nomos at `_build/default/nomos-bin/nomos.exe`.
To typecheck a file with nomos, run
```
$ _build/default/nomos-bin/nomos.exe -tc <file-path>
```
The `-tc` flag tells Nomos to only typecheck the target file. It ignores any `exec` statements.

You can also omit `-tc` to run the `exec` statements.
For example, the `wallet-demo.nom` file has an example transactions as well as the
necessary support code. You typecheck and run this with
```
$ ./_build/default/nomos-bin/nomos.exe nomos-tests/wallet-demo.nom
```
It should produce the output "% runtime successful!" at the end.

A configuration is a description of the state of the all contracts we have running at a specific time. When we run a
transaction, the configuration is perturbed but will reach final state, which is where execution stops until the next
transaction is run.
You can run transactions on a specific configuration using the -i flag and can save the resulting configuration with
the -o flag. If you don't use the -i flag, your transaction will be run on an empty configuration.
You must specify the sender of the transaction using the -ts flag.

For example,
```
$ _build/default/nomos-bin/nomos.exe -ts someone -o s1.conf some_transaction.nom
$ _build/default/nomos-bin/nomos.exe -ts someone -i s1.conf -o s2.conf another_transaction.nom
```

### Top Level
It's also possible to parse, typecheck, and interpret Nomos files and transactions from the OCaml interpreter.

First, load the interpreter using `dune utop`.

Now we can try out some transactions interactively:
```
utop [0]: load_and_exec "nomos-tests/test-wallet/t1.nom";;
....
exec main1
created a wallet with 1000 coins on channel #ch3[S]
- : unit = ()
utop [1]: load_and_exec "nomos-tests/test-wallet/t2.nom";;
...
exec main2
created a wallet with 100 coins on channel #ch7[S]
- : unit = ()
utop [3]: load_and_exec "nomos-tests/test-wallet/t3.nom";;
...
exec main3
transfer of 100 coins from #ch3[S] to #ch7[S] successful
- : unit = () 
utop [4]: save "final.conf";;
- : unit = ()
```
This saves the final configuration after running t1, t2, and t3 to final.conf.

For a complete listing of available commands at the top level, see `nomos-src/TopLevel.mli`.


### Troubleshooting
1. Sometimes, your `$ make` command may fail with the error "dune: command not found". In this case, try restarting your terminal and running `$ make` again.

2. Sometimes, the core library of ocaml is not correctly installed (generally, if you already have an old installation of core). In these cases, simply run `$ opam install core` and try running `$ make` again.

## Writing Nomos programs
Writing session-typed programs needs some guidance. First, I will introduce the basic declarations. There are three forms of declarations:

1. Type Definitions: New type names can be defined using the following syntax `type v = A` where type name `v` has definition `A`. As an example, the `auction` type is defined as follows:
```
type auction = /\ <{*}| +{running : &{bid : int -> money -o |{*}> \/ auction,
                                      cancel : |{*}> \/ auction},
                          ended : &{collect : int -> +{won : lot * |{*}> \/ auction,
                                                       lost : money * |{*}> \/ auction},
                                    cancel : |{*}> \/ auction}}
```
Here, `/\` and `\/` are used to denote up-shift and down-shift, `<{q}|` and `|{q}>` are used to receive and send potential resp., `->` and `^` are used to receive and send functional data, `-o` and `*` are used to receive and send channels, `+` and `&` denote internal and external choice, and `1` indicates termination. Note that `*` can be used in place of `q` to denote unknown potential, which is later inferred by the compiler.

2. Process Definitions: New processes are defined using the syntax `proc <mode> f : (x1 : A1), (x2 : A2), ..., (xn : An) |{q}- (x : A) = P` where the process name is `f`, its context is a sequence of arguments `xi` with types `Ai`, the potential stored is `q`, and the offered channel is `x` of type `A`. The definition is denoted by the expression `P`. An empty context is described using `.`. The `mode` can be either `asset`, `contract` or `transaction` depending on the role of the process.

3. Process Execution: A process `f` can be executed using the syntax `exec f`. Note that since Nomos only allows closed processes to execute, I require that `f` is defined with an empty context (this is checked by the type checker).

### Process Syntax
The channels in Nomos are prefixed with either a `#` or `$` character. Thus, shared channel `c` is denoted using `#c` while linear channel `c` is denoted using `$c`. This is required to visually separate functional variables from session-typed channels.
