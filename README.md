# POPL 2023 Artifact Evaluation

## Paper Abstract
Session types guarantee that message-passing processes adhere to predefined communication protocols. Prior work on session types has focused on deterministic languages but many message-passing systems, such as Markov chains and randomized distributed algorithms, are probabilistic. To implement and analyze such systems, this article develops the meta theory of probabilistic session types with an application focus on automatic expected resource analysis. Probabilistic session types describe probability distributions over messages and are a conservative extension of intuitionistic (binary) session types. To send on a probabilistic channel, processes have to utilize internal randomness from a probabilistic branching or external randomness from receiving on a probabilistic channel. The analysis for expected resource bounds is smoothly integrated with the type system and is a variant of automatic amortized resource analysis. Type inference relies on linear constraint solving to automatically derive symbolic bounds for various cost metrics. The technical contributions include the meta theory that is based on a novel nested multiverse semantics and a type-reconstruction algorithm that allows flexible mixing of different sources of randomness without burdening the programmer with complex type annotations. The type system has been implemented in the language PRast with linear-time type checking. Experiments demonstrate that PRast is applicable in different domains such as cost analysis of randomized distributed algorithms, analysis of Markov chains, probabilistic analysis of amortized data structures and digital contracts. PRast is also shown to be scalable by (i) implementing two broadcast and a bounded retransmission protocol where messages are dropped with a fixed probability, and (ii) verifying the limiting distribution of a large Markov chain (64 states and 420 transitions).

## Paper Link
You can download the paper from [here]().

# Artifact Evaluation
We would like to thank the reviewers for reviewing the artifact for the POPL 2023 submission 590 titled "Probabilistic Resource-Aware Session Types".
The `prob` branch of the `Nomos` repository contains the artifact for this submission.

## VM Link
You can download the VM image from [here]().

### VM Login

VM Username: popl23-aec
VM Password: popl23-aec

## VM Instructions
The VM contains an image of Ubuntu 22.04.1 LTS with the PRast implementation already downloaded, installed, and compiled. The implementation requires a specific version of OCaml, so you may sometimes be prompted by the terminal to type `$ eval $(opam env)`.

Open a terminal and navigate to the PRast implementation and compile (if files have changed).
```
$ cd Documents/Nomos/nomos
$ make
```

The location for the executable is `/home/popl23-aec/Documents/Nomos/nomos/_build/default/nomos-bin/nomos.exe` (default location for binaries with our build system). We have already added this path into `PATH`. So, the implementation can be run by just issuing `$ nomos.exe`. All the test cases are implemented in the directory `Nomos/nomos/prob-tests`. For instance, to execute the `prob-tests/leader-election.nom`, we issue the command
```
$ nomos.exe -tc prob-tests/leader-election.nom
```

## Test Cases
The following test cases from the `prob-tests` folder represent the examples from Table 1 in the paper.

- leader: `leader-election.nom`
- bdd ret. (N = 4): `bounded-retransmission-c4b10.nom`
- bdd ret. (N = 5): `bounded-retransmission-c5b10.nom`
- bdd sym.: `bounded-retransmission-symbolic.nom`
- auth: `authenticated_broadcast.nom`
- bracha: `brach_broadcast.nom`
- din. crypto.: `prob_dining_crypto.nom`
- queue: `queue.nom` 
- 3 die: `3dice.nom`
- 6 die: `dice.nom`
- fair coin: `fair-coin.nom`
- exp. trials: `expected-trials.nom`
- rnd walk: `rnd-walk.nom`
- repair: `repair.nom`
- weather: `weather.nom`
- chessboard: `chess-king.nom`
- nat add: `nat-add.nom`
- nat double: `nat-double.nom`
- lottery: `lottery1.nom`
- slots: `slot-machine2.nom`

### Running A Test Case
To run a specific test case file, you need to run the command
```
$ nomos.exe -tc <test-file>
```
For each test case, you can confirm the lines of code (LOC), number of definitions (Defs), number of variables (Vars) and constraints (Cons) reported by the LP solver, and the bound value as reported in Table 1. The type checking (T (ms)) and inference time (I (ms)) are wall clock times and would be different in the VM, so would not exactly match Table 1.

In addition, to automatically charge a cost of 1 for every send operation, we provide a flag `-w send`. So, if you run a test case as follows:
```
$ nomos.exe -tc <test-file> -w send
```
the cost instrumentation engine will insert `work {1}` before each send operation. We use this cost model for some examples. Similarly, we have another cost model `-w flip` to charge a cost of 1 for every flip operation.

### Running All Test Cases
We have already created a script called `run_all.sh` in the `Nomos/nomos` directory which runs all the tests specified in the aforementioned list.
To execute, simply run `$ ./run_all.sh`.

#### Interpreting Reported Cost Bound
PRast cost bounds need to be interpreted by reading the potential annotations for types and processes. Below we explain how to determine the cost bound for each test case. For simplicity, the type checker re-prints the program with potential annotations filled in to easily verify the cost bound of each test case.

- leader: `leader-election.nom`: The process `n3k3` (for N = 3, K = 3) has a cost bound of 1.8 (printed within the turnstile).
- bdd ret. (N = 4): `bounded-retransmission-c4b10.nom`: The process `sender11` reports a bound of 4.9999 for 10 retries, i.e., approaches 5 for arbitrary retries.
- bdd ret. (N = 5): `bounded-retransmission-c5b10.nom`: The process `sender11` reports a bound of 6.2499 for 10 retries, i.e., approaches 6.25 for arbitrary retries.
- bdd sym.: `bounded-retransmission-symbolic.nom`: The type `potlist` stores a potential of 1.25 per element, meaning the total bound is 1.25N where N is the size of `potlist`, i.e., number of chunks.
- auth: `authenticated_broadcast.nom`: No bound reported.
- bracha: `brach_broadcast.nom`: No bound reported.
- din. crypto.: `prob_dining_crypto.nom`: Each process has a cost bound of 2 since 2 messages are exchanged. Bound obtained after running with the `-w send` flag.
- queue: `queue.nom`: The type `reqlist` stores a potential of 7 per element, meaning the total bound is 7n. Bound obtained after running with the `-w send` flag.
- 3 die: `3dice.nom`: The `p1` process stores 2.667 potential units. Bound obtained after running with the `-w flip` flag.
- 6 die: `dice.nom`: The `p1` process stores 3.667 potential units. Bound obtained after running with the `-w flip` flag.
- fair coin: `fair-coin.nom`: The `biased_to_fair` process stores 4.167 potential units. Bound obtained after running with the `-w flip` flag.
- exp. trials: `expected-trials.nom`: The `expected` process stores 2 potential units. Bound obtained after running with the `-w flip` flag.
- rnd walk: `rnd-walk.nom`: The `ulist` type stores 4 potential units per element, thus the total bound is 4n.
- repair: `repair.nom`: The `days` type stores 0.11 potential units per element, thus the total bound is 0.11n.
- weather: `weather.nom`: The `steady_state` process stores 2 potential units. Bound obtained after running with the `-w send` flag.
- chessboard: `chess-king.nom`: The `transition` process stores 5.847 potential units. Bound obtained after running with the `-w send` flag.
- nat add: `nat-add.nom`: The `pnat1` type stores 1 potential unit per element, thus the total bound is n. Bound obtained after running with the `-w send` flag.
- nat double: `nat-double.nom`: The `pnat3` type stores 2.4 potential units per element, and the `double` process stores 0.4 potential. Thus, the total bound is 0.4+2.4n. Bound obtained after running with the `-w send` flag.
- lottery: `lottery1.nom`: The `lottery` type returns 10 potential units in the `won` branch.
- slots: `slot-machine2.nom` The `slot` type returns 5 potential units in the `won` branch.

#### Caveat
A caveat is that for the following examples, we have made minor modifications and refactoring in the code, so the number of variables (Vars) and constraints (Cons) reported in Table 1 are slightly different than what is reported by the LP solver: `3dice.nom`, `dice.nom`, `fair-coin.nom`, `expected-trials.nom`, `rnd-walk.nom`, and `slot-machine2.nom`. We will update Table 1 to reflect the latest numbers reported by the LP solver.

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
