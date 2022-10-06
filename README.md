# POPL 2023 Artifact Evaluation

## Paper Abstract
Session types guarantee that message-passing processes adhere to predefined communication protocols. Prior work on session types has focused on deterministic languages but many message-passing systems, such as Markov chains and randomized distributed algorithms, are probabilistic. To implement and analyze such systems, this article develops the meta theory of probabilistic session types with an application focus on automatic expected resource analysis. Probabilistic session types describe probability distributions over messages and are a conservative extension of intuitionistic (binary) session types. To send on a probabilistic channel, processes have to utilize internal randomness from a probabilistic branching or external randomness from receiving on a probabilistic channel. The analysis for expected resource bounds is smoothly integrated with the type system and is a variant of automatic amortized resource analysis. Type inference relies on linear constraint solving to automatically derive symbolic bounds for various cost metrics. The technical contributions include the meta theory that is based on a novel nested multiverse semantics and a type-reconstruction algorithm that allows flexible mixing of different sources of randomness without burdening the programmer with complex type annotations. The type system has been implemented in the language PRast with linear-time type checking. Experiments demonstrate that PRast is applicable in different domains such as cost analysis of randomized distributed algorithms, analysis of Markov chains, probabilistic analysis of amortized data structures and digital contracts. PRast is also shown to be scalable by (i) implementing two broadcast and a bounded retransmission protocol where messages are dropped with a fixed probability, and (ii) verifying the limiting distribution of a large Markov chain (64 states and 420 transitions).

## Paper Link
You can download the paper from [here](https://sandbox.zenodo.org/record/1111473/files/popl23-paper590.pdf?download=1).

# Artifact Evaluation
We would like to thank the reviewers for reviewing the artifact for the POPL 2023 submission 590 titled "Probabilistic Resource-Aware Session Types".
The `prob` branch of the `Nomos` repository contains the artifact for this submission.

## VM Link
You can download the VM image from [here](https://sandbox.zenodo.org/record/1111473/files/ubuntu-popl23-submission-590.ova?download=1).

### VM Login

VM Username: popl23-aec

VM Password: popl23-aec

## VM Instructions
The VM contains an image of Ubuntu 22.04.1 LTS with the PRast implementation already downloaded, installed, and compiled. The implementation requires a specific version of OCaml, so you may sometimes be prompted by the terminal to type `$ eval $(opam env)`.

Open a terminal and navigate to the PRast implementation and compile (if files have changed).
```
$ cd Documents/Nomos/nomos
$ git pull
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
To execute, simply run `$ ./run_all.sh`. The script is designed to redirect the execution output to a log file with the same name as the test case. The execution output of each test case can be viewed in the `Nomos/nomos/prob-tests` folder.

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

## Further Instructions
Instructions to prepare the VM image are [here](https://github.com/ankushdas/Nomos/blob/prob/BUILD.md).

