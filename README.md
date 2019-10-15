# Nomos
## Resource-Aware Session Types for Digital Contracts
This repository will contain an implementation of Nomos, a programming language for smart contracts based on resource-aware session types.

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
$ opam pin add -y rast .    # only the first time you build
$ opam upgrade              # after packages upgrade
```

### Building
Simply run the make command.
```
$ make
```

### Executing
The executable generated exists at `_build/default/src/rast.exe`. To typecheck a file, simply run
```
$ ./_build/default/src/rast.exe <file-path>
```

To test whether your build is successful, I have created a test file in the repository. Run it using
```
$ ./_build/default/src/rast.exe test/success/testfile.rast
```
It should produce the output "% runtime successful!" at the end.

### Regression Testing
I have also created an executable at `_build/default/src/regression.exe`. This executes a set of files and generates a report by matching the expected and actual output.

To test if all files work as expected, run the command
```
$ ./_build/default/src/regression.exe test/*/*.rast
```

### Troubleshooting
1. Sometimes, your `$ make` command may fail with the error "dune: command not found". In this case, try restarting your terminal and running `$ make` again.

2. Sometimes, the core library of ocaml is not correctly installed (generally, if you already have an old installation of core). In these cases, simply run `$ opam install core` and try running `$ make` again.
