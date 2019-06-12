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
$ git clone https://github.com/ankushdas/SmartContracts.git
$ cd SmartContracts/implementation
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
The executable generated exists at `_build/default/rast.exe`. To typecheck a file, simply run
```
$ ./_build/default/rast.exe <file-path>
```

To test whether your build is successful, I have created a test file in the repository. Run it using
```
$ ./_build/default/rast.exe test.rast
```
It should produce the output "file processing successful!".

### Troubleshooting
1. Sometimes, your `$ make` command may fail with the error "dune: command not found". In this case, try restarting your terminal and running `$ make` again.

2. Sometimes, the core library of ocaml is not correctly installed (generally, if you already have an old installation of core). In these cases, simply run `$ opam install core` and try running `$ make` again.
