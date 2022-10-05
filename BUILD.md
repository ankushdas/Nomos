# Preparing the VM
To prepare the VM image, we downloaded the Ubuntu 22.04.1 LTS from [here](https://ubuntu.com/download/desktop/thank-you?version=22.04.1&architecture=amd64).
We imported the VM with VirtualBox and installed the following dependencies.

## Configuring Dependencies
We need the following packages: `git`, `make`, `pkg-config`, and `opam`. We follow the commands below:
```
$ sudo add-apt-repository ppa:avsm/ppa
$ sudo apt update
$ sudo apt install git
$ sudo apt install make
$ sudo apt install pkg-config
$ sudo apt install opam
$ opam init
$ eval $(opam env)
```

To install the correct version of OCaml (4.10.0), use the following commands next:
```
$ opam switch create 4.10.0
$ eval $(opam env)
```

## Setting up Nomos repository
To set up the Nomos repository, we use the commands and switch to the PRast branch:
```
$ cd Documents
$ git clone https://github.com/ankushdas/Nomos.git
$ cd Nomos/nomos
$ git checkout prob
```

### Installing OCaml Libraries
Clone the repository and obtain the source code, and install the necessary libraries to build.
```
$ opam update
$ opam pin add -y nomos .    # only the first time you build
$ opam upgrade               # after packages upgrade
```

## Setting up Coin-OR LP Installation
Instructions for setting up the Coin-OR LP solver can be found [here](https://github.com/coin-or/Clp). We build it from source using the instructions below:
```
$ cd clp
$ wget https://raw.githubusercontent.com/coin-or/coinbrew/master/coinbrew
$ chmod u+x coinbrew
$ ./coinbrew fetch Clp@master
$ ./coinbrew build Clp
```

For PRast to find the CLP solver, we issue the following command:
```
$ export CLP_PATH=/home/popl23-aec/Documents/Nomos/nomos/clp/dist
```

## Building
Simply run the make command at the `Nomos/nomos` directoryeiifccuhfbbjeenjrvneurtthjeibcckdhnhurvegfvf
.
```
$ make
```

## Troubleshooting
1. Sometimes, your `$ make` command may fail with the error "dune: command not found". In this case, try restarting your terminal and running `$ make` again. In some case, you may need to install it using `opam install dune`.

2. Sometimes, the core library of ocaml is not correctly installed (generally, if you already have an old installation of core). In these cases, simply run `$ opam install core` and try running `$ make` again.

