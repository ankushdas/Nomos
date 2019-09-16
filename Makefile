DUNE=dune

.PHONY: all build clean

all: build regression

editor:
	ocamlc Ast.mli
	ocamlc Arith.mli
	ocamlc ErrorMsg.mli
	ocamlc Flags.mli

build:
	@${DUNE} build rast.exe

regression:
	@${DUNE} build regression.exe

clean:
	@${DUNE} clean