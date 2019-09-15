DUNE=dune

.PHONY: all build clean

all: build regression exec

editor:
	ocamlc Ast.mli

build:
	@${DUNE} build rast.exe

regression:
	@${DUNE} build regression.exe

exec:
	@${DUNE} build exec.exe

clean:
	@${DUNE} clean