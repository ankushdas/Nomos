DUNE=dune

.PHONY: all build clean

all: build regression

editor:
	ocamlc Arith.mli
	ocamlc Mark.mli
	ocamlc ErrorMsg.mli
	ocamlc Parsestate.mli
	ocamlc SafeIO.mli
	ocamlc Terminal.mli
	ocamlc TokStream.mli
	ocamlc Lex.mli
	ocamlc Ast.mli
	ocamlc Parse.mli
	ocamlc ErrorMsg.mli
	ocamlc Flags.mli
	ocamlc Cost.mli
	ocamlc Pprint.mli
	ocamlc TpError.mli
	ocamlc Typecheck.mli
	ocamlc Elab.mli
	ocamlfind ocamlc -thread -linkpkg -package core Exec.mli
	ocamlfind ocamlc -thread -linkpkg -package core RastConfig.mli

build:
	@${DUNE} build rast.exe

regression:
	@${DUNE} build regression.exe

clean:
	@${DUNE} clean