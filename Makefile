DUNE=dune

.PHONY: all build clean

all: build regression

editor:
	cd src; \
	ocamlc Arith.mli; \
	ocamlc Normalize.mli; \
	ocamlc Mark.mli; \
	ocamlc ErrorMsg.mli; \
	ocamlc Parsestate.mli; \
	ocamlc SafeIO.mli; \
	ocamlc Terminal.mli; \
	ocamlc TokStream.mli; \
	ocamlc Lex.mli; \
	ocamlc Ast.mli; \
	ocamlc Parse.mli; \
	ocamlc ErrorMsg.mli; \
	ocamlc Flags.mli; \
	ocamlc Cost.mli; \
	ocamlc Pprint.mli; \
	ocamlc TpError.mli; \
	ocamlc Typecheck.mli; \
	ocamlfind ocamlc -thread -linkpkg -package core Infer.mli; \
	ocamlc Elab.mli; \
	ocamlfind ocamlc -thread -linkpkg -package core Exec.mli; \
	ocamlfind ocamlc -thread -linkpkg -package core RastConfig.mli; \
	cd ..

build:
	@${DUNE} build src/rast.exe

regression:
	@${DUNE} build src/regression.exe

clean:
	@${DUNE} clean
