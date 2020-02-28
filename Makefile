DUNE=dune

.PHONY: all rast nomos clean

all: nomos

rast-editor:
	cd rast-src; \
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

nomos-editor:
	cd nomos-lib; \
	ocamlc Arith.mli; \
	ocamlc Normalize.mli; \
	ocamlc Mark.mli; \
	ocamlc ErrorMsg.mli; \
	ocamlc SafeIO.mli; \
	ocamlc Ast.mli; \
	ocamlc ErrorMsg.mli; \
	ocamlc NomosFlags.mli; \
	ocamlc Cost.mli; \
	ocamlc Pprint.mli; \
	ocamlc TpError.mli; \
	ocamlc Typecheck.mli; \
	ocamlfind ocamlc -thread -linkpkg -package core Infer.mli; \
	ocamlc Elab.mli; \
	ocamlfind ocamlc -thread -linkpkg -package core Exec.mli; \
	ocamlfind ocamlc -thread -linkpkg -package core NomosConfig.mli; \
	cd ..


rast:
	@${DUNE} build rast-src/rast.exe

regression:
	@${DUNE} build rast-src/regression.exe

nomos:
	@${DUNE} build nomos-bin/nomos.exe

clean:
	@${DUNE} clean
