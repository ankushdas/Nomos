.PHONY: all clean test

MENHIR          := menhir

MENHIRFLAGS     := --infer --explain

OCAMLBUILD      := ocamlbuild -use-ocamlfind -tag thread -quiet -pkg core -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := main

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean
