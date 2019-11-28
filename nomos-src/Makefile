# .PHONY: all clean test

# MENHIR          := menhir

# MENHIRFLAGS     := --infer --explain --unused-tokens

# OCAMLBUILD      := ocamlbuild -use-ocamlfind -tag thread -quiet -pkg core -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

# MAIN            := main

# all:
# 	$(OCAMLBUILD) $(MAIN).native

# clean:
# 	rm -f *~ .*~
# 	$(OCAMLBUILD) -clean

DUNE=dune

.PHONY: all build clean

all: build

build:
	@${DUNE} build nomos.exe

clean:
	@${DUNE} clean

