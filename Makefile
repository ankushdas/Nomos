DUNE=dune

.PHONY: all build clean

all: build regression

build:
	@${DUNE} build rast.exe

regression:
	@${DUNE} build regression.exe

clean:
	@${DUNE} clean