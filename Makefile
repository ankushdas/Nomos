DUNE=dune

.PHONY: all build clean

all: build

build:
	@${DUNE} build rast.exe

clean:
	@${DUNE} clean