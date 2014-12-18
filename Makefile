PKG=nanomsg
PREFIX=`opam config var prefix`
BUILDOPTS=native=true native-dynlink=true lwt=true

all: build

test: suite.native
	./suite.native -shards 1 -runner sequential

suite.native: build

build:
	ocaml pkg/build.ml $(BUILDOPTS)
	./suite.native -shards 1 -runner sequential

install: build
	opam-installer --prefix=$(PREFIX) $(PKG).install

uninstall: $(PKG).install
	opam-installer -u --prefix=$(PREFIX) $(PKG).install

PHONY: clean

clean:
	ocamlbuild -clean
