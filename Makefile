default:
	ocp-build

install:
	ocp-build install

uninstall:
	ocp-build uninstall

oasis_gtfo:
	rm -f setup.ml
	rm -f setbup.data
	rm -f -rf _build
	rm -f myocamlbuild.ml
	rm -f tags
	rm -f lib/*.mllib
	rm -f lib/*.clib

symlinks:
	./symlinks.sh

.PHONY: build test configure clean symlinks
