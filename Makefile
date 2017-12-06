.PHONY: all build clean test

all: build

build:
	jbuilder build

clean:
	rm -rf _build *.install

format:
	ocamlformat --inplace cssinliner.ml
	ocamlformat --inplace test/test.ml
	ocamlformat --inplace inliner/inliner.ml

test:
	jbuilder runtest
