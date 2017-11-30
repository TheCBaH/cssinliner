.PHONY: all build clean

all: build

build:
	jbuilder build --dev @install

clean:
	rm -rf _build *.install

