
elm_src := $(shell find src/elm -type f -name '*.elm')
hs_src := $(shell find src/haskell -type f -name '*.hs')

elm_main := src/elm/Main.elm

all: build/dndgrid.js build/dndgrid
dev: all
	spk dev
run: all
	./start.sh
pack: dndgrid.spk

build/.mkdir:
	[ -d build ] || mkdir build
	touch $@
build/dndgrid.js: $(elm_src) elm.json build/.mkdir
	elm make --debug $(elm_main) --output $@
build/dndgrid: $(hs_src) build/.mkdir
	cabal v2-build
	find dist-newstyle \
		-type f \
		-executable \
		-name dndgrid \
		-exec cp \{} $@ \;
	strip $@
dndgrid.spk: all sandstorm-files.list sandstorm-pkgdef.capnp
	spk pack

.PHONY: all run dev pack
