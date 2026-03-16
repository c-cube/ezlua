
DUNE_OPTS?=
build:
	dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

test-autopromote:
	@dune runtest $(DUNE_OPTS) --auto-promote

doc:
	@dune build $(DUNE_OPTS) @doc

build-dev:
	dune build @install @runtest $(DUNE_OPTS) --workspace=dune-workspace.dev

format:
	@dune build $(DUNE_OPTS) @fmt --auto-promote

format-check:
	@dune build $(DUNE_OPTS) @fmt --display=quiet

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

.PHONY: test clean

VERSION=$(shell awk '/^version:/ {print $$2}' moonpool.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/**/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.mli)
