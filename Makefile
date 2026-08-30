.POSIX:
.PHONY: all gen build test lint fmt fmt-check docs run clean ci

CABAL ?= cabal
DB    ?= ./state/upi.sqlite3
PORT  ?= 8080

all: gen build test

# package.yaml is authoritative. Every target that touches cabal depends on gen.
gen:
	hpack

build: gen
	$(CABAL) build all

test: gen
	$(CABAL) test all --test-show-details=direct

lint:
	hlint app src test

fmt:
	fourmolu --mode inplace $$(git ls-files '*.hs')

fmt-check:
	fourmolu --mode check $$(git ls-files '*.hs')

# Dump the generated OpenAPI 3 document without starting the server.
docs: gen
	$(CABAL) run -v0 upi-mock-engine -- openapi | jq .

run: gen
	mkdir -p $$(dirname $(DB))
	$(CABAL) run upi-mock-engine -- --port $(PORT) --database $(DB)

ci: fmt-check lint build test

clean:
	$(CABAL) clean
	rm -f upi-mock-engine.cabal
