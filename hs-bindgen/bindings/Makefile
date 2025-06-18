help: # show this help
	@if command -v column >/dev/null 2>&1 \
		; then \
			grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
			| sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
			| column -t -s $$'\t' \
		; else \
			grep '^[a-zA-Z0-9_-]\+:[^#]*# ' $(MAKEFILE_LIST) \
			| sed 's/^\([^:]\+\):[^#]*# \(.*\)/make \1\t\2/' \
		; fi
.PHONY: help

check: # check that all binding specifications are up-to-date
check: check-base
check: check-runtime
.PHONY: check

check-base: # check that base binding specification is up-to-date
check-base: hs-bindgen-dev
	cabal exec hs-bindgen-dev binding-spec base 2>/dev/null | diff -s base.yaml -
.PHONY: check-base

check-runtime: # check that hs-bindgen-runtime binding specification is up-to-date
check-runtime: hs-bindgen-dev
	cabal exec hs-bindgen-dev binding-spec runtime 2>/dev/null | diff -s hs-bindgen-runtime.yaml -
.PHONY: check-runtime

hs-bindgen-dev: # build hs-bindgen-dev executable
hs-bindgen-dev:
	cabal build hs-bindgen:hs-bindgen-dev
.PHONY: hs-bindgen-dev

write: # write all binding specifications
write: write-base
write: write-runtime
.PHONY: write

write-base: # write base binding specification
write-base: hs-bindgen-dev
	cabal exec hs-bindgen-dev binding-spec base 2>/dev/null >base.yaml
.PHONY: write-base

write-runtime: # write hs-bindgen-runtime binding specification
write-runtime: hs-bindgen-dev
	cabal exec hs-bindgen-dev binding-spec runtime 2>/dev/null >hs-bindgen-runtime.yaml
.PHONY: write-runtime
