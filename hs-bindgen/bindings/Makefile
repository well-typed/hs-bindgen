all: base.yaml hs-bindgen-runtime.yaml
.PHONY: all

%.yaml: %.dhall
	dhall-to-yaml --generated-comment --omit-empty --file $< --output $@
