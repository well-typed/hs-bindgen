#!/usr/bin/env magix
#!magix bash
#!packages hyperfine

echo "Building 'hs-bindgen-cli'"
cabal build hs-bindgen-cli
hs_bindgen_bin=$(cabal list-bin hs-bindgen-cli)
echo "Location of binary: ${hs_bindgen_bin}"
export hs_bindgen_bin

test_name="binding-specs/stdlib/instances.h"
echo "Name of test: ${test_name}"
export test_name

create_bindings() {
    tmp_dir=$(mktemp -d)
    echo "Temporary directory: ${tmp_dir}"

    ${hs_bindgen_bin} preprocess \
        --unique-id "$(basename "${tmp_dir}")" \
        -I ./hs-bindgen/examples/golden \
        -I ./hs-bindgen/musl-include/x86_64 \
        --module Example \
        --hs-output-dir "${tmp_dir}" \
        ${test_name} \
        "$@"
}
export -f create_bindings

echo
hyperfine 'create_bindings'
