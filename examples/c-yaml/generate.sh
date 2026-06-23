#!/usr/bin/env bash

# Resolve the repository root if not already exported by a parent script
# (generate-and-run.sh exports PROJECT_ROOT before sourcing/calling us).
if [ -z "${PROJECT_ROOT:-}" ]; then
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
    export PROJECT_ROOT
fi

LIBYAML_INCLUDE_DIR="${LIBYAML_INCLUDE_DIR:-./yaml/include}"

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I "$LIBYAML_INCLUDE_DIR" \
    --unique-id org.hs-bindgen.c-yaml \
    --hs-output-dir "hs-project/src" \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Yaml \
    --omit-field-prefixes \
    "yaml.h"
