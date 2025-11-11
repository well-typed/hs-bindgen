#!/usr/bin/env bash

# Command line flags affecting module generation.
module_flags=(
    -I "./libpcap"
    # C has a global namespace. Providing a unique ID ensures the C wrappers
    # have unique names.
    --unique-id org.hs-bindgen.libpcap
    # Base output directory of generated bindings.
    --hs-output-dir hs-project/src
    # Create output directory if it does not exist.
    --create-output-dirs
    # Base module name. Submodules will have name `Generated.Pcap.Safe`, for
    # example.
    --module Generated.Pcap
)

# Command line flags affecting `libclang`.
libclang_flags=(
    # Enable GNU extensions. GNU extensions define types, such as `u_int`, which
    # `libpcap` requires.
    --gnu
)

# `hs-bindgen` parses the C header files using `libclang` and interprets/reifies
# the declarations into an `hs-bindgen`-internal abstract syntax tree. These
# flags affect the parsing/reifying step.
parse_flags=(
    # TODO: We panic without `--parse-all`, see
    # https://github.com/well-typed/hs-bindgen/issues/1155.
    --parse-all
)

# We only generate bindings for a sub-set of all parsed/reified declarations.
# These flags configure the selection step.
select_flags=(
    # `pcap.h` itself does not define any declarations, but only includes
    # `pcap/pcap.h`. Tell `hs-bindgen` to also select bindings from other
    # headers matching Pearl-compatible regular expression ".*pcap\.h.*".
    --select-by-header-path pcap.h
    # Select transitive dependencies.
    --enable-program-slicing
    --select-except-deprecated
    # We manually de-select some declarations. These declarations are only
    # available when `libpcap` is built with "remote capture" enabled, which is
    # not the case by default in Nixpkgs.
    --select-except-by-decl-name 'pcap_open'
    --select-except-by-decl-name 'pcap_createsrcstr'
    --select-except-by-decl-name 'pcap_parsesrcstr'
    --select-except-by-decl-name 'pcap_findalldevs_ex'
    --select-except-by-decl-name 'pcap_setsampling'
    --select-except-by-decl-name 'pcap_remoteact'
)

# Increase the verbosity of `hs-bindgen` to learn something about its internals.
# For example, activate Info-level log messages to see which declarations are
# selected/not selected, or which C macros we succeed or fail to parse.
debug_flags=(
    # # Run `hs-bindgen` with log level "Info".
    # -v3
    # # Run `hs-bindgen` with log level "Debug".
    # -v4
)

cabal run --project-dir=${PROJECT_ROOT} -- hs-bindgen-cli preprocess \
    "${module_flags[@]}" \
    "${libclang_flags[@]}" \
    "${parse_flags[@]}" \
    "${select_flags[@]}" \
    "${debug_flags[@]}" \
    pcap.h
