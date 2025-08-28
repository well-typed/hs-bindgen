#!/usr/bin/env bash

select_flags=(
    --select-by-header-path pcap.h
    # We need to exclude a lot of deprecated functions. These functions lead to
    # linker errors on my machine because the pre-compiled shared `libpcap.so`
    # library does not provide them anymore.
    --select-except-by-decl-name pcap_compile_nopcap
    --select-except-by-decl-name pcap_createsrcstr
    --select-except-by-decl-name pcap_findalldevs_ex
    --select-except-by-decl-name pcap_lookupdev
    --select-except-by-decl-name pcap_open
    --select-except-by-decl-name pcap_parsesrcstr
    --select-except-by-decl-name pcap_remoteact_accept
    --select-except-by-decl-name pcap_remoteact_accept_ex
    --select-except-by-decl-name pcap_remoteact_cleanup
    --select-except-by-decl-name pcap_remoteact_close
    --select-except-by-decl-name pcap_remoteact_list
    --select-except-by-decl-name pcap_setsampling
    # Select transitive dependencies.
    --enable-program-slicing
)
cabal run -- hs-bindgen-cli preprocess pcap.h \
    --gnu \
    --parse-all \
    "${select_flags[@]}" \
    --module Generated.Pcap \
    -o src/Generated/Pcap.hs
