#!/bin/sh

export LC_ALL=C.UTF-8

# POSIX compliant method for 'pipefail':
warn=$(mktemp)

# Check cabal-fmt version
cabal_fmt_expect_version="0.1.12"
if [ "${cabal_fmt}" = "" ]; then
    cabal_fmt=$(which "cabal-fmt")
    if [ "${cabal_fmt}" = "" ]; then
        echo "Requires cabal-fmt ${cabal_fmt_expect_version}; no version found"
        exit 1
    fi
fi
cabal_fmt_actual_version="$($cabal_fmt --version | head -n 1 | cut -d' ' -f2)"
if [ ! "${cabal_fmt_actual_version}" = "${cabal_fmt_expect_version}" ]; then
    echo "Expected cabal-fmt ${cabal_fmt_expect_version}; version ${cabal_fmt_actual_version} found"
    echo > "$warn"
fi

# Format Cabal files with cabal-fmt
echo "Formatting Cabal source files with cabal-fmt version ${cabal_fmt_actual_version}"
# shellcheck disable=SC2016
if ! git ls-files --exclude-standard --no-deleted --deduplicate '*.cabal' | xargs -L 1 sh -c 'echo "$0" && cabal-fmt -c "$0" 2>/dev/null || (cabal-fmt -i "$0" && exit 1)'; then
    exit 1
fi

# Check whether any warning was issued; on CI, warnings are errors
if [ "${CI}" = "true" ] && [ -s "$warn" ]; then
    rm "$warn"
    exit 1
else
    rm "$warn"
    exit 0
fi