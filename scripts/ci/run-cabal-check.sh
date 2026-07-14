#!/bin/sh

# Check for cabal
cabal="$(which cabal)"
if [ "${cabal}" = "" ]; then
    echo "Requires cabal; no version found"
    exit 1
fi
cabal_actual_version="$(${cabal} --numeric-version | head -n 1)"

# We only run checks on packages that we intend to publish. There are a bunch of
# other cabal files (examples, manual, alternatives) that we can igore.
search_pattern="hs-bindgen*/*.cabal"

# Lint Cabal files with cabal
echo "Linting Cabal source files with cabal version ${cabal_actual_version}..."
# shellcheck disable=SC2016
if ! git ls-files --exclude-standard --no-deleted --deduplicate "$search_pattern" | xargs -L 1 sh -c 'echo "$0" && cd "$(dirname "$0")" && cabal check'; then
    exit 1
fi
