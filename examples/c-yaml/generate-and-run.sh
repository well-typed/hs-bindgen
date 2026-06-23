#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

(
    echo "# "
    echo "# Building LibYAML"
    echo "# "

    rm -rfv yaml*
    wget http://pyyaml.org/download/libyaml/yaml-0.2.5.tar.gz
    tar -xf yaml-0.2.5.tar.gz
    mv yaml-0.2.5 yaml
    rm yaml-0.2.5.tar.gz

    cd yaml
    ./configure
    make
)

LIBYAML_INCLUDE_DIR="$(realpath yaml)/include"
LIBYAML_LIB_DIR="$(realpath yaml)/src/.libs"

echo "# "
echo "# Generating Haskell bindings"
echo "# "

./generate.sh

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(cat <<-EOF
package c-yaml
    extra-include-dirs:
        $LIBYAML_INCLUDE_DIR
    extra-lib-dirs:
        $LIBYAML_LIB_DIR
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >>"$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    LD_LIBRARY_PATH="$LIBYAML_LIB_DIR:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH

    cd "hs-project"
    cabal build c-yaml-bin
    cabal run c-yaml-bin
)
