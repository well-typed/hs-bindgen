#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

(
    echo "# "
    echo "# Building libpcap"
    echo "# "

    cd "libpcap"
    cmake .
    make
)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

./generate.sh

echo "# "
echo "# Creating cabal.project.local"
echo "# "

cat >hs-project/cabal.project.local <<EOF
package hs-pcap
    extra-include-dirs:
        $SCRIPT_DIR/libpcap
    extra-lib-dirs:
        $SCRIPT_DIR/libpcap
EOF

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    LD_LIBRARY_PATH="$(realpath libpcap):$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH

    cd "hs-project"
    cabal build
    cabal run hs-pcap-bin
)
