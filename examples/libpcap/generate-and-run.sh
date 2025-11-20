#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

(
    echo "# "
    echo "# Building libpcap"
    echo "# "

    rm -rfv libpcap*
    wget https://www.tcpdump.org/release/libpcap-1.10.5.tar.xz
    tar -xf libpcap-1.10.5.tar.xz
    mv libpcap-1.10.5 libpcap
    rm libpcap-1.10.5.tar.xz

    cd "libpcap"
    cmake .
    make
)

LIBPCAP_DIR=$(realpath libpcap)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

./generate.sh

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(cat <<-EOF
package libpcap
    extra-include-dirs:
        $LIBPCAP_DIR
    extra-lib-dirs:
        $LIBPCAP_DIR
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >> "$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    LD_LIBRARY_PATH="$LIBPCAP_DIR:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH

    cd "hs-project"
    cabal build libpcap-bin
    cabal run libpcap-bin
)
