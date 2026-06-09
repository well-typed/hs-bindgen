#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT
cd "$SCRIPT_DIR"

# Download and build libpcap only if it is not already built. Delete the
# ./libpcap directory to force a fresh download and rebuild. (The previous
# version unconditionally deleted and re-downloaded, which fails offline and
# throws away an existing build.)
if [ -e libpcap/libpcap.so ]; then
    echo "# "
    echo "# libpcap already built in ./libpcap, skipping download"
    echo "# "
else
    (
        echo "# "
        echo "# Building libpcap"
        echo "# "

        rm -rfv libpcap libpcap-1.10.5 libpcap-1.10.5.tar.xz
        wget https://www.tcpdump.org/release/libpcap-1.10.5.tar.xz
        tar -xf libpcap-1.10.5.tar.xz
        mv libpcap-1.10.5 libpcap
        rm libpcap-1.10.5.tar.xz

        cd "libpcap"
        cmake .
        make
    )
fi

LIBPCAP_DIR=$(realpath libpcap)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

./generate.sh

echo "# "
echo "# Writing cabal.project.local"
echo "# "

# cabal.project.local is gitignored and machine-specific, so rewrite it from
# scratch each run rather than appending (which would accumulate stale paths).
cat > "$SCRIPT_DIR/hs-project/cabal.project.local" <<EOF
package libpcap
    extra-include-dirs:
        $LIBPCAP_DIR
    extra-lib-dirs:
        $LIBPCAP_DIR
EOF
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
