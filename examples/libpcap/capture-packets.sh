#!/usr/bin/env bash

echo "# "
echo "This script should only be used locally (never on CI). It showcases"
echo "$(hs-bindgen) by capturing live packets, which requires elevated privileges."
echo "# "

echo
echo "# "
echo "Building the project"
echo "# "

LIBPCAP_DIR=$(realpath libpcap)
LD_LIBRARY_PATH="$LIBPCAP_DIR:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

cd "hs-project" || exit 1
cabal build libpcap-bin

echo
echo "# "
echo "Running the project with elevated privileges (packet capture)"
echo "# "

BIN=$(cabal list-bin libpcap-bin)

# We need to set the environment variable also in the superuser environment.
sudo env LD_LIBRARY_PATH="$LD_LIBRARY_PATH" "$BIN"
