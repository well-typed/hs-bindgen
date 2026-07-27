#!/usr/bin/env bash

echo "# "
echo "Running the project"
echo "# "

LIBPCAP_DIR=$(realpath libpcap)
LD_LIBRARY_PATH="$LIBPCAP_DIR:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

cd "hs-project" || exit 1
cabal build libpcap-bin
cabal run libpcap-bin
