#!/usr/bin/env bash

##############################################################################
# This script searches the Cabal store for cached libclang-bindings builds.
# For each build found, it shows what libclang.so is linked.
#
# Output is hierarchical, as follows:
#
# (0 leading spaces) libclang-bindings build directory under ~/.cabal/store
#   (2 leading spaces) shared library in that build directory
#     (4 leading spaces) libclang.so link for that shared library
#
# When hs-bindgen reports a version mismatch, the best practice is to delete
# the whole GHC directory from the cabal store.  The drawback to this is that
# all dependencies then need to be rebuilt.
#
# Just removing the problematic libclang-bindings build might work, but it is
# not safe because it invalidates the package database for that version of
# GHC.
##############################################################################

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

STORE_DIR="$(cabal path --store-dir 2>/dev/null | tail -n 1 || true)"

if [ $# -gt 0 ] ; then
  echo "Usage: $0"
  echo
  awk '/^####/{if(f==0){f=1;c=0}c++}f{if(!/^####/)print};/^####/&&c==2{f=0}' \
      "${BASH_SOURCE[0]}" \
    | sed 's/^# \?//'
  echo
  if [ -z "${STORE_DIR}" ] ; then
    echo 'Cabal store directory: (unable to determine)'
    echo '  Is the cabal command in your PATH?'
  elif [ -d "${STORE_DIR}" ] ; then
    echo "Cabal store directory: ${STORE_DIR}"
  else
    echo "Cabal store directory: ${STORE_DIR} (not found)"
  fi
  exit 2
fi

if [ -z "${STORE_DIR}" ] ; then
  echo 'error: unable to determine Cabal store directory' >&2
  exit 1
fi

if [ ! -d "${STORE_DIR}" ] ; then
  echo "error: Cabal store directory ${STORE_DIR} not found" >&2
  exit 1
fi

while IFS=$'\n' read -r pkg ; do
  echo "${pkg//*.cabal\/store\//}"
  while IFS=$'\n' read -r lib ; do
    echo "  ${lib//${pkg}\//}"
    ldd "${lib}" | sed 's/^[[:space:]]*/    /' | grep '^    libclang.so'
  done < <(find "${pkg}" -type f -name '*.so')
done < <(find "${STORE_DIR}" -type d -name 'libclang-bindings-*')
