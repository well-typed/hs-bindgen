#!/usr/bin/env bash

##############################################################################
# This script finds symbolic links, which we should avoid to make development
# on Windows (slightly) less painful.
##############################################################################

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

usage() {
  echo "Usage: $0"
  echo
  awk '/^####/{if(f==0){f=1;c=0}c++}f{if(!/^####/)print};/^####/&&c==2{f=0}' \
      "${BASH_SOURCE[0]}" \
    | sed 's/^# \?//'
}

for arg in "$@" ; do
  case "${arg}" in
    '-h' | '--help' )
      usage
      exit 0
      ;;
    * )
      usage >&2
      exit 2
      ;;
  esac
done

echo "Checking for symbolic links..."
found="$(find . -type l)"
if [ -n "${found}" ] ; then
  echo "${found}"
  exit 1
fi
