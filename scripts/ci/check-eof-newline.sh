#!/usr/bin/env bash

##############################################################################
# This script searches for text files that do not end with a newline
# character, and can optionally fix them.
##############################################################################

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

usage() {
  echo "Usage: $0 [-f]"
  echo
  awk '/^####/{if(f==0){f=1;c=0}c++}f{if(!/^####/)print};/^####/&&c==2{f=0}' \
      "${BASH_SOURCE[0]}" \
    | sed 's/^# \?//'
  echo
  echo 'Options:'
  echo '  -f, --fix    Fix found files by adding a newline'
}

MODE_FIX=0

for arg in "$@" ; do
  case "${arg}" in
    '-f' | '--fix' )
      MODE_FIX=1
      ;;
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

if [ ${MODE_FIX} -eq 0 ] ; then
  echo "Checking for text files with no EOF newline..."
else
  echo "Fixing text files with no EOF newline..."
fi

ERROR=0

while IFS=$'\n' read -r fp ; do
  case "${fp}" in
    *.svg )
      ;;
    * )
      if [ -n "$(tail -c 1 "${fp}")" ] ; then
        echo "${fp}"
        if [ ${MODE_FIX} -eq 0 ] ; then
          ERROR=1
        else
          echo >> "${fp}"
        fi
      fi
      ;;
  esac
done < <(git grep -I --name-only '')

if [ ${ERROR} -eq 1 ] ; then
  exit 1
fi
