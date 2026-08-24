#!/usr/bin/env bash

##############################################################################
# This script checks that the generated Nix expressions under nix/generated/
# agree with the git pins in cabal.project.base.
#
# Cabal and Nix read different files: Cabal takes a git dependency from the
# `source-repository-package` stanza in cabal.project.base, while Nix takes it
# from nix/generated/<package>.nix. Regenerating the latter is a manual step
# (nix/generate.sh), so the two can drift apart, and no version bound catches
# it: a git revision usually keeps the version number of the release it
# follows, so both sides claim the same version while shipping different code.
#
# The check is text-only, needing neither Nix nor network access. It does not
# verify the sha256, only the url and rev.
##############################################################################

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=../lib/git-pins.sh
source "${here}/../lib/git-pins.sh"

cd "${here}/../.."

project=cabal.project.base
generated=nix/generated

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

echo "Checking that ${generated} agrees with the git pins in ${project}..."

ERROR=0

fail() {
  echo "  ${1}" >&2
  ERROR=1
}

# Every git pin must be mirrored by a matching fetchgit expression.
pinned=()
while IFS=$'\t' read -r name url rev _subdir _tagline ; do
  [ -n "${name}" ] || continue
  pinned+=("${name}")
  expr="${generated}/${name}.nix"
  echo "${name}: pinned at ${rev}"
  if [ ! -f "${expr}" ] ; then
    fail "missing ${expr}; run nix/generate.sh ${name}"
    continue
  fi
  if ! grep -qF 'src = fetchgit' "${expr}" ; then
    fail "${expr} is not pinned to git; run nix/generate.sh ${name}"
  elif ! grep -qF "url = \"${url}\";" "${expr}" ; then
    fail "${expr} does not fetch from ${url}; run nix/generate.sh ${name}"
  elif ! grep -qF "rev = \"${rev}\";" "${expr}" ; then
    fail "${expr} is not at ${rev}; run nix/generate.sh ${name}"
  fi
done < <(git_pins "${project}")

# Conversely, no expression may be pinned to git without a stanza to match: a
# stanza removed from ${project} leaves the package on Hackage, so its
# expression has to go back to a Hackage tarball.
while IFS= read -r expr ; do
  name="$(basename "${expr}" .nix)"
  if [ ${#pinned[@]} -eq 0 ] \
    || ! printf '%s\n' "${pinned[@]}" | grep -qxF "${name}" ; then
    fail "${expr} fetches from git, but ${project} has no stanza for ${name}"
  fi
done < <(grep -l 'src = fetchgit' "${generated}"/*.nix || true)

if [ ${ERROR} -eq 1 ] ; then
  exit 1
fi
