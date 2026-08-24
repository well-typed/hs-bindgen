#!/usr/bin/env bash

##############################################################################
# This script bumps a git-pinned dependency of cabal.project.base: it rewrites
# the `tag` of the corresponding `source-repository-package` stanza and then
# regenerates the affected Nix expressions under nix/generated/.
#
# Both steps are needed. The stanza is what Cabal reads; the generated
# expression is what Nix reads. They have to be committed together, and
# scripts/ci/check-nix-pins.sh fails if they disagree.
#
# Without --revision, the remote's HEAD is used. Regeneration needs Nix (it
# runs cabal2nix from nixpkgs) and network access.
##############################################################################

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=lib/git-pins.sh
source "${here}/lib/git-pins.sh"

cd "${here}/.."

project=cabal.project.base

usage() {
  echo "Usage: $0 [OPTION ...] PACKAGE"
  echo
  awk '/^####/{if(f==0){f=1;c=0}c++}f{if(!/^####/)print};/^####/&&c==2{f=0}' \
      "${BASH_SOURCE[0]}" \
    | sed 's/^# \?//'
  echo
  echo 'Options:'
  echo '  -r, --revision REV    bump to REV instead of the remote HEAD'
  echo '  -h, --help            show this help text'
  echo
  echo "Packages pinned in ${project}:"
  git_pins "${project}" | cut -f 1 | sed 's/^/  /'
}

usage_error() {
  usage >&2
  exit 2
}

error() {
  echo "error: $*" >&2
  exit 1
}

PACKAGE=''
REV_NEW=''

while [ $# -gt 0 ] ; do
  case "${1}" in
    '-h' | '--help' )
      usage
      exit 0
      ;;
    '-r' | '--revision' )
      [ $# -ge 2 ] || usage_error
      REV_NEW="${2}"
      shift 2
      ;;
    -* )
      usage_error
      ;;
    * )
      [ -z "${PACKAGE}" ] || usage_error
      PACKAGE="${1}"
      shift
      ;;
  esac
done

[ -n "${PACKAGE}" ] || usage_error

# Locate the stanza. A stanza with several subdirs provides several packages,
# all sharing one `tag` line, so bumping any of them bumps all of them.
url=''
rev_old=''
tag_line=''
while IFS=$'\t' read -r name pin_url pin_rev _subdir pin_tag_line ; do
  if [ "${name}" == "${PACKAGE}" ] ; then
    url="${pin_url}"
    rev_old="${pin_rev}"
    tag_line="${pin_tag_line}"
  fi
done < <(git_pins "${project}")

if [ -z "${url}" ] ; then
  echo "error: ${PACKAGE} is not pinned in ${project}" >&2
  usage_error
fi
[ -n "${rev_old}" ] || error "the stanza for ${PACKAGE} has no tag"

echo "Package:          ${PACKAGE}"
echo "Repository:       ${url}"
echo "Old revision:     ${rev_old}"

if [ -z "${REV_NEW}" ] ; then
  REV_NEW="$(git ls-remote "${url}" HEAD | cut -f 1)"
  [ -n "${REV_NEW}" ] || error "unable to resolve HEAD of ${url}"
fi

echo "New revision:     ${REV_NEW}"

# All packages provided by this stanza; each has its own generated expression.
affected=()
while IFS=$'\t' read -r name _pin_url _pin_rev _subdir pin_tag_line ; do
  [ "${pin_tag_line}" != "${tag_line}" ] || affected+=("${name}")
done < <(git_pins "${project}")

echo "Regenerating:     ${affected[*]}"

BUMPED=0

if [ "${REV_NEW}" == "${rev_old}" ] ; then
  echo "Already at ${REV_NEW}; regenerating anyway to resynchronize Nix."
else
  BUMPED=1
  # Rewrite the revision on the stanza's `tag` line only, preserving alignment.
  tmp="$(mktemp)"
  trap 'rm -f "${tmp}"' EXIT INT QUIT TERM HUP
  awk -v line="${tag_line}" -v rev="${REV_NEW}" \
    'NR == line { sub(/[^[:space:]]+[[:space:]]*$/, rev) } { print }' \
    "${project}" >"${tmp}"
  cat "${tmp}" >"${project}"
  echo "Updated ${project}"
fi

if ! ./nix/generate.sh "${affected[@]}" ; then
  echo >&2
  echo "error: regeneration failed, so Nix is out of sync." >&2
  if [ ${BUMPED} -eq 1 ] ; then
    echo "       Either rerun" >&2
    echo "         ./nix/generate.sh ${affected[*]}" >&2
    echo "       or revert ${project}." >&2
  fi
  exit 1
fi

echo
echo "Done. Commit ${project} and nix/generated/ together."
