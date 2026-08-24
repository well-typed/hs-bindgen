#!/usr/bin/env bash
#
# Regenerate the checked-in cabal2nix expressions under nix/generated/.
#
# Each expression is self-contained: `src` is a GitHub `fetchgit` for external
# source-repository-packages, a Hackage tarball for external packages on
# Hackage, and the in-repo directory for our own packages.
#
# The source of truth for external packages is cabal.project.base: a package
# with a `source-repository-package` stanza is fetched from git at the pinned
# `tag`; any other external package (see HACKAGE_PACKAGES) is fetched from
# Hackage at the version pinned below.
#
# To bump a git dependency, use scripts/update-git-dependency.sh, which edits
# the `tag` and reruns this script; the sha256 is recomputed automatically.
# scripts/ci/check-nix-pins.sh verifies that the two sides stay in sync.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=../scripts/lib/git-pins.sh
source "${here}/../scripts/lib/git-pins.sh"

cd "${here}/.."
out=nix/generated
mkdir -p "$out"

project=cabal.project.base

# External packages we build ourselves (not available from Nixpkgs). Those with
# a source-repository-package stanza in $project are fetched from git; the rest
# are fetched from Hackage at the version pinned here.
#
# The versions are pinned so that regeneration is reproducible: an unpinned
# `cabal://pkg` resolves to whatever is newest on Hackage, which is unrelated to
# the `index-state` in $project. Revisit them when bumping `index-state`.
HACKAGE_PACKAGES=(
  libclang-bindings=0.1.0.0
  doxygen-parser=0.1.1
  c-expr-dsl=0.1.0.1
  c-expr-runtime=0.1.0.0
  # libclang-bindings requires tasty <1.5.4, but Nixpkgs has 1.5.4; also
  # overridden (scoped to libclang-bindings only) in
  # nix/overlay/libclang-bindings.nix, keep both in sync.
  tasty=1.5.3
)

for entry in "${HACKAGE_PACKAGES[@]}" ; do
  case "$entry" in
    *=* ) ;;
    * )
      echo "error: HACKAGE_PACKAGES entry '$entry' pins no version" >&2
      exit 1
      ;;
  esac
done

# Our own packages, built from their in-repo directory.
LOCAL_PACKAGES=(hs-bindgen hs-bindgen-runtime hs-bindgen-test-runtime)

# Every name this script knows how to generate: the two hardcoded lists above,
# plus any git pin not already listed there.
known=("${HACKAGE_PACKAGES[@]%=*}" "${LOCAL_PACKAGES[@]}")
while IFS=$'\t' read -r name _url _rev _subpath _tagline ; do
  [ -n "$name" ] || continue
  printf '%s\n' "${known[@]}" | grep -qxF "$name" || known+=("$name")
done < <(git_pins "$project")

usage() {
  echo "Usage: $0 [PACKAGE ...]"
  echo
  echo 'Regenerate the cabal2nix expressions under nix/generated/. With no'
  echo 'arguments, regenerate all of them.'
  echo
  echo 'Known packages:'
  local name
  for name in "${known[@]}" ; do
    echo "  ${name}"
  done
}

for arg in "$@" ; do
  case "$arg" in
    '-h' | '--help' )
      usage
      exit 0
      ;;
    -* )
      usage >&2
      exit 2
      ;;
  esac
done

selected=("$@")

for requested in "${selected[@]:+${selected[@]}}" ; do
  if ! printf '%s\n' "${known[@]}" | grep -qxF "$requested" ; then
    echo "error: unknown package '${requested}'" >&2
    usage >&2
    exit 2
  fi
done

# Regenerate $1 only if it was named on the command line, or if none was.
wanted() {
  [ ${#selected[@]} -gt 0 ] || return 0
  local requested
  for requested in "${selected[@]}" ; do
    [ "$requested" != "$1" ] || return 0
  done
  return 1
}

# Run cabal2nix, writing $1 only if it succeeds. Redirecting straight into the
# target would truncate it on failure, leaving an empty expression behind for
# the next reader -- or committer -- to find.
c2n() {
  local target="$1" ; shift
  if nix run nixpkgs#cabal2nix -- "$@" >"${target}.tmp" ; then
    mv "${target}.tmp" "$target"
  else
    rm -f "${target}.tmp"
    echo "error: failed to generate $target" >&2
    return 1
  fi
}

# Git: generate from the pinned source-repository-package stanzas. `src` is a
# GitHub fetchgit, so the expressions are self-contained.
#
# cabal2nix prints alarming-looking noise here ("*** parsing cabal file:
# ...: Not a directory" and "error: failed to open archive"): it probes the
# plain and --unpack nix-prefetch-url fetchers first, both of which fail on a
# git repo URL, before falling back to nix-prefetch-git. These are expected and
# harmless as long as generation exits 0 and the output contains a fetchgit src.
declare -A from_git=()
while IFS=$'\t' read -r name url rev subpath _tagline ; do
  [ -n "$name" ] || continue
  from_git[$name]=1
  wanted "$name" || continue
  echo "generating $out/$name.nix (git $rev)"
  if [ -n "$subpath" ]; then
    c2n "$out/$name.nix" --revision "$rev" --subpath "$subpath" "$url"
  else
    c2n "$out/$name.nix" --revision "$rev" "$url"
  fi
done < <(git_pins "$project")

# Hackage: any external package without a git stanza.
for entry in "${HACKAGE_PACKAGES[@]}"; do
  p=${entry%=*}
  ver=${entry#*=}
  [ -z "${from_git[$p]:-}" ] || continue
  wanted "$p" || continue
  echo "generating $out/$p.nix (hackage, pinned $ver)"
  c2n "$out/$p.nix" "cabal://$p-$ver"
done

# Local: `src` is the in-repo directory, relative to the generated file.
for p in "${LOCAL_PACKAGES[@]}"; do
  wanted "$p" || continue
  echo "generating $out/$p.nix (local)"
  c2n "$out/$p.nix" --src-expression "../../$p" "./$p"
done
