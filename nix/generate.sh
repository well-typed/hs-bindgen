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
# Hackage. To bump a git dependency, edit its `tag` in cabal.project.base and
# rerun; the sha256 is recomputed automatically.
set -euo pipefail

cd "$(dirname "$0")/.."
out=nix/generated
mkdir -p "$out"

project=cabal.project.base

# External packages we build ourselves (not available from Nixpkgs). Those with
# a source-repository-package stanza in $project are fetched from git; the rest
# are fetched from Hackage.
HACKAGE_PACKAGES=(libclang-bindings doxygen-parser c-expr-dsl c-expr-runtime)

# Our own packages, built from their in-repo directory.
LOCAL_PACKAGES=(hs-bindgen hs-bindgen-runtime hs-bindgen-test-runtime)

c2n() { nix run nixpkgs#cabal2nix -- "$@"; }

# Emit one TSV line (name, url, rev, subpath) per package in each
# source-repository-package stanza of $project. A stanza with N subdirs yields N
# lines; a stanza with no subdir yields one line named after the repo.
parse_stanzas() {
  awk '
    /^source-repository-package/ { in_stanza=1; url=""; rev=""; subdir=""; next }
    in_stanza && /^[[:space:]]*location:/ { url=$2; next }
    in_stanza && /^[[:space:]]*tag:/      { rev=$2; next }
    in_stanza && /^[[:space:]]*subdir:/   { $1=""; subdir=$0; next }
    in_stanza && /^[^[:space:]]/ && !/^source-repository-package/ { flush(); in_stanza=0 }
    END { flush() }
    function flush(  n, a, i, name) {
      if (url == "") return
      if (subdir == "") {
        n = split(url, a, "/"); name = a[n]
        printf "%s\t%s\t%s\t\n", name, url, rev
      } else {
        n = split(subdir, a, " ")
        for (i = 1; i <= n; i++)
          if (a[i] != "") printf "%s\t%s\t%s\t%s\n", a[i], url, rev, a[i]
      }
    }
  ' "$project"
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
while IFS=$'\t' read -r name url rev subpath; do
  [ -n "$name" ] || continue
  from_git[$name]=1
  echo "generating $out/$name.nix (git $rev)"
  if [ -n "$subpath" ]; then
    c2n --revision "$rev" --subpath "$subpath" "$url" >"$out/$name.nix"
  else
    c2n --revision "$rev" "$url" >"$out/$name.nix"
  fi
done < <(parse_stanzas)

# Hackage: any external package without a git stanza.
for p in "${HACKAGE_PACKAGES[@]}"; do
  [ -z "${from_git[$p]:-}" ] || continue
  echo "generating $out/$p.nix (hackage)"
  c2n "cabal://$p" >"$out/$p.nix"
done

# Local: `src` is the in-repo directory, relative to the generated file.
for p in "${LOCAL_PACKAGES[@]}"; do
  echo "generating $out/$p.nix (local)"
  c2n --src-expression "../../$p" "./$p" >"$out/$p.nix"
done
