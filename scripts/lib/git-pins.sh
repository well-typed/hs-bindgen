#!/usr/bin/env bash

##############################################################################
# Shared parser for the `source-repository-package` stanzas of a Cabal project
# file. Source this file; it defines no top-level behavior.
#
# The stanzas are the single source of truth for git-pinned dependencies. Three
# consumers need to read them: nix/generate.sh (to regenerate the Nix mirror),
# scripts/ci/check-nix-pins.sh (to verify the mirror is in sync), and
# scripts/update-git-dependency.sh (to bump a pin).
##############################################################################

# Print one tab-separated record per package pinned by a
# `source-repository-package` stanza in the given Cabal project file, with
# columns:
#
#   name  url  rev  subdir  tag-line-number
#
# A stanza with N subdirs yields N records, all sharing one tag line; a stanza
# with no subdir yields a single record named after the repository (which is
# only correct when the package name matches the repository name).
git_pins() {
  awk '
    function flush(  n, a, i, name) {
      if (url != "") {
        if (subdir == "") {
          n = split(url, a, "/")
          name = a[n]
          sub(/\.git$/, "", name)
          printf "%s\t%s\t%s\t\t%d\n", name, url, rev, tagline
        } else {
          n = split(subdir, a, " ")
          for (i = 1; i <= n; i++)
            if (a[i] != "")
              printf "%s\t%s\t%s\t%s\t%d\n", a[i], url, rev, a[i], tagline
        }
      }
      url = ""; rev = ""; subdir = ""; tagline = 0
    }
    /^source-repository-package/ { flush(); in_stanza = 1; next }
    in_stanza && /^[[:space:]]*location:/ { url = $2; next }
    in_stanza && /^[[:space:]]*tag:/      { rev = $2; tagline = NR; next }
    in_stanza && /^[[:space:]]*subdir:/   { $1 = ""; subdir = $0; next }
    in_stanza && /^[^[:space:]]/          { flush(); in_stanza = 0 }
    END { flush() }
  ' "$1"
}
