# Convenience shell for building the vendored libgit2 from source.
#
# build-libgit2.sh already pulls these via `nix shell nixpkgs#...`, so this file
# is only for people who prefer `nix-shell` and want cmake + a C toolchain on
# their PATH manually.
{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = [
    pkgs.cmake
    pkgs.gnumake
    pkgs.gcc
    pkgs.pkg-config
  ];
}
