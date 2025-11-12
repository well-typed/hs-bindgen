{ pkgs ? import <nixpkgs> {} }:

let
  # Get rpm-sequoia from unstable
  unstable = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Build tools
    cmake
    pkg-config
    scdoc

    # RPM dependencies
    popt
    libarchive
    sqlite
    zstd
    elfutils
    xz
    lua
    zlib
    bzip2
    readline
    file
    libcap
    audit
    libselinux
    dbus

    # From unstable
    unstable.rpm-sequoia
  ];
}
