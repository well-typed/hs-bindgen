#!/usr/bin/env bash

echo "# "
echo "# Configuring"
echo "# "

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
C_DIR="$SCRIPT_DIR/c"
HS_DIR="$SCRIPT_DIR/hs"

is_linux() {
  local kernel
  kernel="$(uname -s)"
  if [[ "$kernel" == "Linux" ]]; then
    true
  else
    false
  fi
}

is_darwin() {
  local kernel
  kernel="$(uname -s)"
  if [[ "$kernel" == "Darwin" ]]; then
    true
  else
    false
  fi
}

is_unix() {
  local kernel
  kernel="$(uname -s)"
  if is_linux || is_darwin; then
    true
  else
    false
  fi
}

is_windows() {
  local kernel
  kernel="$(uname -s)"
  if [[ "$kernel" == MINGW32* ||
        "$kernel" == MINGW64* ||
        "$kernel" == MSYS* ]]; then
    true
  else
    false
  fi
}



### PATH ###

# Add manual library directory to PATH so that the DLLs are found.
set_path_windows() {
  local C_DIR_WINDOWS
  C_DIR_WINDOWS=$(cygpath -aw "$C_DIR")
  export PATH="$C_DIR_WINDOWS:${PATH:-}"
}

if is_windows; then
  echo "Setting PATH"
  set_path_windows
fi



### BINDGEN_EXTRA_CLANG_ARGS ###

# There's a quirk with Apple and Windows assembler and LLVM IR that do not
# accept Unicode characters. There's SUPPORTS_UNICODE flag that allows Unicode
# characters and we only enable that for non-MacOS and non-LLVM backend
# compilation
#
# Check inside manual_examples.{c,h} for where this macro flag is used.
#
if is_linux && [[ "${LLVM_BACKEND}" != "1" ]]; then
    echo "Setting SUPPORTS_UNICODE in BINDGEN_EXTRA_CLANG_ARGS"
    export BINDGEN_EXTRA_CLANG_ARGS="-DSUPPORTS_UNICODE ${BINDGEN_EXTRA_CLANG_ARGS:-}"
else
    echo "Not setting SUPPORTS_UNICODE (not Linux or LLVM backend enabled)"
fi



### LD_LIBRARY_PATH ###

echo "Setting LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$SCRIPT_DIR/c:${LD_LIBRARY_PATH:-}"
if is_darwin; then
    echo "Setting DYLD_LIBRARY_PATH"
    export DYLD_LIBRARY_PATH="$SCRIPT_DIR/c:${DYLD_LIBRARY_PATH:-}"
fi



### cabal.project.local ###

generate_cabal_project_local_windows () {
  local C_DIR_WINDOWS
  C_DIR_WINDOWS=$(cygpath -aw "$C_DIR")

  tee "$HS_DIR/cabal.project.local" <<EOF
package manual
  extra-include-dirs:
    $C_DIR_WINDOWS
  extra-lib-dirs:
    $C_DIR_WINDOWS

package hs-game
  extra-include-dirs:
    $C_DIR_WINDOWS
  extra-lib-dirs:
    $C_DIR_WINDOWS

package hs-vector
  extra-include-dirs:
    $C_DIR_WINDOWS
  extra-lib-dirs:
    $C_DIR_WINDOWS
EOF
}

generate_cabal_project_local_unix () {
  local SUPPORTS_UNICODE_STANZA
  SUPPORTS_UNICODE_STANZA=""
  if [[ "$(uname -s)" == "Linux" && "${LLVM_BACKEND}" != "1" ]]; then
      SUPPORTS_UNICODE_STANZA="package manual
  ghc-options:
    -optc-DSUPPORTS_UNICODE
    -DSUPPORTS_UNICODE

"
  fi

  tee "$HS_DIR/cabal.project.local" <<EOF
${SUPPORTS_UNICODE_STANZA}package manual
  extra-include-dirs:
      $SCRIPT_DIR/c
  extra-lib-dirs:
      $SCRIPT_DIR/c

package hs-game
  extra-include-dirs:
      $SCRIPT_DIR/c
  extra-lib-dirs:
      $SCRIPT_DIR/c

package hs-vector
  extra-include-dirs:
      $SCRIPT_DIR/c
  extra-lib-dirs:
      $SCRIPT_DIR/c
EOF
}

if [[ -f "$HS_DIR/cabal.project.local" ]]; then
    echo "Using existing cabal.project.local"
    cat "$HS_DIR/cabal.project.local"
else
    echo "Generating cabal.project.local"

    if is_unix; then
      generate_cabal_project_local_unix
    else
      if is_windows; then
        generate_cabal_project_local_windows
      else
        echo "Can not configure unsupported kernel: $(uname -s)"
        exit 1
      fi
    fi
fi
