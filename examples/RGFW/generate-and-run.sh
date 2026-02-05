#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

(
    echo "# "
    echo "# Downloading RGFW"
    echo "# "

    if [ -d "$SCRIPT_DIR/RGFW" ]; then
      rm -r "$SCRIPT_DIR/RGFW"
    fi
    mkdir -p "$SCRIPT_DIR/RGFW"
    cd "$SCRIPT_DIR/RGFW"

    curl -L https://github.com/ColleagueRiley/RGFW/releases/download/1.8.1/RGFW-1.8.0_linux_amd64.tar.gz > "RGFW.tar.gz"
    tar -xf "RGFW.tar.gz" -C "." --strip-components=1
)

RGFW_DIR=$(realpath RGFW)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I "$RGFW_DIR/include" \
    --parse-from-main-header-dirs \
    --create-output-dirs \
    --overwrite-files \
    --hs-output-dir "hs-project/src" \
    --module Generated.RGFW \
    --define-macro=RGFW_OPENGL \
    --define-macro=RGFW_EXPORT \
    --define-macro=RGFW_IMPLEMENTATION \
    --single-file \
    --safe "_safe" \
    --unsafe "_unsafe" \
    --pointer "_pointer" \
    "RGFW.h"

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(
    cat <<-EOF
package rgfw
    extra-include-dirs:
        $RGFW_DIR/include
    extra-lib-dirs:
        $RGFW_DIR/lib
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >>"$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    LD_LIBRARY_PATH="$BOTAN_DIR:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH

    cd "hs-project"
    cabal build
    cabal run rgfw
)
