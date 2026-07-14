#!/usr/bin/env bash
#
# Generate low-level libsodium bindings (one module per header, in topological
# order), wire up the cabal package, build, and run the demo programs.
#
# The header order is derived from the tool, not hand-curated: we ask
# `hs-bindgen-cli info include-graph` for the include DAG, then `tsort` it so
# that a header is always generated after every header it depends on. Each pass
# feeds the binding specs of all previously generated headers as
# `--external-binding-spec`, so cross-header types (the opaque state structs, the
# primitive-header constants, ...) resolve to the module that already defines them.
#
# Generation is resilient: a header that fails to generate is logged and skipped
# (what breaks at this scale is itself a finding), so the run continues.
#
set -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$( cd "$SCRIPT_DIR/../.." && pwd )"

# libsodium, pinned to a release tag so the example is reproducible. We fetch it
# on demand rather than as a git submodule: a submodule is cloned by cabal for
# every project that depends on hs-bindgen via source-repository-package, even
# though only hs-bindgen-runtime is needed.
LIBSODIUM_REPO="https://github.com/jedisct1/libsodium"
LIBSODIUM_TAG="1.0.22-RELEASE"

SRC="$SCRIPT_DIR/libsodium"
PREFIX="$SRC/build-prefix"
INCLUDE_DIR="$PREFIX/include"
LIB_DIR="$PREFIX/lib"
BINDING_SPEC_DIR="$SCRIPT_DIR/binding-specs"
HS_OUTPUT_DIR="$SCRIPT_DIR/hs-project/src"
CABAL_FILE="$SCRIPT_DIR/hs-project/libsodium.cabal"
GRAPH="$SCRIPT_DIR/include-graph.mmd"
GENLOG_DIR="$SCRIPT_DIR/gen-logs"

# Headers we deliberately do not generate. Populated as failures are triaged; a
# skip here is a finding recorded in FINDINGS.md, not a silent omission.
SKIP=""

# snake_case header -> CamelCase module suffix (crypto_secretbox.h -> CryptoSecretbox)
camel() { echo "${1%.h}" | sed -E 's/(^|_)([a-z])/\U\2/g'; }

# -----------------------------------------------------------------------------
# 0. Prerequisites: pinned source fetched, libsodium built and installed.
# -----------------------------------------------------------------------------
# -e (not -d): a prior submodule checkout leaves .git as a gitlink file, which is
# still a usable clone, so only clone when nothing is there.
if [ ! -e "$SRC/.git" ]; then
  # --filter=blob:none keeps the download small (blobs are fetched on demand)
  # while still allowing checkout of the pinned tag; a shallow clone could not
  # check out an arbitrary older tag.
  git clone --filter=blob:none "$LIBSODIUM_REPO" "$SRC" || exit 1
fi
git -C "$SRC" checkout --quiet "$LIBSODIUM_TAG" || exit 1

if ! ls "$LIB_DIR"/libsodium.so* >/dev/null 2>&1; then
  echo "==> libsodium not built yet; building it"
  "$SCRIPT_DIR/build-libsodium.sh" || exit 1
fi

mkdir -p "$BINDING_SPEC_DIR" "$HS_OUTPUT_DIR" "$GENLOG_DIR"

cd "$PROJECT_ROOT"
CLI="$(cabal list-bin hs-bindgen-cli)"
echo "==> using hs-bindgen-cli at $CLI"

# -----------------------------------------------------------------------------
# 1. Include graph -> topological order (dependencies first).
# -----------------------------------------------------------------------------
echo "==> Computing include graph"
rm -f "$GRAPH"   # info include-graph will not overwrite an existing output file
"$CLI" info include-graph -I "$INCLUDE_DIR" --show-paths -o "$GRAPH" sodium.h || exit 1

# Mermaid format (Data/DynGraph/Labelled.hs:dumpMermaid):
#   graph TD;
#     v<id>("<path>")          node
#     v<a>-->v<b>              edge: a includes b  =>  b is a dependency of a
#     v<a>-.->v<b>             (transient include)
# Keep only per-family headers under .../include/sodium/<name>.h (the umbrella
# sodium.h just aggregates them). For tsort we print "<dep> <dependent>".
awk '
  match($0, /^  v([0-9]+)\("(.*)"\)$/, m) {
    id=m[1]; path=m[2]
    if (path ~ /\/include\/sodium\/[^/]+\.h$/) {
      n=split(path, p, "/"); name[id]=p[n]; isg[id]=1
    }
    next
  }
  match($0, /^  v([0-9]+)-\.?->v([0-9]+)$/, e) {
    if (isg[e[1]] && isg[e[2]]) print name[e[2]], name[e[1]]
    next
  }
  END { for (i in name) print name[i] > "/dev/stderr" }
' "$GRAPH" >"$SCRIPT_DIR/.edges" 2>"$SCRIPT_DIR/.nodes"

SORTED="$(tsort "$SCRIPT_DIR/.edges" 2>"$SCRIPT_DIR/.tsort.err")"
if [ -s "$SCRIPT_DIR/.tsort.err" ]; then
  echo "   note: tsort reported include cycles (still produced an order):"
  sed 's/^/     /' "$SCRIPT_DIR/.tsort.err"
fi
# Append isolated headers (present in no sodium<->sodium edge); order among them is free.
ISOLATED="$(comm -23 <(sort -u "$SCRIPT_DIR/.nodes") <(printf '%s\n' "$SORTED" | sort -u))"
ORDER="$(printf '%s\n%s\n' "$SORTED" "$ISOLATED" | sed '/^$/d')"

echo "==> Topological order ($(printf '%s\n' "$ORDER" | grep -c .) headers):"
printf '%s\n' "$ORDER" | sed 's/^/     /'

# -----------------------------------------------------------------------------
# 2. Generate one module per header, accumulating binding specs.
# -----------------------------------------------------------------------------
echo "==> Generating bindings"
SPECS=()
OK_COUNT=0
FAILED=""
for header in $ORDER; do
  case " $SKIP " in *" $header "*) echo "  skip $header"; continue;; esac
  mod="Generated.$(camel "$header")"
  spec="$BINDING_SPEC_DIR/${header%.h}.yaml"

  ext=()
  for s in "${SPECS[@]}"; do ext+=(--external-binding-spec "$s"); done

  if "$CLI" preprocess \
       -I "$INCLUDE_DIR" \
       --unique-id org.libsodium \
       --hs-output-dir "$HS_OUTPUT_DIR" \
       --create-output-dirs --overwrite-files \
       --module "$mod" \
       --select-from-main-headers --enable-program-slicing \
       --select-except-deprecated \
       --gen-binding-spec "$spec" \
       "${ext[@]}" \
       "sodium/$header" >"$GENLOG_DIR/${header%.h}.log" 2>&1; then
    echo "  ok   $header -> $mod"
    [ -f "$spec" ] && SPECS+=("$spec")
    OK_COUNT=$((OK_COUNT+1))
  else
    echo "  FAIL $header  (see gen-logs/${header%.h}.log)"
    FAILED="$FAILED $header"
  fi
done

echo "==> Generated $OK_COUNT header modules."
[ -n "$FAILED" ] && echo "==> Failed headers:$FAILED"

# -----------------------------------------------------------------------------
# 3. Rewrite the generated-modules block of the cabal file from the file tree.
# -----------------------------------------------------------------------------
MODS="$( cd "$HS_OUTPUT_DIR" && find Generated -name '*.hs' \
          | sed -E 's#/#.#g; s#\.hs$##' | sort | sed 's/^/    /' )"
awk -v list="$MODS" '
  /-- BEGIN GENERATED MODULES/ { print; print list; skip=1; next }
  /-- END GENERATED MODULES/   { skip=0 }
  !skip                        { print }
' "$CABAL_FILE" >"$CABAL_FILE.tmp" && mv "$CABAL_FILE.tmp" "$CABAL_FILE"
echo "==> Updated $CABAL_FILE generated-modules block."

# -----------------------------------------------------------------------------
# 4. Point cabal at the headers and the freshly built shared library.
# -----------------------------------------------------------------------------
cat >"$SCRIPT_DIR/hs-project/cabal.project.local" <<EOF
package libsodium
    extra-include-dirs: $INCLUDE_DIR
    extra-lib-dirs: $LIB_DIR
EOF
echo "==> Wrote cabal.project.local"

# -----------------------------------------------------------------------------
# 5. Build and (if present) run the demo programs.
# -----------------------------------------------------------------------------
cd "$SCRIPT_DIR/hs-project"
export LD_LIBRARY_PATH="$LIB_DIR:${LD_LIBRARY_PATH:-}"

echo "==> cabal build"
cabal build all || { echo "build failed"; exit 1; }

# Each demo is self-contained (no arguments); the low-level and high-level
# variants must print identical output.
echo "==> demos (low-level vs high-level)"
for demo in secretbox sign; do
  for variant in low high; do
    exe="libsodium-$demo-$variant"
    cabal list-bin "$exe" >/dev/null 2>&1 || continue
    echo "--- $exe ---"
    cabal run -v0 "$exe"
  done
done
