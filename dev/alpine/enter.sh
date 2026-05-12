#!/usr/bin/env bash
#
# Enter the Alpine dev container with the repository mounted at /repo.
#
# Usage:
#   ./dev/alpine/enter.sh                     # interactive shell
#   ./dev/alpine/enter.sh cabal build all     # one-shot command
#
# The image is built on demand from dev/alpine/Dockerfile. CI uses the same
# Dockerfile so local and CI environments are identical.
set -euo pipefail

REPO=$(git rev-parse --show-toplevel)
IMAGE_TAG="hs-bindgen-alpine-dev"

DOCKER_BIN=$(command -v docker || command -v podman)
if [[ -z "${DOCKER_BIN}" ]]; then
  echo "error: neither docker nor podman is on PATH" >&2
  exit 1
fi

"${DOCKER_BIN}" build --quiet -t "${IMAGE_TAG}" "${REPO}/dev/alpine" >/dev/null

# Multiple `.ghc.environment.*` files at the repo root confuse the fixture
# test runner. Host and container cabal builds write different ones into the
# same bind-mounted tree. Clear all of them on entry; the next `cabal build`
# inside the container will write a fresh, accurate file for the container's
# GHC.
rm -f "${REPO}"/.ghc.environment.*

# Use a separate cabal store inside the container to avoid mixing host
# (glibc) and container (musl) build artifacts.
CABAL_VOL="hs-bindgen-alpine-cabal-store"
"${DOCKER_BIN}" volume create "${CABAL_VOL}" >/dev/null

DOCKER_FLAGS=(--rm -v "${REPO}:/repo" -v "${CABAL_VOL}:/root/.cabal" -w /repo)
if [[ -t 0 && -t 1 ]]; then
  DOCKER_FLAGS+=(-it)
fi

exec "${DOCKER_BIN}" run "${DOCKER_FLAGS[@]}" "${IMAGE_TAG}" "$@"
