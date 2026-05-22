# Alpine-based dev container for hs-bindgen.
#
# Single source of truth for the Alpine build environment, consumed by both
# the local dev loop (dev/alpine/enter.sh) and CI
# (.github/workflows/alpine.yml).
#
# When bumping the Alpine version, also re-run a full `cabal build all` and
# `cabal test all` inside the container to surface any apk-package drift.
FROM alpine:3.21
ARG LLVM_VERSION=19

RUN apk add --no-cache \
      bash git gcc g++ make pkgconfig musl-dev \
      ghc cabal \
      llvm${LLVM_VERSION}-dev \
      clang${LLVM_VERSION}-dev \
      clang${LLVM_VERSION}-libclang \
      clang${LLVM_VERSION}-static \
      gmp-dev ncurses-dev libffi-dev \
      zlib-dev zlib-static \
      libpcap-dev \
      libdispatch-dev \
      file xz curl

# `git submodule init` and similar inside the bind-mounted /repo otherwise
# fails with "dubious ownership" because the host UID differs from the
# container's root UID. System-wide opt-out is fine for this dev/CI container.
RUN git config --system --add safe.directory '*'

# Build doxygen 1.15.0 from source to match the version pinned by the rest
# of CI (.github/actions/setup-doxygen) and the nix dev shell. Alpine 3.21's
# apk-packaged doxygen is 1.12.0, which produces slightly different XML for
# some doxygen-comment edge cases, causing four golden tests to drift. The
# upstream binary tarball is glibc-linked, so source build is the only path
# on musl. Build deps are installed as a virtual apk package so they can be
# removed in the same RUN step, keeping the final image small.
RUN apk add --no-cache --virtual .doxygen-build-deps \
        cmake flex bison python3 g++ \
    && curl -fsSL -o /tmp/doxygen.tar.gz \
         https://github.com/doxygen/doxygen/releases/download/Release_1_15_0/doxygen-1.15.0.src.tar.gz \
    && cd /tmp && tar xzf doxygen.tar.gz \
    && cd doxygen-1.15.0 && mkdir build && cd build \
    && cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local .. \
    && make -j"$(nproc)" \
    && make install \
    && cd / && rm -rf /tmp/doxygen-1.15.0 /tmp/doxygen.tar.gz \
    && apk del .doxygen-build-deps \
    && doxygen --version

# llvm-config ships under the versioned prefix and is not on PATH by default.
ENV PATH=/usr/lib/llvm${LLVM_VERSION}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

WORKDIR /repo
CMD ["bash"]
