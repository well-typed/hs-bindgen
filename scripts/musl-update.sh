#!/usr/bin/env bash

# This script updates the vendored Musl headers.

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

##############################################################################
# Settings

declare MUSL_VER="${MUSL_VER:-1.2.5}"
declare AARCH64_TOOLCHAIN_VER="${AARCH64_TOOLCHAIN_VER:-14.2.rel1}"
declare DOWNLOAD_DIR="${DOWNLOAD_DIR:-/tmp}"

declare proj_dir
proj_dir="$(dirname "$(cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P)")"
declare musl_dir
musl_dir="${proj_dir}/hs-bindgen/musl-include"
declare tmp_dir=''

declare yolo=false

declare musl_tarball="musl-${MUSL_VER}.tar.gz"
declare aarch64_toolchain_tarball='arm-gnu-toolchain-'
aarch64_toolchain_tarball+="${AARCH64_TOOLCHAIN_VER}"
aarch64_toolchain_tarball+='-x86_64-aarch64-none-linux-gnu.tar.xz'


##############################################################################
# Functions

usage() {
  echo "Usage: $(basename "$0") [OPTION ...]"
  echo
  echo 'Options:'
  echo '  -h,--help    show this help text'
  echo '  -y,--yolo    do not confirm deletion'
  echo
  echo 'Environment:'
  echo "  MUSL_VER                 Musl version " \
       "(${MUSL_VER})"
  echo "  AARCH64_TOOLCHAIN_VER    aarch64 toolchain version " \
       "(${AARCH64_TOOLCHAIN_VER})"
  echo "  DOWNLOAD_DIR             download directory " \
       "(${DOWNLOAD_DIR})"
}

usage_error() {
  usage 1>&2
  exit 2
}

error() {
  echo "error: $*" 1>&2
  exit 1
}

assert_musl_dir_exists() {
  test -d "${musl_dir}" || error "musl directory not found"
}

assert_arch() {
  if [ "$(gcc -dumpmachine)" != "x86_64-pc-linux-gnu" ] ; then
    error "arch not x86_64-pc-linux-gnu"
  fi
}

cleanup() {
  if [ "${yolo}" == true ] ; then
    rm -f \
      "${DOWNLOAD_DIR}/${musl_tarball}" \
      "${DOWNLOAD_DIR}/${aarch64_toolchain_tarball}"
    if [ "${tmp_dir}" != "" ] ; then
      rm -rf "${tmp_dir}"
    fi
  else
    rm -i \
      "${DOWNLOAD_DIR}/${musl_tarball}" \
      "${DOWNLOAD_DIR}/${aarch64_toolchain_tarball}"
    if [ "${tmp_dir}" != "" ] ; then
      echo "Removing temporary directory..."
      rm -rI "${tmp_dir}"
    fi
  fi
}

download_musl() {
  local url="https://musl.libc.org/releases/${musl_tarball}"
  if [ ! -f "${DOWNLOAD_DIR}/${musl_tarball}" ] ; then
    echo "Downloading musl..."
    wget --directory-prefix="${DOWNLOAD_DIR}" "${url}" \
      || error "unable to download musl"
  fi
}

download_aarch64_toolchain() {
  local url='https://developer.arm.com/-/media/Files/downloads/gnu/'
  url+="${AARCH64_TOOLCHAIN_VER}"
  url+='/binrel/arm-gnu-toolchain-'
  url+="${AARCH64_TOOLCHAIN_VER}"
  url+='-x86_64-aarch64-none-linux-gnu.tar.xz'
  if [ ! -f "${DOWNLOAD_DIR}/${aarch64_toolchain_tarball}" ] ; then
    echo "Downloading aarch64 toolchain..."
    wget --directory-prefix="${DOWNLOAD_DIR}" "${url}" \
      || error "unable to download aarch64 toolchain"
  fi
}

create_tmp_dir() {
  tmp_dir="$(mktemp -d --tmpdir=/tmp 'hs-bindgen-musl-update-XXX')"
  echo "Using temporary directory: ${tmp_dir}"
}

unpack_musl_source() {
  echo "Unpacking musl source..."
  tar \
    -C "${tmp_dir}" \
    -zxf "${DOWNLOAD_DIR}/${musl_tarball}"
}

build_x86_64() {
  local build_dir="${tmp_dir}/musl-${MUSL_VER}/build/x86_64"
  echo 'Building x86_64...'
  cd "${tmp_dir}/musl-${MUSL_VER}"
  mkdir -p "${build_dir}"
  make clean || error 'clean failed'
  ./configure \
    --prefix="${build_dir}" \
    --exec-prefix="${build_dir}" \
    --syslibdir="${build_dir}/lib" \
    || error 'configure failed'
  make || error 'build failed'
  make install || error 'install failed'
}

unpack_aarch64_toolchain() {
  echo "Unpacking aarch64 toolchain..."
  tar \
    -C "${tmp_dir}" \
    -Jxf "${DOWNLOAD_DIR}/${aarch64_toolchain_tarball}"
}

build_aarch64() {
  local build_dir="${tmp_dir}/musl-${MUSL_VER}/build/aarch64"
  local old_path="${PATH}"
  local tmp_path="${tmp_dir}"
  tmp_path+='/arm-gnu-toolchain-'
  tmp_path+="${AARCH64_TOOLCHAIN_VER}"
  tmp_path+='-x86_64-aarch64-none-linux-gnu/bin'
  tmp_path+=":${PATH}"
  echo 'Building aarch64...'
  cd "${tmp_dir}/musl-${MUSL_VER}"
  mkdir -p "${build_dir}"
  make clean || error 'clean failed'
  export PATH="${tmp_path}"
  ./configure \
    --prefix="${build_dir}" \
    --exec-prefix="${build_dir}" \
    --syslibdir="${build_dir}/lib" \
    --target=aarch64-none-linux-gnu \
    || error 'configure failed'
  make || error 'build failed'
  make install || error 'install failed'
  export PATH="${old_path}"
}

copy_musl_dir() {
  if [ "${yolo}" == true ] ; then
    rm -rf "${musl_dir}"
  else
    echo "Reset project musl directory?"
    rm -rI "${musl_dir}"
    if [ -d "${musl_dir}" ] ; then
      error "musl directory not deleted"
    fi
  fi
  mkdir "${musl_dir}"
  cp "${tmp_dir}/musl-${MUSL_VER}/VERSION" "${musl_dir}"
  cp "${tmp_dir}/musl-${MUSL_VER}/COPYRIGHT" "${musl_dir}"
  cp -r \
    "${tmp_dir}/musl-${MUSL_VER}/build/x86_64/include" \
    "${musl_dir}/x86_64"
  cp -r \
    "${tmp_dir}/musl-${MUSL_VER}/build/aarch64/include" \
    "${musl_dir}/aarch64"
}

report_success() {
  echo 'Use git status to check results.'
  echo 'Success!'
}


##############################################################################
# Main

while [ $# -gt 0 ] ; do
  case "$1" in
    '-h' | '--help' )
      usage
      exit 0
      ;;
    '-y' | '--yolo' )
      yolo=true
      shift
      ;;
    * )
      usage_error
      ;;
  esac
done

assert_musl_dir_exists
assert_arch
trap 'cleanup' EXIT INT QUIT TERM HUP
download_musl
download_aarch64_toolchain
create_tmp_dir
unpack_musl_source
build_x86_64
unpack_aarch64_toolchain
build_aarch64
copy_musl_dir
report_success
