#!/usr/bin/env bash

# This script updates the vendored Musl headers.

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

##############################################################################
# Settings

declare MUSL_VER="${MUSL_VER:-1.2.5}"
declare DOWNLOAD_DIR="${DOWNLOAD_DIR:-/tmp}"

declare proj_dir
proj_dir="$(dirname "$(cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P)")"
declare musl_dir="${proj_dir}/hs-bindgen/musl-include"
declare tmp_dir=''

declare yolo=false
declare musl_tarball="musl-${MUSL_VER}.tar.gz"
declare targets=('x86_64' 'aarch64')


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
  echo '  MUSL_VER                 Musl version ' \
       "(${MUSL_VER})"
  echo '  DOWNLOAD_DIR             download directory ' \
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
  test -d "${musl_dir}" || error 'musl directory not found'
}

cleanup() {
  if [ "${yolo}" == true ] ; then
    rm -f "${DOWNLOAD_DIR}/${musl_tarball}"
    if [ "${tmp_dir}" != "" ] ; then
      rm -rf "${tmp_dir}"
    fi
  else
    rm -i "${DOWNLOAD_DIR}/${musl_tarball}"
    if [ "${tmp_dir}" != "" ] ; then
      echo 'Removing temporary directory...'
      rm -rI "${tmp_dir}"
    fi
  fi
}

download_musl() {
  local url="https://musl.libc.org/releases/${musl_tarball}"
  if [ ! -f "${DOWNLOAD_DIR}/${musl_tarball}" ] ; then
    echo 'Downloading musl...'
    wget --directory-prefix="${DOWNLOAD_DIR}" "${url}" \
      || error 'unable to download musl'
  fi
}

create_tmp_dir() {
  tmp_dir="$(mktemp -d --tmpdir=/tmp 'hs-bindgen-musl-update-XXX')"
  echo "Using temporary directory: ${tmp_dir}"
}

unpack_musl_source() {
  echo 'Unpacking musl source...'
  tar -C "${tmp_dir}" -zxf "${DOWNLOAD_DIR}/${musl_tarball}"
}

build_target() {
  local arch="${1}"
  local build_dir="${tmp_dir}/musl-${MUSL_VER}/build/${arch}"
  echo "Building ${arch}..."
  cd "${tmp_dir}/musl-${MUSL_VER}"
  mkdir -p "${build_dir}"
  {
    echo "ARCH = ${arch}"
    echo 'srcdir = .'
    echo "prefix = ${build_dir}"
    # shellcheck disable=SC2016
    echo 'exec_prefix = $(prefix)'
    # shellcheck disable=SC2016
    echo 'bindir = $(exec_prefix)/bin'
    # shellcheck disable=SC2016
    echo 'libdir = $(prefix)/lib'
    # shellcheck disable=SC2016
    echo 'includedir = $(prefix)/include'
    # shellcheck disable=SC2016
    echo 'syslibdir = $(prefix)/lib'
  } > config.mak
  make clean || error 'clean failed'
  make install-headers || error 'install-headers failed'
}

copy_musl_dir() {
  if [ "${yolo}" == true ] ; then
    rm -rf "${musl_dir}"
  else
    echo 'Reset project musl directory?'
    rm -rI "${musl_dir}"
    if [ -d "${musl_dir}" ] ; then
      error 'musl directory not deleted'
    fi
  fi
  mkdir "${musl_dir}"
  cp "${tmp_dir}/musl-${MUSL_VER}/VERSION" "${musl_dir}"
  cp "${tmp_dir}/musl-${MUSL_VER}/COPYRIGHT" "${musl_dir}"
  for target in "${targets[@]}" ; do
    cp -r \
      "${tmp_dir}/musl-${MUSL_VER}/build/${target}/include" \
      "${musl_dir}/${target}"
  done
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
trap 'cleanup' EXIT INT QUIT TERM HUP
download_musl
create_tmp_dir
unpack_musl_source
for target in "${targets[@]}" ; do
  build_target "${target}"
done
copy_musl_dir
report_success
