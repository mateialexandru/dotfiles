#!/usr/bin/env bash
set -euo pipefail

stage=$1
root=$(cygpath -u "$2")
revision=$3
jobs=$4
shift 4
prefix=mingw-w64-ucrt-x86_64
packages=(
  autoconf automake make texinfo git diffutils patch tar gzip findutils
  "$prefix-gcc" "$prefix-libgccjit" "$prefix-pkgconf" "$prefix-texinfo"
  "$prefix-gnutls" "$prefix-libtree-sitter" "$prefix-libxml2" "$prefix-sqlite"
  "$prefix-librsvg" "$prefix-libwebp" "$prefix-giflib" "$prefix-libjpeg-turbo"
  "$prefix-libpng" "$prefix-libtiff" "$prefix-lcms2" "$prefix-harfbuzz"
  "$prefix-freetype" "$prefix-xpm-nox" "$prefix-zlib"
)

case "$stage" in
update)
  # A fresh shell finishes updates after the MSYS2 core runtime is replaced.
  pacman -Syu --noconfirm
  ;;
dependencies)
  missing=()
  for package in "${packages[@]}"; do
    if ! pacman -Q "$package" >/dev/null 2>&1; then missing+=("$package"); fi
  done
  if ((${#missing[@]})); then
    pacman -S --needed --noconfirm "${missing[@]}" || {
      echo 'Dependency installation failed. If the package database is stale, rerun with -UpdateTools.' >&2
      exit 1
    }
  fi
  ;;
source)
  source_dir="$root/source"
  fresh=0
  if [[ ! -d "$source_dir/.git" ]]; then
    git -c core.autocrlf=false clone --filter=blob:none --no-checkout https://github.com/emacs-mirror/emacs.git "$source_dir"
    fresh=1
  fi
  cd "$source_dir"
  git config core.autocrlf false
  [[ "$fresh" == 1 || -z "$(git status --porcelain --untracked-files=normal)" ]] || {
    echo 'Source checkout has local changes; refusing to overwrite them.' >&2
    exit 1
  }
  if ! git cat-file -e "$revision^{commit}" 2>/dev/null; then
    git fetch origin "$revision"
  fi
  git -c core.autocrlf=false checkout --detach "$revision"
  for fix in abc802ee2eb0b1663349ddf22a461f8e54a383fb 044b0203267db3b7912325d076d79a79000afe75; do
    git merge-base --is-ancestor "$fix" HEAD || {
      echo "Required upstream security fix is missing: $fix" >&2
      exit 1
    }
  done
  ;;
build)
  candidate=$(cygpath -u "$1")
  shift
  export CC=gcc CXX=g++ CFLAGS=-O2 CXXFLAGS=-O2 CPPFLAGS='' LDFLAGS='' PKG_CONFIG=pkg-config
  cd "$root/source"
  ./autogen.sh
  mkdir -p "$candidate/build" "$candidate/install"
  cd "$candidate/build"
  "$root/source/configure" --prefix="$candidate/install" \
    --host=x86_64-w64-mingw32 --build=x86_64-w64-mingw32 "$@"
  make -j"$jobs"
  # Windows login names may need domain qualification; numeric ownership is unambiguous.
  LOGNAME=$(id -u) make install
  ;;
*)
  echo "Unknown build stage: $stage" >&2
  exit 1
  ;;
esac
