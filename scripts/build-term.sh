#!/usr/bin/env bash
# Build the ghostel native module: Ghostty's terminal engine (libghostty-vt)
# wrapped as an Emacs dynamic module.
#
# This is optional. Without the module the config starts normally and
# `+fate/term' tells you to run this script; nothing else is affected.
#
# ghostel's build.zig refuses anything but one exact Zig version, so this does
# not use whatever `zig' happens to be on PATH unless it matches. Otherwise it
# drops the pinned toolchain into cache/zig/ (gitignored) and uses that, which
# keeps the build reproducible and leaves your system zig alone.
#
# Needs network on a cold build: the toolchain, and the libghostty source
# tarball pinned in vendor/ghostel/build.zig.zon.
#
# Usage: ./scripts/build-term.sh
set -euo pipefail

cd "$(dirname "$0")/.."
REPO="$(pwd)"
GHOSTEL="$REPO/vendor/ghostel"

if [ ! -d "$GHOSTEL" ]; then
  echo "error: vendor/ghostel missing. Run ./scripts/vendor.sh first." >&2
  exit 1
fi

# Single source of truth: the version build.zig itself demands.
ZIG_VERSION="$(sed -n 's/.*minimum_zig_version = "\([0-9.]*\)".*/\1/p' \
  "$GHOSTEL/build.zig.zon")"
if [ -z "$ZIG_VERSION" ]; then
  echo "error: could not read minimum_zig_version from build.zig.zon" >&2
  exit 1
fi

case "$(uname -s)-$(uname -m)" in
  Darwin-arm64)  ZIG_ARCH="aarch64-macos" ;;
  Darwin-x86_64) ZIG_ARCH="x86_64-macos" ;;
  Linux-aarch64) ZIG_ARCH="aarch64-linux" ;;
  Linux-x86_64)  ZIG_ARCH="x86_64-linux" ;;
  *) echo "error: unsupported platform $(uname -s)-$(uname -m)" >&2; exit 1 ;;
esac

# Prefer an exact match already on PATH (or $ZIG); fall back to the cache.
ZIG="${ZIG:-}"
if [ -z "$ZIG" ] && command -v zig > /dev/null 2>&1 \
   && [ "$(zig version)" = "$ZIG_VERSION" ]; then
  ZIG="$(command -v zig)"
fi

if [ -z "$ZIG" ]; then
  ZIG_DIR="$REPO/cache/zig/zig-$ZIG_ARCH-$ZIG_VERSION"
  ZIG="$ZIG_DIR/zig"
  if [ ! -x "$ZIG" ]; then
    url="https://ziglang.org/download/$ZIG_VERSION/zig-$ZIG_ARCH-$ZIG_VERSION.tar.xz"
    echo "Fetching zig $ZIG_VERSION for $ZIG_ARCH..."
    mkdir -p "$REPO/cache/zig"
    tmp=$(mktemp -d)
    curl -fsSL "$url" -o "$tmp/zig.tar.xz"
    tar -xJf "$tmp/zig.tar.xz" -C "$REPO/cache/zig"
    rm -rf "$tmp"
  fi
fi

if [ "$("$ZIG" version)" != "$ZIG_VERSION" ]; then
  echo "error: $ZIG is $("$ZIG" version), need exactly $ZIG_VERSION" >&2
  exit 1
fi

case "$(uname)" in
  Darwin) MODULE="ghostel-module.dylib" ;;
  *)      MODULE="ghostel-module.so" ;;
esac

cd "$GHOSTEL"
echo "Building $MODULE with zig $("$ZIG" version)..."
# Flags match ghostel's own Makefile. -Dcpu=baseline keeps the module loadable
# on any machine of this architecture, not just the one that built it.
"$ZIG" build --prefix . -Doptimize=ReleaseFast -Dcpu=baseline

if [ ! -f "$MODULE" ]; then
  echo "error: build finished but $MODULE is missing." >&2
  exit 1
fi

echo "Built $GHOSTEL/$MODULE"
echo "M-x +fate/term to use it."
