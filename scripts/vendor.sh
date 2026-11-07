#!/usr/bin/env bash
# Pull pinned dependencies into vendor/ for committing.
# Run once, commit the result. Re-run after changing a hash to update.
#
# Usage: ./scripts/vendor.sh [--force]
#   --force: re-vendor everything (removes vendor/ first)
#
# Each package records its pin in vendor/<name>/.pin, so a re-run only
# re-fetches the packages whose hash changed here. Bumping a dependency is a
# one-line edit followed by `./scripts/vendor.sh`.
set -euo pipefail

cd "$(dirname "$0")/.."
VENDOR_DIR="vendor"

if [ "${1:-}" = "--force" ]; then
  echo "Removing existing vendor directory..."
  rm -rf "$VENDOR_DIR"
fi

# ── Pinned commits ───────────────────────────────────────────────────
#
# Tagged releases are pinned to the tag's commit; the rest track the upstream
# default branch, because those projects do not tag meaningfully.
#
# Evil ecosystem (no useful tags -- upstream HEAD):
EVIL_HASH="6a3e1ddd04ac504a016590940d0af2a3361b9efd"
EVIL_COLLECTION_HASH="412c49c836db086ff990e7aa1c80a6994a8f9b03"
EVIL_SURROUND_HASH="e6548372e8359ee55e67d73ca418314086011f1a"
EVIL_SNIPE_HASH="16317d7e54313490a0fe8642ed9a1a72498e7ad2"
EVIL_EASYMOTION_HASH="629c894af63336028a61cc93d6465d10837eb82b"
AVY_HASH="933d1f36cca0f71e4acb5fac707e9ae26c536264"
GOTO_CHG_HASH="72f556524b88e9d30dc7fc5b0dc32078c166fda7"
ANNALIST_HASH="f83019d61b82a64e621d8dfd63a527a578ff0a11"
# Magit + transitive. Magit 4.7 dropped dash and picked up llama and cond-let:
MAGIT_HASH="bb79175d7c2ddc309b89f33b499c8f27d7965503"        # v4.7.1
TRANSIENT_HASH="3e3bf32c71d11b4309ecc62db30ec3b0042094e2"    # v0.13.9
WITH_EDITOR_HASH="0d02c350fbde69bfd6ba3fafa561f331c7fb1066"  # v3.6.0
COMPAT_HASH="904a2ca44b168bf4444c35c073f26e6d7a038258"       # 31.1.0.0
LLAMA_HASH="9bd4ae571bc976cc37c63d308a1bc99628b9ed29"        # v1.0.6
COND_LET_HASH="54e7962ee8fa281c49dacef0ea22fd596621e63e"     # v1.1.5
# Snippets:
YASNIPPET_HASH="691ae42961b9849afaf4bd5c4abe7d443952334b"    # 0.14.0
# Completion:
CORFU_HASH="51e1ca1215b494c9d8e2c9a383ee75fd102c9473"        # 2.16
# Editing/UI:
DIFF_HL_HASH="8dfb5c27ea43b8c7ac1c008bb97090ad4f059285"
INDENT_BARS_HASH="7ef4c38cb1f54cac8f5b43661aee6795b530966c"
SOLARIZED_HASH="50a13dfdde6ccf998a03095c3ddfcfedf6efcacc"
# Language modes:
GLSL_MODE_HASH="854e6e5c9a971634c9d40501b4ef3ca71881739b"
YAML_MODE_HASH="93dba98c050e9abfc623ec66aa499dbbb46b2fe1"
# cmake-mode ships inside the CMake tree, so it is pinned to a CMake release
# and pulled as a single file rather than cloned.
CMAKE_TAG="v4.4.5"
# Terminal. Unlike everything else here, ghostel is half Zig: the elisp drives
# a native module built from src/ against libghostty-vt. Needs the build
# inputs vendored too, and `./scripts/build-term.sh' to produce the module.
GHOSTEL_HASH="3f3631e83f4a35243dfd25e1a8e6a542c4097691"

# ── vendor function ──────────────────────────────────────────────────
#
# vendor <name> <url> <hash> [subdir]
#
# Copies the .el files from <subdir> (default: repo root), preserving
# structure -- evil-collection needs modes/*/ and corfu needs extensions/.

vendor() {
  local name="$1" url="$2" hash="$3" subdir="${4:-.}"
  local dest="$(pwd)/$VENDOR_DIR/$name"
  local pin="$dest/.pin"

  if [ -f "$pin" ] && [ "$(cat "$pin")" = "$hash" ]; then
    echo "  skip $name (pinned at ${hash:0:10})"
    return
  fi

  echo "  pull $name @ ${hash:0:10}"
  tmp=$(mktemp -d)
  # Shallow single-commit fetch: cloning magit's full history to throw it away
  # costs far more than it buys.
  git -C "$tmp" init --quiet
  git -C "$tmp" remote add origin "$url"
  git -C "$tmp" fetch --quiet --depth 1 origin "$hash"
  git -C "$tmp" checkout --quiet FETCH_HEAD

  rm -rf "$dest"
  mkdir -p "$dest"
  cd "$tmp/$subdir"
  # Only package sources. Test files and the upstream projects' own lint/CI
  # helpers are dead weight in a tree that is supposed to be auditable.
  find . -name '*.el' \
    -not -path './test/*' \
    -not -path './tests/*' \
    -not -path './.github/*' \
    -not -path './lint/*' \
    -not -path './targets/*' \
    -not -name '*-test.el' \
    -not -name '*-tests.el' \
    -print0 | while IFS= read -r -d '' f; do
      mkdir -p "$dest/$(dirname "$f")"
      cp "$f" "$dest/$f"
  done
  cd - > /dev/null
  rm -rf "$tmp"
  echo "$hash" > "$pin"
}

# vendor_ghostel -- the one dependency that is not pure elisp. Takes the elisp
# plus the Zig build inputs and the bundled xterm-ghostty terminfo, flattening
# lisp/ to the package root like the other magit-style layouts. The module
# binary itself is built locally and gitignored, not vendored.
vendor_ghostel() {
  local dest="$(pwd)/$VENDOR_DIR/ghostel"
  local pin="$dest/.pin"

  if [ -f "$pin" ] && [ "$(cat "$pin")" = "$GHOSTEL_HASH" ]; then
    echo "  skip ghostel (pinned at ${GHOSTEL_HASH:0:10})"
    return
  fi

  echo "  pull ghostel @ ${GHOSTEL_HASH:0:10}"
  tmp=$(mktemp -d)
  git -C "$tmp" init --quiet
  git -C "$tmp" remote add origin "https://github.com/dakra/ghostel.git"
  git -C "$tmp" fetch --quiet --depth 1 origin "$GHOSTEL_HASH"
  git -C "$tmp" checkout --quiet FETCH_HEAD

  rm -rf "$dest"
  mkdir -p "$dest"
  cp "$tmp"/lisp/*.el "$dest/"
  cp -R "$tmp"/src "$tmp"/vendor "$tmp"/etc "$dest/"
  cp "$tmp"/build.zig "$tmp"/build.zig.zon "$tmp"/symbols.map "$dest/"
  # evil-ghostel is a separate package in the same repo; straight.el already
  # puts vendor/*/extensions/*/ on the load-path. consult-ghostel is skipped --
  # no consult here.
  mkdir -p "$dest/extensions"
  cp -R "$tmp"/extensions/evil-ghostel "$dest/extensions/"
  rm -rf "$tmp"
  echo "$GHOSTEL_HASH" > "$pin"
}

# vendor_file <name> <url> <pin> -- single .el file, no repository to clone.
vendor_file() {
  local name="$1" url="$2" pin_value="$3"
  local dest="$(pwd)/$VENDOR_DIR/$name"
  local pin="$dest/.pin"

  if [ -f "$pin" ] && [ "$(cat "$pin")" = "$pin_value" ]; then
    echo "  skip $name (pinned at $pin_value)"
    return
  fi

  echo "  pull $name @ $pin_value"
  rm -rf "$dest"
  mkdir -p "$dest"
  curl -fsSL "$url" -o "$dest/$name.el"
  echo "$pin_value" > "$pin"
}

# ── Pull everything ──────────────────────────────────────────────────

echo "Vendoring dependencies..."

echo "Evil:"
vendor evil            https://github.com/emacs-evil/evil.git            "$EVIL_HASH"
vendor evil-collection https://github.com/emacs-evil/evil-collection.git "$EVIL_COLLECTION_HASH"
vendor evil-surround   https://github.com/emacs-evil/evil-surround.git   "$EVIL_SURROUND_HASH"
vendor evil-snipe      https://github.com/hlissner/evil-snipe.git        "$EVIL_SNIPE_HASH"
vendor evil-easymotion https://github.com/PythonNut/evil-easymotion.git  "$EVIL_EASYMOTION_HASH"
vendor avy             https://github.com/abo-abo/avy.git                "$AVY_HASH"
vendor goto-chg        https://github.com/emacs-evil/goto-chg.git        "$GOTO_CHG_HASH"
vendor annalist        https://github.com/noctuid/annalist.el.git        "$ANNALIST_HASH"

echo "Magit:"
vendor magit           https://github.com/magit/magit.git                "$MAGIT_HASH" lisp
vendor transient       https://github.com/magit/transient.git            "$TRANSIENT_HASH" lisp
vendor with-editor     https://github.com/magit/with-editor.git          "$WITH_EDITOR_HASH" lisp
vendor compat          https://github.com/emacs-compat/compat.git        "$COMPAT_HASH"
vendor llama           https://github.com/tarsius/llama.git              "$LLAMA_HASH"
vendor cond-let        https://github.com/tarsius/cond-let.git           "$COND_LET_HASH"

echo "Snippets:"
vendor yasnippet       https://github.com/joaotavora/yasnippet.git       "$YASNIPPET_HASH"

echo "Completion:"
vendor corfu           https://github.com/minad/corfu.git                "$CORFU_HASH"

echo "Editing/UI:"
vendor diff-hl         https://github.com/dgutov/diff-hl.git             "$DIFF_HL_HASH"
vendor indent-bars     https://github.com/jdtsmith/indent-bars.git       "$INDENT_BARS_HASH"
vendor solarized-theme https://github.com/bbatsov/solarized-emacs.git    "$SOLARIZED_HASH"

echo "Language modes:"
vendor glsl-mode       https://github.com/jimhourihan/glsl-mode.git      "$GLSL_MODE_HASH"
vendor yaml-mode       https://github.com/yoshiki/yaml-mode.git          "$YAML_MODE_HASH"
vendor_file cmake-mode \
  "https://raw.githubusercontent.com/Kitware/CMake/$CMAKE_TAG/Auxiliary/cmake-mode.el" \
  "$CMAKE_TAG"

echo "Terminal:"
vendor_ghostel

echo
echo "Done. Now: git add vendor/ && git commit"
if [ ! -f "$VENDOR_DIR/ghostel/ghostel-module.dylib" ] \
   && [ ! -f "$VENDOR_DIR/ghostel/ghostel-module.so" ]; then
  echo "Note: no terminal module built. ./scripts/build-term.sh (optional)."
fi
