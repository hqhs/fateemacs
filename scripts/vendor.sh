#!/usr/bin/env bash
# Pull pinned dependencies into vendor/ for committing.
# Run once, commit the result. Re-run after changing a hash to update.
#
# Usage: ./scripts/vendor.sh [--force]
#   --force: re-vendor everything (removes vendor/ first)
set -euo pipefail

cd "$(dirname "$0")/.."
VENDOR_DIR="vendor"

if [ "${1:-}" = "--force" ]; then
  echo "Removing existing vendor directory..."
  rm -rf "$VENDOR_DIR"
fi

# ── Pinned commits ───────────────────────────────────────────────────
#
# Two dependencies. Everything else is custom elisp.
#
# Evil ecosystem:
EVIL_HASH="a7ffa73bbdc523c0e473d79c0ded7c6457bcb65c"
EVIL_COLLECTION_HASH="8da60e37b9f8e11dc24c49f1255b3711b37b16fc"
EVIL_SURROUND_HASH="c7116cdc774b1e259eaf3e9e7a318a6c99c2da17"
EVIL_SNIPE_HASH="c2108d3932fcd2f75ac3e48250d6badd668f5b4f"
EVIL_EASYMOTION_HASH="f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9"
AVY_HASH="be612110cb116a38b8603df367942e2bb3d9bdbe"
GOTO_CHG_HASH="278cd3e6d5107693aa2bb33189ca503f22f227d0"
ANNALIST_HASH="134fa3f0fb91a636a1c005c483516d4b64905a6d"
# Magit + transitive:
MAGIT_HASH="28bcd29db547ab73002fb81b05579e4a2e90f048"
TRANSIENT_HASH="3e30f5bff633a1d0d720305f6c8b5758b8ff1997"
WITH_EDITOR_HASH="5db5f0eb2202f52d44f529fe00654c866bb64eb1"
DASH_HASH="5df7605da5a080df769d4f260034fb0e5e86a7a4"
COMPAT_HASH="cccd41f549fa88031a32deb26253b462021d7e12" # 30.1.0.1

# ── vendor function ──────────────────────────────────────────────────

vendor() {
  local name="$1" url="$2" hash="$3"
  local dest="$VENDOR_DIR/$name"

  if [ -d "$dest" ]; then
    echo "  skip $name (already exists)"
    return
  fi

  echo "  pull $name @ ${hash:0:10}"
  tmp=$(mktemp -d)
  git clone --quiet "$url" "$tmp"
  git -C "$tmp" checkout --quiet "$hash"
  mkdir -p "$dest"
  # only .el files -- no docs, tests, CI, images
  find "$tmp" -name '*.el' \
    -not -path '*/test*' \
    -not -path '*/.github*' \
    -exec cp {} "$dest/" \;
  rm -rf "$tmp"
}

# magit keeps .el files under lisp/, everything else is the same
vendor_magit() {
  local dest="$VENDOR_DIR/magit"

  if [ -d "$dest" ]; then
    echo "  skip magit (already exists)"
    return
  fi

  echo "  pull magit @ ${MAGIT_HASH:0:10}"
  tmp=$(mktemp -d)
  git clone --quiet "https://github.com/magit/magit.git" "$tmp"
  git -C "$tmp" checkout --quiet "$MAGIT_HASH"
  mkdir -p "$dest"
  cp "$tmp"/lisp/*.el "$dest/"
  rm -rf "$tmp"
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
vendor_magit
vendor transient       https://github.com/magit/transient.git            "$TRANSIENT_HASH"
vendor with-editor     https://github.com/magit/with-editor.git          "$WITH_EDITOR_HASH"
vendor dash            https://github.com/magnars/dash.el.git            "$DASH_HASH"
vendor compat          https://github.com/emacs-compat/compat.git       "$COMPAT_HASH"

echo
echo "Done. Now: git add vendor/ && git commit -m 'vendor evil + magit'"
