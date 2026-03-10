# Security Hardening

Every measure currently in the config, where it lives, and why.

## No remote code execution

| Setting | File | What it does |
|---------|------|--------------|
| `package-enable-at-startup nil` | early-init.el, lisp/straight.el | Disables package.el entirely -- no ELPA bootstrap at startup |
| `package-archives nil` | lisp/straight.el | Empties the archive list -- even if package.el activates, nowhere to connect |
| `use-package-always-ensure nil` | lisp/straight.el | use-package never tries to install anything |
| Vendored deps committed to repo | vendor/ | No network needed at any point. `git clone` and run. |

## Local variable / eval restrictions

| Setting | File | What it does |
|---------|------|--------------|
| `enable-local-variables :safe` | lisp/defaults.el | `.dir-locals.el` can only set variables marked safe by their `defcustom` -- arbitrary vars rejected |
| `enable-local-eval nil` | lisp/defaults.el | Blocks `eval:` forms in file-local variables -- no code execution from opening a file |

## TLS / Network

| Setting | File | What it does |
|---------|------|--------------|
| `network-security-level 'high` | init.el | Emacs NSM rejects weak TLS connections |
| `gnutls-verify-error t` | init.el | Certificate verification failures are hard errors, not warnings |
| `gnutls-min-prime-bits 3072` | init.el, lisp/defaults.el | Rejects DH key exchange with primes under 3072 bits (per keylength.com recommendations) |
| `gnutls-algorithm-priority` | lisp/defaults.el | Forces TLS 1.2+ only, SECURE128/SECURE192 cipher suites. TLS 1.3 enabled when GnuTLS >= 3.6.5 |
| `tls-checktrust` tied to `gnutls-verify-error` | lisp/defaults.el | Fallback TLS programs also verify certificates |
| `tls-program` hardened | lisp/defaults.el | OpenSSL/GnuTLS CLI fallbacks use `--no_ssl3 --no_tls1 --no_tls1_1`, OCSP stapling, strict TOFU |

## Credential handling

| Setting | File | What it does |
|---------|------|--------------|
| `password-cache-expiry 3600` | init.el | Cached passwords expire after 1 hour (default is 16 seconds, which forces constant re-entry and encourages insecure workarounds) |

## Filesystem safety

| Setting | File | What it does |
|---------|------|--------------|
| `find-file-visit-truename t` | lisp/defaults.el | Resolves symlinks when visiting files -- prevents symlink-based path confusion |
| `vc-follow-symlinks nil` | lisp/defaults.el | Asks before following symlinks to version-controlled files |
| `create-lockfiles nil` | lisp/defaults.el | No `.#file` lockfiles (prevents information leakage about editing activity) |
| `make-backup-files nil` | lisp/defaults.el | No `file~` backups (prevents world-readable copies of sensitive content) |
| `large-file-warning-threshold 5MB` | lisp/defaults.el | Warns before opening files over 5MB (prevents accidental loading of binary blobs / core dumps) |
| `global-so-long-mode` | init.el | Auto-disables expensive features for minified / single-line megafiles (prevents denial-of-service via crafted files) |

## File content attack surface reduction

| Setting | File | What it does |
|---------|------|--------------|
| Enriched text mode disabled | lisp/defaults.el | Neutered `enriched-decode-display-prop` and removed `.rtf` from `auto-mode-alist`. Enriched text historically allowed arbitrary code execution via display properties embedded in file content |
| `inhibit-images t` | lisp/defaults.el | Disables inline image rendering. Images are decoded by native C libraries (libpng, libjpeg, librsvg, etc.) -- a crafted image can exploit memory corruption bugs in those libraries without any Emacs-level code running |
| `auto-image-file-mode nil` | lisp/defaults.el | Prevents Emacs from auto-detecting and rendering image files when visited |

## What we DON'T do

- No `custom-set-variables` for `eglot-server-programs` with unchecked paths -- LSP servers are found via `$PATH`, not hardcoded
- No `shell-command` or `async-shell-command` in hooks -- we never run shell commands implicitly on file open
- No `eval-after-load` with user-controlled data
- No Emacs server/daemon exposed to network (`emacsclient` is local socket only)

## Vendored dependency audit

Vendored packages are pinned to exact commits in `scripts/vendor.sh`. Only `.el` files are copied -- no CI scripts, no Makefiles, no shell scripts from upstream. The vendor script strips `test/` and `.github/` directories.

To audit what's vendored:

```bash
# Show all vendored packages and their line counts
for d in vendor/*/; do echo "$(basename $d): $(cat $d/*.el 2>/dev/null | wc -l) lines"; done

# Diff against upstream at pinned hash
cd vendor/evil && git diff $PINNED_HASH -- '*.el'
```
