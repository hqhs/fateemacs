# Fate Emacs

This is a stripped-down fork of Doom Emacs with every dependency vendored
under `vendor/`. The goal of changes here is to reduce cognitive load
while coding, not to learn Emacs internals.

## Structure

- `early-init.el` - Pre-init: GC, file-handler, redisplay suppression for fast startup
- `init.el` - Entry point, loads lisp/ and lang/ modules
- `lisp/` - Core config modules (completion, evil, lsp, keybindings, etc.)
- `lang/` - Language-specific configs
- `test/` - ERT test suite
- `snippets/` - Yasnippet snippets
- `cache/` - Runtime data (autosave, recentf, savehist)

## Testing

Run all tests:
```bash
./test/run-tests.sh
```

Run a specific test file:
```bash
emacs -Q --batch -l test/test-config-loads.el -f ert-run-tests-batch-and-exit
```

### Test files
- `test/test-config-loads.el` - Validates all .el files parse correctly, checks macro/var definitions
- `test/test-autoloads.el` - Unit tests for `+fate/*` autoload functions (search, copy-path, etc.)

### Writing new tests
- Use ERT (`require 'ert`), not buttercup
- File naming: `test/test-<module>.el`
- Use `cl-letf` to mock functions, `unwind-protect` for cleanup
- Tests run with `emacs -Q --batch` (no packages loaded), so `require` any needed built-ins

## Conventions

- Prefix custom functions with `+fate/` (public) or `+fate--` (private)
- Use `use-package` with `:straight t` for external packages, `:ensure nil` for built-in
- Prefer built-in packages over external deps
- `fate-emacs-dir`, `fate-lisp-dir`, `fate-cache-dir` are the key path variables

### Mode

Pure delegation. Output working Elisp that matches the existing
Doom-style idioms (`use-package`, `after!`, `:config`, `:hook`). Edit
files directly. Keep explanation minimal — I review at PR time, not
per-change.

### Vendoring discipline (strict)

- Never use `package-install`, `package-refresh-contents`,
  `straight-use-package`, or `use-package :ensure t`. All dependencies
  are vendored under `vendor/`.
- To add a new package: clone its source into `vendor/<name>/`, add the
  `load-path` entry, and wire it up with `use-package`. Do all of this
  in the same change; don't leave a dangling `use-package` for a
  package that isn't vendored yet.
- Prefer built-in Emacs packages (eglot, flymake, project.el, dired,
  tab-bar, etc.) over new vendored dependencies. Assume Emacs 29+.
- Check the `vendor/` directory before adding anything. If a similar
  package is already there, use it.
- Never use `custom-set-variables` or the Customize UI. Everything
  lives in hand-written config.

### Current vendored packages

annalist, avy, cmake-mode, compat, corfu, dash, evil, evil-collection,
evil-easymotion, evil-snipe, evil-surround, glsl-mode, goto-chg, magit,
transient, with-editor, yaml-mode, yasnippet.

Completion UI is Corfu. No LSP client installed yet; use built-in eglot
when one is needed.

### Behavioral rules

- Never ask about pausing, stopping, or "is this a good stopping point."
- When I describe a symptom ("eglot flakes out," "corfu lags"),
  investigate first: hypotheses in prose before touching config.
- If a change is genuinely ambiguous (multiple reasonable approaches
  with different tradeoffs), name the choice and pick one. Don't ask.
- If a change requires something outside Emacs (a system binary like
  `clangd`, a dotfile outside this repo), call it out clearly at the
  top of the response so I don't miss it in PR review.
