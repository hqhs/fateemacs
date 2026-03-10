# Fate Emacs

Personal emacs configuration. Evil-based, minimal dependencies, built-in packages preferred.

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
