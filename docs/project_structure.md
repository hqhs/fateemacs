# Project Structure

## Directory Layout

```
~/.emacs.d/
  early-init.el              startup perf: GC, file-handlers, redisplay suppression
  init.el                    entry point, load order, utility macros

  vendor/                    git submodules, pinned versions
    evil/                    vim emulation
    evil-collection/         evil bindings for built-in modes
    evil-surround/           surround text objects (cs, ds, ys)
    evil-snipe/              enhanced f/t motions
    evil-easymotion/         avy-based motion jumping
    magit/                   git porcelain
    (transitive deps)        compat, dash, transient, with-editor, etc.

  lisp/                      custom elisp (every file testable)
    defaults.el              built-in emacs settings, perf, security
    evil.el                  evil config + custom escape handler
    magit.el                 magit config (just settings, no code)
    completion.el            CUSTOM: completion-in-region via fido, capfs
    keybindings.el           leader key, all SPC bindings
    ui.el                    theme, mode-line, ligatures (built-in HarfBuzz)
    prog-conf.el             CUSTOM: prog-mode, formatter, indent detection, editorconfig
    autoloads.el             CUSTOM: search, file utils, rg integration
    project.el               built-in project.el config

  lang/                      language-specific configs
    rust.el                  eglot + tree-sitter + rustfmt
    python.el                eglot + tree-sitter + ruff
    cc.el                    eglot + clangd
    go.el                    eglot + tree-sitter
    elisp.el                 built-in
    org.el                   built-in
    *.el                     CUSTOM: simple keyword modes (dockerfile, meson, etc.)

  test/                      ERT test suite
    test-autoloads.el        unit tests for +fate/* functions
    test-config-loads.el     parse validation for all .el files
    test-completion.el       tests for custom completion-in-region
    test-prog-conf.el        tests for formatter, indent detection, editorconfig
    test-lang-modes.el       tests for custom keyword modes
    run-tests.sh             batch runner

  docs/                      project documentation
    project_values.md        philosophy and core principles
    project_structure.md     this file

  cache/                     runtime data (gitignored)
    autosave/
    tramp-autosave/
    recentf
    savehist
    projects

  snippets/                  skeleton/abbrev definitions (replaces yasnippet)
  tree-sitter/               compiled grammar .dylib/.so files
```

## Package Management

None. No straight.el, no package.el, no MELPA.

Vendored dependencies are committed directly into `vendor/`. Setup is:

```bash
git clone <repo-url> ~/.emacs.d
```

One command. No `--recursive`, no bootstrap, no network on first launch.

To re-vendor or update a pinned hash:

```bash
./scripts/vendor.sh          # pulls pinned commits into vendor/
./scripts/vendor.sh --force  # re-vendor everything from scratch
```

Loading is explicit in init.el:

```elisp
(dolist (dir (directory-files (expand-file-name "vendor/" user-emacs-directory) t "^[^.]"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))
```

## What Lives Where

| Concern | File(s) | External dep? |
|---------|---------|---------------|
| Vim emulation | vendor/evil*, lisp/evil.el | Yes (vendored) |
| Git interface | vendor/magit, lisp/magit.el | Yes (vendored) |
| Completion (all) | lisp/completion.el | No (completing-read + fido-vertical-mode, built-in) |
| LSP | lisp/lsp.el | No (eglot, built-in) |
| Syntax highlighting | lisp/prog-conf.el | No (tree-sitter, built-in) |
| Format on save | lisp/prog-conf.el | No (custom call-process-region) |
| Indent detection | lisp/prog-conf.el | No (custom heuristic) |
| EditorConfig | lisp/prog-conf.el | No (custom INI parser) |
| Snippets | lisp/snippets.el | No (skeleton/abbrev) |
| Search (rg) | lisp/autoloads.el | No (custom xref integration) |
| Theme | lisp/ui.el | No (monokai, checked in) |
| Ligatures | lisp/ui.el | No (HarfBuzz composition-function-table) |
| Whitespace cleanup | lisp/prog-conf.el | No (delete-trailing-whitespace) |
| Simple lang modes | lang/*.el | No (custom define-derived-mode) |

## Dependency Graph

```
init.el
  ├── vendor/*          (git submodules, load-path only)
  ├── lisp/defaults.el  (pure setq, no deps)
  ├── lisp/evil.el      (requires vendor/evil*)
  ├── lisp/magit.el     (requires vendor/magit)
  ├── lisp/completion.el(built-in only)
  ├── lisp/prog-conf.el (built-in only)
  ├── lisp/lsp.el       (built-in eglot)
  ├── lisp/keybindings.el(requires evil for leader)
  ├── lisp/ui.el        (built-in only)
  └── lang/*.el         (built-in + tree-sitter)
```
