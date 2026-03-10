# Fate Emacs: Project Values

## Philosophy

Learn the editor. Don't rely on someone else's code to do what you need.

Emacs is a Lisp machine with 40 years of built-in functionality. Most packages exist because people didn't read the manual. The goal is to understand the tool deeply enough to always be able to debug issues yourself -- not to accumulate a dependency tree you can't reason about.

This is the same "engine" approach applied to a text editor: own the code you run, understand every line, write tests for your customizations.

## Core Principles

### 1. Zero-download startup

A fresh clone should work immediately. No bootstrap scripts downloading packages from the internet. No `straight-pull-all`. No waiting for MELPA. Everything needed is already in the repo.

### 2. Vendor only what you can't reasonably write

Two vendored dependencies, chosen because replacing them would take months and produce worse results:

- **Evil** (+ evil-collection, evil-surround, evil-snipe, evil-easymotion) -- Vim emulation is a complete paradigm. The evil ecosystem is battle-tested across thousands of edge cases in cursor motion, text objects, operator-pending state, and visual mode. Writing this from scratch would be mass-producing bugs.

- **Magit** -- The best Git interface ever built. Porcelain-level abstraction over every Git operation, with transient menus, diff rendering, and staging UX that no custom code will match.

Everything else is custom elisp or built-in Emacs functionality.

### 3. Security by default

No code execution unless explicitly requested:

- `enable-local-variables` set to `:safe` -- `.dir-locals.el` can only set variables marked safe by their defcustom
- `enable-local-eval` disabled -- no `eval:` forms in file local variables
- TLS verification enforced, minimum 3072-bit DH primes
- No arbitrary package code runs at startup -- vendored deps are audited and pinned

### 4. Unit-test your code

Every piece of custom elisp gets an ERT test. Not "maybe later" -- before you consider it done.

- Custom completion functions: test that they return the right candidates
- Format-on-save: test that it pipes through the formatter and replaces buffer contents
- Indent detection: test against known file samples
- Language modes: test that keywords fontify correctly
- Utility functions: test edge cases, test error paths

Tests run with `emacs -Q --batch` -- no config loaded, no packages, no network. If your code needs the full config to be testable, the design is wrong. Factor out pure functions, mock what you must, keep the test surface small.

```bash
./test/run-tests.sh           # run everything
emacs -Q --batch -l test/test-autoloads.el -f ert-run-tests-batch-and-exit  # one file
```

### 5. Built-in first

Before adding any code, check if Emacs already does it:

- Completion: `completion-at-point`, `completion-in-region`, `fido-vertical-mode`
- Project management: `project.el`
- LSP: `eglot`
- Syntax highlighting: `tree-sitter`
- Snippets: `skeleton`, `abbrev-mode`, `tempo`
- Version control: `vc-mode` (for status), Magit (for operations)
- Formatting: `before-save-hook` + `call-process-region`
- Indentation detection: sample the buffer, count tabs vs spaces
- EditorConfig: parse the INI file yourself, it's 60 lines

### 6. Understand what you run

Every file in `lisp/` should be readable in one sitting. If a module grows past ~200 lines, it's doing too much. If you can't explain what a piece of code does, delete it and rewrite it from scratch -- you'll end up with something better.

## What this is NOT

- Not a framework. No module system, no DSL, no `doom!` macro.
- Not portable to other people's machines without them reading the code first. That's a feature.
- Not optimized for "out of the box" experience. Optimized for "I know exactly what's happening."
