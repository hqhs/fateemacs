# Terminal

`SPC '` opens a terminal (`+fate/term`). It runs ghostel, which drives
Ghostty's terminal engine (libghostty-vt) through a compiled native module.
Setup and the optional-by-design build live in `lisp/term.el` and
`scripts/build-term.sh`; this file is just the keys.

`SPC '` only works from evil normal state, so it will not open a second
terminal from inside the first.

## Getting out

A terminal grabs almost every key, but not all of them.
`ghostel-keymap-exceptions` is `("C-c" "C-x" "C-u" "C-h" "M-x" "M-:" "C-\")`
and those prefixes never reach the shell. That is the escape hatch:

| Key | Effect |
| --- | --- |
| `C-x o` | Other window. Terminal keeps running in the background. |
| `C-x b` | Switch buffer. |
| `C-x 0` / `C-x 1` | Delete / maximise the window. |
| `M-x` | Any command, straight from the terminal. |
| `ESC` | Leave insert state for evil normal state, where `SPC` works again. |

`C-g` is the exception to the exception: in a terminal it is
`ghostel-send-C-g`, i.e. an interrupt sent to the shell, *not* the global
evil-escape it is everywhere else. Use `ESC` to change state and `C-g` to
interrupt what is running.

### When ESC does not leave insert state

`evil-ghostel-escape` is `auto`, which routes insert-state `ESC` **to the
application** whenever the terminal is in alt-screen mode -- vim, less, htop,
Claude Code. This is deliberate: those programs need their own `ESC`.

| Key | Effect |
| --- | --- |
| `C-c <escape>` | Force evil normal state once, leaving the routing alone. |
| `C-c C-r` | Toggle ESC routing for this buffer (`evil-ghostel-toggle-send-escape`). |

Set `evil-ghostel-escape` to `evil` or `terminal` to pick one behaviour
globally instead of the alt-screen heuristic.

## Input modes

The mode decides how much of the keyboard the terminal gets. Semi-char is the
default and the one you type in.

| Key | Mode | Behaviour |
| --- | --- | --- |
| `C-c C-j` | semi-char | Default. Most keys to the shell, Emacs prefixes kept. |
| `C-c M-d` | char | Everything to the shell, including `C-c`. Only `C-c C-j` gets out. |
| `C-c C-l` | line | Compose a line in Emacs, send it with `RET`. |
| `C-c C-t` | copy | Read-only. Move point and copy freely. |
| `C-c C-e` | emacs | Read-only with plain Emacs keys everywhere. |

## Signals and sending keys literally

| Key | Sends |
| --- | --- |
| `C-c C-c` | `C-c` (interrupt) |
| `C-c C-z` | `C-z` (suspend) |
| `C-c C-d` | `C-d` (EOF) |
| `C-c C-\` | `C-\` (quit) |
| `C-c C-q` | the next key literally, whatever it is |

## Navigation and clipboard

| Key | Effect |
| --- | --- |
| `C-c C-n` / `C-c C-p` | Next / previous hyperlink (OSC 8, URLs, `file:line`) |
| `C-c M-n` / `C-c M-p` | Next / previous shell prompt (OSC 133) |
| `C-c C-y` | Paste into the terminal |
| `C-c M-w` | Copy the whole buffer |
| `C-c M-l` | Clear scrollback |

## Multiple terminals

These have **no default keys** -- they are only on the `Ghostel` menu, so
reach them with `M-x` until they are worth binding:

| Command | Effect |
| --- | --- |
| `ghostel-new` | New terminal |
| `ghostel-project-new` | New terminal in the current project root |
| `ghostel-next` / `ghostel-previous` | Cycle terminals |
| `ghostel-list-buffers` | List them |
| `ghostel-status-next` | Jump to the next terminal **needing attention** |

`ghostel-status-next` is the interesting one for running several agents at
once: ghostel tracks per-buffer attention state from OSC 9/777 notifications
and progress reports, so a finished or prompting session marks itself without
polling.

## Gotchas

- **Restart after changing `lisp/term.el`.** The evil wiring is installed
  once, from the `ghostel` `:config` block.
- **`j` and `k`.** The `jk` escape in `lisp/evil.el` binds both in *every*
  insert state, and its handler writes into renderer-owned text rather than
  the PTY, so plain `j`/`k` would never reach the shell.
  `+fate--term-setup-evil` rebinds them for insert state in terminal buffers.
  If typing `j` ever stops working, that is the thing that broke;
  `test/test-term.el` guards it.
- **No module, no terminal.** If `SPC '` reports a missing module, run
  `./scripts/build-term.sh`. Re-vendoring ghostel deletes the module, so
  rebuild after a dependency bump.
