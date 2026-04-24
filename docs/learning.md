# Learning Evil (Vim in Emacs)

Practical progression path. One new concept per week — use it until it's muscle memory.

## Phase 1: Text Objects (week 1-2)

Text objects are the biggest multiplier. They combine with any operator (`d`, `c`, `y`, `v`).

| Command | Action |
|---------|--------|
| `ciw`   | Change inner word (delete word, enter insert) |
| `ci"`   | Change inside quotes |
| `ci(`   | Change inside parens |
| `ci{`   | Change inside braces |
| `ca(`   | Change around parens (including the parens) |
| `diw`   | Delete inner word |
| `yiw`   | Yank inner word |
| `vip`   | Select inner paragraph |
| `vi{`   | Select inside braces |

The `i` (inner) vs `a` (around) distinction applies to all paired delimiters.
With evil-surround: `cs"'` changes surrounding `"` to `'`, `ds"` deletes surrounding quotes, `ysiw"` wraps word in quotes.

## Phase 2: Line Motions (week 3)

| Command | Action |
|---------|--------|
| `f<c>`  | Jump forward to character `c` on current line |
| `t<c>`  | Jump forward to just before character `c` |
| `F<c>`  | Jump backward to character `c` |
| `;`     | Repeat last f/t motion |
| `,`     | Repeat last f/t motion in reverse |
| `0`     | Start of line |
| `^`     | First non-blank character |
| `$`     | End of line |

Combine with operators: `dt)` deletes up to (but not including) `)`. `cf,` changes through the next comma.
evil-snipe extends `s`/`S` for two-character searches across lines.

## Phase 3: Broader Motions (week 4)

| Command | Action |
|---------|--------|
| `%`     | Jump to matching bracket/paren |
| `{` / `}` | Previous / next paragraph (blank line) |
| `gg` / `G` | Start / end of file |
| `*` / `#` | Search current word forward / backward |
| `gd`    | Go to definition (with eglot) |
| `C-o` / `C-i` | Jump back / forward in jump list |

## Phase 4: Registers and Macros (week 5-6)

### Registers
| Command | Action |
|---------|--------|
| `"ayy`  | Yank line into register `a` |
| `"ap`   | Paste from register `a` |
| `"Ayy`  | Append yank to register `a` (uppercase = append) |
| `"0p`   | Paste last yank (ignores deletes) |
| `"+y`   | Yank to system clipboard |
| `"+p`   | Paste from system clipboard |

### Macros
| Command | Action |
|---------|--------|
| `qa`    | Start recording macro into register `a` |
| `q`     | Stop recording |
| `@a`    | Play macro from register `a` |
| `@@`    | Replay last macro |
| `10@a`  | Play macro 10 times |

Macro workflow: position cursor, record a repeatable edit, apply N times.

## Phase 5: Advanced Operators (week 7+)

| Command | Action |
|---------|--------|
| `>>` / `<<` | Indent / outdent line |
| `>ip`   | Indent paragraph |
| `gq`    | Reflow text (respects fill-column) |
| `gu` / `gU` | Lowercase / uppercase |
| `g~`    | Toggle case |
| `.`     | Repeat last change — the most powerful single key |

## Practice Method

1. Pick one concept from the next phase
2. Use it deliberately for a full week, even when it feels slower
3. Once it's automatic, pick the next one
4. Revisit `vimtutor` periodically — you'll notice new things each time

Run `vimtutor` from terminal, or use the built-in evil-tutor if installed.

## Fate Emacs Specific

| Binding | Command |
|---------|---------|
| `SPC SPC` | Find file in project |
| `SPC f s` | Save buffer |
| `SPC g g` | Magit status |
| `SPC p c` | Project compile |
| `SPC c c` | Compile |
| `SPC c r` | Recompile |
| `K`       | Show documentation (eldoc) |
