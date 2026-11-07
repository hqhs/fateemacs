# Emacs Workflow Coach — Design Document

## Overview

Three small, loosely coupled packages sharing a common event export format.
The goal is minimal workflow intrusion: passive observation, periodic analysis,
gentle nudging.

```
emacs-workflow-capture  →  export file  →  emacs-workflow-coach
                                       →  emacs-workflow-drill
```

---

## Shared Export Format

All packages communicate through a single append-only JSONL file
(`~/.emacs-workflow/events.jsonl`). Each line is one burst.

### Burst Structure

A burst is a sequence of commands with no idle gap longer than the flush
threshold (default: 5 seconds, same pattern as GCMH).

```json
{
  "burst_id": "2026-06-03T15:42:00Z",
  "session_id": "abc123",
  "events": [
    {
      "cmd": "forward-word",
      "mode": "c++-mode",
      "buf_type": "file",
      "prev_cmd": "forward-word"
    },
    {
      "cmd": "kill-region",
      "mode": "c++-mode",
      "buf_type": "file",
      "prev_cmd": "forward-word"
    }
  ]
}
```

### Field Reference

| Field | Type | Notes |
|-------|------|-------|
| `burst_id` | ISO timestamp | Start time of burst, used as unique ID |
| `session_id` | string | Randomized per Emacs session |
| `cmd` | string | `this-command` symbol name |
| `mode` | string | `major-mode` at time of command |
| `buf_type` | enum | `file`, `dired`, `scratch`, `special` |
| `prev_cmd` | string | Previous command in burst (null for first) |

### What Is Explicitly Not Captured

- Buffer contents or file paths
- Cursor position or region contents
- Raw keystrokes or keybindings
- Any personally identifiable information

---

## Package 1: `emacs-workflow-capture`

### Responsibilities

- Hook into `post-command-hook`
- Maintain an in-memory ring buffer
- Flush to disk on idle (GCMH-style)
- Rotate export file to prevent unbounded growth

### Ring Buffer

Fixed-size vector, default 500 events. Older events are dropped on overflow
(we care about recent patterns, not full history).

### Flush Logic

```
post-command-hook fires
  → push event to ring buffer

idle-timer fires (default: 5s with no commands)
  → if ring buffer non-empty:
      → serialize buffer as one burst to events.jsonl
      → clear ring buffer
      → reset idle timer
```

The idle gap is the burst boundary. This naturally aligns with how humans
actually work — pauses between distinct micro-tasks.

### File Rotation

When `events.jsonl` exceeds a configurable size (default: 5MB), rotate:
- Rename to `events.jsonl.1`
- Keep last N rotations (default: 3)
- Coach and Drill always read from `events.jsonl` only

### Configuration

```elisp
(setq ewc-flush-idle-seconds 5)
(setq ewc-ring-buffer-size 500)
(setq ewc-export-file "~/.emacs-workflow/events.jsonl")
(setq ewc-max-file-size (* 5 1024 1024))
(setq ewc-max-rotations 3)
```

### Performance Constraints

`post-command-hook` runs on every keystroke. The handler must be O(1) and
allocation-free in the hot path. Only the flush path does I/O.

---

## Package 2: `emacs-workflow-coach`

### Responsibilities

- Periodically read `events.jsonl`
- Aggregate patterns into a human-readable report
- Generate a ready-to-use prompt for Claude Code
- No API calls, no automation — output is for human review

### Analysis Trigger

On-demand via `M-x ewcoach-report`. Generates two files and opens them.

### Aggregation

Reads bursts since last report checkpoint and computes:

```json
{
  "period": "2026-06-01 to 2026-06-07",
  "top_commands": [
    {"cmd": "forward-word", "mode": "c++-mode", "count": 412},
    {"cmd": "backward-char", "mode": "c++-mode", "count": 287}
  ],
  "frequent_sequences": [
    {"seq": ["forward-word", "forward-word", "forward-word"], "count": 89},
    {"seq": ["isearch-forward", "isearch-forward", "keyboard-quit"], "count": 34}
  ],
  "active_goals": ["avy-goto-word", "consult-line"]
}
```

### Output: Report File

`~/.emacs-workflow/report.md` — human-readable summary of patterns observed.
Opened automatically after generation for review.

### Output: Prompt File

`~/.emacs-workflow/prompt.md` — ready to paste into Claude Code. Contains:

1. The aggregated data
2. Your current `init.el` (or relevant sections)
3. A fixed instruction preamble:

```
You are an Emacs workflow analyst. Given command frequency and sequence data
and my current Emacs config, suggest specific improvements. Be concrete:
provide exact elisp to add or modify in my config. Prioritize high-frequency
patterns. Do not suggest anything already in active_goals.

For each suggestion also provide a goal in this format so I can add it to
my goals file:
GOAL_CMD: <command>
GOAL_TARGET: <N>
```

You review the prompt, optionally edit it, then feed it to Claude Code.
Claude Code produces elisp diffs you apply at your discretion.

---

## Package 3: `emacs-workflow-drill`

### Responsibilities

- Parse `suggestions.org` for active goals
- Watch `events.jsonl` for goal commands appearing
- Update progress in `suggestions.org` automatically
- Emit a non-blocking nudge when you do something the old way

### Goal Tracking

On idle, Drill reads new bursts since its last checkpoint and counts
occurrences of any active goal command. Updates the org file in place:

```org
:GOAL_CMD: avy-goto-word
:GOAL_TARGET: 20
:GOAL_PROGRESS: 7
:GOAL_STATUS: active     ; → changes to "done" when progress >= target
```

When a goal completes, the TODO headline is marked DONE automatically.

### Nudging

When Drill detects a frequent-sequence pattern that matches a known suggestion
(e.g., three consecutive `forward-word` calls), it emits a single minibuffer
message:

```
[Coach] Tip: avy-goto-word (goal: 7/20) — C-g to dismiss
```

Nudge rules:
- At most once per 5 minutes per suggestion
- Never during isearch or active minibuffer
- Never if the user has dismissed this suggestion 3+ times (tracked in org file)

### Configuration

```elisp
(setq ewdrill-nudge-cooldown-minutes 5)
(setq ewdrill-max-dismissals 3)
(setq ewdrill-goals-file "~/.emacs-workflow/suggestions.org")
```

---

## File Layout

```
~/.emacs-workflow/
  events.jsonl          ; append-only burst log (capture writes, coach/drill read)
  events.jsonl.1        ; rotated
  report.md             ; human-readable pattern summary (coach writes)
  prompt.md             ; ready-to-paste Claude Code prompt (coach writes)
  suggestions.org       ; goals file (human writes after Claude Code review, drill updates)
```

---

## Open Questions

1. **Aggregation granularity** — pre-aggregating before sending loses some
   sequence information. May need tuning based on actual analysis quality.
   Could include raw burst samples for the most interesting sequences.

2. **Cold start** — no suggestions until first analysis run. Worth shipping
   a small set of universally useful default goals (e.g., `avy`, `consult`)
   that activate immediately if those packages are installed but rarely used.

3. **Config snapshot** — how much of `init.el` to include in the prompt.
   Full file risks being too large; need a way to extract only relevant
   sections (keybindings, navigation-related packages).
