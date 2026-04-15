---
layout: default
title: guide - Show ABAP Development Guide
nav_order: 2
parent: Utility Commands
---

# guide Command

## Overview

Display the full ABAP development guide bundled with the abapgit-agent package.
The guide covers the complete development workflow, ABAP coding guidelines,
object naming conventions, unit testing patterns, and debugging guide.

This command reads directly from the installed npm package — always up-to-date
with the installed version, no manual copying or updating required.

## Command

```bash
# Print the full guide to stdout
abapgit-agent guide

# Print only the path to the guide file
abapgit-agent guide --path

# Output as JSON
abapgit-agent guide --json

# Migrate repo from locally-copied guidelines to bundled fallback
abapgit-agent guide --migrate

# Preview migration without making changes
abapgit-agent guide --migrate --dry-run

# Migrate without confirmation prompt
abapgit-agent guide --migrate --yes
```

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--path` | No | Print only the absolute path to the bundled `abap/CLAUDE.md` file |
| `--json` | No | Output as JSON `{ "path": "...", "content": "..." }` |
| `--migrate` | No | Migrate repo from locally-copied guidelines to bundled fallback |
| `--dry-run` | No | (with `--migrate`) Preview changes without applying them |
| `--yes` / `-y` | No | (with `--migrate`) Skip confirmation prompt |

---

## Output

### Default (print guide)

Prints the full content of the bundled `abap/CLAUDE.md` to stdout.

```
# Claude Code Instructions - Template

This repository uses abapgit-agent for ABAP development...

## Critical Rules
...
```

### `--path` (print file path only)

```bash
abapgit-agent guide --path
```

```
/usr/local/lib/node_modules/abapgit-agent/abap/CLAUDE.md
```

Useful for reading the file with a pager or passing to other tools:

```bash
# Read with pager
abapgit-agent guide | less

# Use path in another command
cat "$(abapgit-agent guide --path)"
```

### `--json` (machine-readable)

```bash
abapgit-agent guide --json
```

```json
{
  "path": "/usr/local/lib/node_modules/abapgit-agent/abap/CLAUDE.md",
  "content": "# Claude Code Instructions - Template\n\n..."
}
```

### `--migrate` (migrate to bundled fallback)

```bash
abapgit-agent guide --migrate
```

```
🔄 guide --migrate: switch to bundled guidelines

Files to remove (12 standard guideline files):
   guidelines/sql.md
   guidelines/classes.md
   guidelines/exceptions.md
   ... (9 more)

Files to keep (project-specific):
   guidelines/objects.local.md

CLAUDE.md: detected as full guide → will replace with slim stub
   (run 'abapgit-agent guide' to read the full guide on demand)

.github/copilot-instructions.md: detected as full guide → will replace with slim stub
   (Copilot uses the slim stub; full guide available online)

Proceed? [y/N]
```

After confirming:

```
🗑️  Removed guidelines/sql.md
🗑️  Removed guidelines/classes.md
...
✅ Replaced CLAUDE.md with slim stub
✅ Replaced .github/copilot-instructions.md with slim stub

✅ Migration complete.
   Standard guidelines are now read from the package automatically.
   Run 'abapgit-agent ref "<pattern>"' or 'abapgit-agent ref --topic <topic>' to search them.
```

#### Already clean

```bash
abapgit-agent guide --migrate
```

```
✅ Already clean — nothing to migrate.
   CLAUDE.md is already the slim stub.
   No standard guideline files found in guidelines/.
```

#### Dry run

```bash
abapgit-agent guide --migrate --dry-run
```

Prints the full preview (files to remove, CLAUDE.md action) but makes no changes:

```
🔄 guide --migrate: switch to bundled guidelines

Files to remove (3 standard guideline files):
   guidelines/sql.md
   guidelines/classes.md
   guidelines/objects.md

ℹ️  Dry run — no changes made.
```

---

## Migration: What Gets Removed

`abapgit-agent init` in older versions copied the 18 standard guideline files into the repo and the full `abap/CLAUDE.md`. These are no longer needed — the `ref` command now falls back to bundled copies automatically.

`--migrate` handles the transition:

| Item | Action |
|------|--------|
| `guidelines/*.md` matching a bundled filename | **Deleted** |
| `guidelines/objects.local.md` (and any `*.local.md`) | **Kept** — project-specific, never auto-removed |
| Other non-standard files in `guidelines/` | **Kept** |
| `guidelines/` directory | Removed only if completely empty after deletions |
| `CLAUDE.md` containing the full guide | **Replaced** with slim stub |
| `CLAUDE.md` already containing slim stub | Left untouched |
| `CLAUDE.md` with custom content (no known marker) | Left untouched, warning printed |
| `.github/copilot-instructions.md` containing the full guide | **Replaced** with slim stub |
| `.github/copilot-instructions.md` already containing slim stub | Left untouched |
| `.github/copilot-instructions.md` with custom content | Left untouched |

---

## Error Handling

| Error | Message |
|-------|---------|
| Guide file not found | `❌ Bundled CLAUDE.md not found. Make sure abapgit-agent is properly installed.` |

---

## Use Cases

- **Read the guide interactively**: `abapgit-agent guide | less`
- **Reference from CLAUDE.md stub**: the slim `CLAUDE.md` stub in your repo (created by `init`) directs Claude to run this command before writing ABAP code
- **Always up-to-date**: the guide is always read from the installed package — no need to run `init --update`
- **No ABAP connection required**: works completely offline
- **Migrate existing repos**: use `--migrate` to remove locally-copied guideline files from repos initialized with an older version

---

## Examples

```bash
# Print full guide
abapgit-agent guide

# Read with pager
abapgit-agent guide | less

# Get path for scripting
abapgit-agent guide --path

# Machine-readable output
abapgit-agent guide --json

# Preview migration (no changes)
abapgit-agent guide --migrate --dry-run

# Migrate interactively
abapgit-agent guide --migrate

# Migrate non-interactively (CI / scripting)
abapgit-agent guide --migrate --yes
```

---

## How Claude Uses This Command

When your repository has a slim `CLAUDE.md` stub (created by `abapgit-agent init`),
it instructs Claude to run `abapgit-agent guide` before writing any ABAP code.
This ensures Claude always follows the latest guidelines from the installed package,
without requiring stale copies in the repository.

### Workflow

```
New project setup:
  abapgit-agent init
    → Creates slim CLAUDE.md stub
    → Creates guidelines/objects.local.md template

Claude starts working:
  Reads CLAUDE.md stub
    → Runs: abapgit-agent guide
    → Reads full development guide
    → Follows latest workflow and syntax rules

Existing project (older init):
  abapgit-agent guide --migrate
    → Removes locally-copied standard guideline files
    → Replaces full CLAUDE.md with slim stub
    → ref command uses bundled guidelines automatically going forward
```

---

## Relationship to `ref` Command

| Command | Purpose |
|---------|---------|
| `abapgit-agent guide` | Full ABAP development guide (workflow, rules, debug) |
| `abapgit-agent ref --topic sql` | Syntax reference for a specific topic |
| `abapgit-agent ref "VALUE #("` | Search for a specific pattern |

Use `guide` to understand the **development workflow and rules**.
Use `ref` to look up **specific ABAP syntax and examples**.
