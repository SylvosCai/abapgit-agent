---
layout: default
title: ABAP Project Guidelines
nav_order: 1
parent: ABAP Development
---

# ABAP Project Guidelines - Template

This file provides guidelines for **generating ABAP code** in abapGit repositories.

**Use this file as a template**: Copy it to your ABAP repository root when setting up new projects with Claude Code.

---

## Critical Rules

### 1. Use `ref` Command for Unfamiliar Topics

**When starting to work on ANY unfamiliar ABAP topic, syntax, or pattern, you MUST use the `ref` command BEFORE writing any code.**

```
❌ WRONG: Start writing code immediately based on assumptions
✅ CORRECT: Run ref command first to look up the correct pattern
```

**Why**: ABAP syntax is strict. Guessing leads to activation errors that waste time.

| Scenario | Example |
|----------|---------|
| Implementing new ABAP feature | "How do I use FILTER operator?" |
| Unfamiliar pattern | "What's the correct VALUE #() syntax?" |
| SQL operations | "How to write a proper SELECT with JOIN?" |
| CDS views | "How to define CDS view with associations?" |
| Getting syntax errors | Check reference before trying approaches |

```bash
# For CDS topics
abapgit-agent ref --topic cds
abapgit-agent ref "CDS view"
abapgit-agent ref "association"
```

```bash
# Search for a pattern
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"

# Browse by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql

# List all topics
abapgit-agent ref --list-topics
```

### 2. Read `.abapGitAgent` for Folder Location

**Before creating ANY ABAP object file, you MUST read `.abapGitAgent` to determine the correct folder.**

```
❌ WRONG: Assume files go in "abap/" folder
✅ CORRECT: Read .abapGitAgent to get the "folder" property value
```

The folder is configured in `.abapGitAgent` (property: `folder`):
- If `folder` is `/src/` → files go in `src/` (e.g., `src/zcl_my_class.clas.abap`)
- If `folder` is `/abap/` → files go in `abap/` (e.g., `abap/zcl_my_class.clas.abap`)

---

### 3. Create XML Metadata for Each ABAP Object

**Each ABAP object requires an XML metadata file for abapGit to understand how to handle it.**

| Object Type | ABAP File (if folder=/src/) | XML File | Details |
|-------------|------------------------------|----------|---------|
| Class | `src/zcl_*.clas.abap` | `src/zcl_*.clas.xml` | See `guidelines/08_abapgit.md` |
| Interface | `src/zif_*.intf.abap` | `src/zif_*.intf.xml` | See `guidelines/08_abapgit.md` |
| Program | `src/z*.prog.abap` | `src/z*.prog.xml` | See `guidelines/08_abapgit.md` |
| Table | `src/z*.tabl.abap` | `src/z*.tabl.xml` | See `guidelines/08_abapgit.md` |
| **CDS View Entity** | `src/zc_*.ddls.asddls` | `src/zc_*.ddls.xml` | **Use by default** - See `guidelines/04_cds.md` |
| CDS View (legacy) | `src/zc_*.ddls.asddls` | `src/zc_*.ddls.xml` | Only if explicitly requested - See `guidelines/04_cds.md` |

**IMPORTANT: When user says "create CDS view", create CDS View Entity by default.**

**Why:** Modern S/4HANA standard, simpler (no SQL view), no namespace conflicts.

**For complete XML templates, DDL examples, and detailed comparison:**
- **CDS Views**: `guidelines/04_cds.md`
- **XML templates**: `guidelines/08_abapgit.md`

---

### 4. Use Syntax Command Before Commit (for CLAS, INTF, PROG, DDLS)

```
❌ WRONG: Make changes → Commit → Push → Pull → Find errors → Fix → Repeat
✅ CORRECT: Make changes → Run syntax → Fix locally → Commit → Push → Pull → Done
```

**For CLAS, INTF, PROG, DDLS files**: Run `syntax` command BEFORE commit to catch errors early.

```bash
# Check syntax of local code (no commit/push needed)
abapgit-agent syntax --files src/zcl_my_class.clas.abap
abapgit-agent syntax --files src/zc_my_view.ddls.asddls

# Check multiple INDEPENDENT files
abapgit-agent syntax --files src/zcl_utils.clas.abap,src/zcl_logger.clas.abap
```

**For other types (DDLS, FUGR, TABL, etc.)**: Skip syntax, proceed to commit/push/pull.

**Why use syntax command?**
- Catches syntax errors BEFORE polluting git history with fix commits
- No broken inactive objects in ABAP system
- Faster feedback loop - fix locally without commit/push/pull cycle
- Works even for NEW objects that don't exist in ABAP system yet

**⚠️ Important: Syntax checks files independently**

When checking multiple files, each is validated in isolation:
- ✅ **Use for**: Multiple independent files (bug fixes, unrelated changes)
- ❌ **Don't use for**: Files with dependencies (interface + implementing class)

**For dependent files, skip `syntax` and use `pull` instead:**
```bash
# ❌ BAD - Interface and implementing class (may show false errors)
abapgit-agent syntax --files src/zif_my_intf.intf.abap,src/zcl_my_class.clas.abap

# ✅ GOOD - Use pull instead for dependent files
git add . && git commit && git push
abapgit-agent pull --files src/zif_my_intf.intf.abap,src/zcl_my_class.clas.abap
```

**Note**: `inspect` still runs against ABAP system (requires pull first). Use `syntax` for pre-commit checking.

---

### 5. Local Classes (Test Doubles, Helpers)

When a class needs local helper classes or test doubles, use separate files:

| File | Purpose |
|------|---------|
| `zcl_xxx.clas.locals_def.abap` | Local class definitions |
| `zcl_xxx.clas.locals_imp.abap` | Local class implementations |

**XML Configuration**: Add `<CLSCCINCL>X</CLSCCINCL>` to the class XML to include local class definitions:

```xml
<VSEOCLASS>
  <CLSNAME>ZCL_XXX</CLSNAME>
  ...
  <CLSCCINCL>X</CLSCCINCL>
</VSEOCLASS>
```

### 6. Use `ref`, `view` and `where` Commands to Learn About Unknown Classes/Methods

**When working with unfamiliar ABAP classes or methods, follow this priority:**

```
1. First: Check local git repo for usage examples
2. Second: Check ABAP reference/cheat sheets
3. Third: Use view/where commands to query ABAP system (if needed)
```

#### Priority 1: Check Local Git Repository

**Look for usage examples in your local ABAP project first:**
- Search for class/interface names in your codebase
- Check how similar classes are implemented
- This gives the most relevant context for your project

#### Priority 2: Check ABAP References

```bash
# Search in ABAP cheat sheets and guidelines
abapgit-agent ref "CLASS"
abapgit-agent ref "INTERFACE"
abapgit-agent ref --topic classes
```

#### Priority 3: Use `where` and `view` Commands (Query ABAP System)

**If local/references don't have the answer, query the ABAP system:**

```bash
# Find where a class/interface is USED (where command)
abapgit-agent where --objects ZIF_UNKNOWN_INTERFACE

# With pagination (default limit: 50, offset: 0)
abapgit-agent where --objects ZIF_UNKNOWN_INTERFACE --limit 20
abapgit-agent where --objects ZIF_UNKNOWN_INTERFACE --offset 50 --limit 20

# View CLASS DEFINITION (view command)
abapgit-agent view --objects ZCL_UNKNOWN_CLASS

# View specific METHOD implementation
abapgit-agent view --objects ZCL_UNKNOWN_CLASS=============CM001
```

**Example workflow for AI:**
```
User: "How do I use ZCL_ABGAGT_AGENT?"

AI thought process:
1. Search local repo for ZCL_ABGAGT_AGENT usage
2. Found: It's instantiated in several places with ->pull() method
3. Still unclear about parameters? Check view command
4. View: abapgit-agent view --objects ZCL_ABGAGT_AGENT
```

**Key differences:**
- `where`: Shows WHERE an object is USED (references)
- `view`: Shows what an object DEFINES (structure, methods, source)

---

### 7. Use CDS Test Double Framework for CDS View Tests

**When creating unit tests for CDS views, use the CDS Test Double Framework (`CL_CDS_TEST_ENVIRONMENT`).**

```
❌ WRONG: Use regular AUnit test class without test doubles
✅ CORRECT: Use CL_CDS_TEST_ENVIRONMENT to create test doubles for CDS views
```

**Why**: CDS views read from database tables. Using test doubles allows:
- Injecting test data without affecting production data
- Testing specific scenarios that may not exist in production
- Fast, isolated tests that don't depend on database state

See `guidelines/03_testing.md` for code examples.

---

### 8. Use `unit` Command for Unit Tests

**Use `abapgit-agent unit` to run ABAP unit tests (AUnit).**

```
❌ WRONG: Try to use SE24, SE37, or other transaction codes
✅ CORRECT: Use abapgit-agent unit --files src/zcl_test.clas.testclasses.abap
```

```bash
# Run unit tests (after pulling to ABAP)
abapgit-agent unit --files src/zcl_test.clas.testclasses.abap

# Multiple test classes
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap
```

---

### 9. Troubleshooting ABAP Issues

Two commands are available for investigating bugs at runtime:

| Command | Use when | What it gives you |
|---------|----------|-------------------|
| `dump`  | Error already occurred (ST22 crash) | Error type, call stack, exact source line |
| `debug` | Need to trace logic step-by-step | Live variable values, step into/over, expand structures |

#### When Something Goes Wrong — Start with `dump`

**First reflex** for any HTTP 500, runtime error, or user-reported crash:

```bash
abapgit-agent dump --date TODAY           # list today's dumps
abapgit-agent dump --date TODAY --detail 1  # full detail: call stack + source
```

The `--detail` output shows the exact failing line (`>>>>>` marker), call stack,
and SAP's error analysis. Use it before asking the user to open ST22.

Common filters:
```bash
abapgit-agent dump --user DEVELOPER --date TODAY        # specific user
abapgit-agent dump --error TIME_OUT                     # specific error type
abapgit-agent dump --program ZMY_PROGRAM --detail 1     # specific program, full detail
```

After identifying the failing class/method, use `view` for broader context:
```bash
abapgit-agent view --objects ZCL_MY_CLASS
```

#### When There Is No Dump — Use `debug`

Use `debug` when:
- The bug is a logic error (wrong output, no crash)
- You need to inspect variable values mid-execution
- You want to verify which branch of code runs

**Step 1 — set a breakpoint** on the first executable statement you want to inspect:
```bash
abapgit-agent debug set --files src/zcl_my_class.clas.abap:42   # from local file
abapgit-agent debug set --objects ZCL_MY_CLASS:42                # by name (no local file needed)
abapgit-agent debug list    # confirm it was registered
```

> **Line number must point to an executable statement** — not a comment, blank line,
> `DATA` declaration, or `METHOD`/`ENDMETHOD`. Use `view --objects` to find valid lines.

**Step 2 — attach and trigger**

Two modes depending on context:

*Interactive (human in a terminal):*
```bash
# Terminal 1 — attach (blocks waiting for breakpoint, opens REPL on hit)
abapgit-agent debug attach

# Terminal 2 — trigger (any command that calls the backend)
abapgit-agent unit --files src/zcl_my_class.clas.testclasses.abap
abapgit-agent inspect --files src/zcl_my_class.clas.abap
```

*Scripted (AI / automation) — best practice: individual sequential calls:*

Once the daemon is running and the session is saved to the state file, each
`vars/stack/step` command is a plain standalone call — no bash script needed.

```bash
# Step 1: start attach listener in background (spawns a daemon, saves session to state file)
abapgit-agent debug attach --json > /tmp/attach.json 2>&1 &
sleep 2

# Step 2: trigger in background — MUST stay alive for the whole session
abapgit-agent unit --files src/zcl_my_class.clas.testclasses.abap > /tmp/trigger.json 2>&1 &

# Step 3: poll until breakpoint fires and session JSON appears in attach output
SESSION=""
for i in $(seq 1 30); do
  sleep 0.5
  SESSION=$(grep -o '"session":"[^"]*"' /tmp/attach.json 2>/dev/null | head -1 | cut -d'"' -f4)
  [ -n "$SESSION" ] && break
done

# Step 4: inspect and step — each is an individual call, no --session needed
abapgit-agent debug stack --json
abapgit-agent debug vars  --json
abapgit-agent debug vars  --expand LS_OBJECT --json
abapgit-agent debug step  --type over --json
abapgit-agent debug vars  --json

# Step 5: ALWAYS release the ABAP work process before finishing
abapgit-agent debug step --type continue --json

# Step 6: check trigger result
cat /tmp/trigger.json
rm -f /tmp/attach.json /tmp/trigger.json
```

> **Four rules for scripted mode:**
> 1. `sleep 2` after starting attach — the listener must register on the server before the trigger fires
> 2. Keep the trigger process alive in the background for the entire session — if it exits, the ABAP work process is released and the session ends
> 3. Always finish with `step --type continue` — this releases the frozen work process so the trigger can complete normally
> 4. **Never pass `--session` to `step/vars/stack`** — it bypasses the daemon IPC and causes `noSessionAttached`. Omit it and let commands auto-load from the saved state file.

**Step 3 — step through and inspect**

*Interactive REPL commands (after `attach` without `--json`):*
```
  debug> v          — show all variables
  debug> x LT_DATA  — expand a table or structure
  debug> n          — step over
  debug> s          — step into
  debug> bt         — call stack
  debug> q          — detach (program continues); kill — hard abort
```

*Scripted commands (after `attach --json`) — omit `--session`, commands auto-load from state file:*
```bash
abapgit-agent debug vars   --json                        # all variables
abapgit-agent debug vars   --name LV_RESULT --json       # one variable
abapgit-agent debug vars   --expand LT_DATA --json       # drill into table/structure
abapgit-agent debug step   --type over --json            # step over
abapgit-agent debug step   --type into --json            # step into
abapgit-agent debug step   --type continue --json        # continue to next breakpoint / finish
abapgit-agent debug stack  --json                        # call stack (shows which test method is active)
```

**Clean up** when done:
```bash
abapgit-agent debug delete --all
```

---

## Development Workflow

This project's workflow mode is configured in `.abapGitAgent` under `workflow.mode`.

### Project-Level Config (`.abapgit-agent.json`)

Checked into the repository — applies to all developers. **Read this file at the start of every session.**

| Setting | Values | Default | Effect |
|---------|--------|---------|--------|
| `safeguards.requireFilesForPull` | `true`/`false` | `false` | Requires `--files` on every pull |
| `safeguards.disablePull` | `true`/`false` | `false` | Disables pull entirely (CI/CD-only projects) |
| `conflictDetection.mode` | `"abort"`/`"ignore"` | `"abort"` | Whether to abort pull on conflict |
| `transports.hook.path` | string | `null` | Path to JS module that auto-selects a transport for pull |
| `transports.hook.description` | string | `null` | Optional label shown when the hook runs |
| `transports.allowCreate` | `true`/`false` | `true` | When `false`, `transport create` is blocked |
| `transports.allowRelease` | `true`/`false` | `true` | When `false`, `transport release` is blocked |

CLI `--conflict-mode` always overrides the project config for a single run.

See **AI Tool Guidelines** below for how to react to each setting.

### Workflow Modes

| Mode | Branch Strategy | Rebase Before Pull | Create PR |
|------|----------------|-------------------|-----------|
| `"branch"` | Feature branches | ✓ Always | ✓ Yes (squash merge) |
| `"trunk"` | Direct to default branch | ✗ No | ✗ No |
| (not set) | Direct to default branch | ✗ No | ✗ No |

**Default branch** (main/master/develop) is **auto-detected** from your git repository.

### Branch Workflow (`"mode": "branch"`)

**IMPORTANT**: Always work on feature branches, never commit directly to the default branch.

#### Starting a New Feature

```bash
# 1. Create and switch to feature branch from default branch
git checkout main  # or master/develop (auto-detected)
git pull origin main
git checkout -b feature/user-authentication

# 2. Make your changes
edit src/zcl_auth_handler.clas.abap

# 3. Check syntax (CLAS/INTF/PROG/DDLS only, if independent)
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap

# 4. Commit
git add src/zcl_auth_handler.clas.abap
git commit -m "feat: add authentication handler"

# 5. Push feature branch
git push origin feature/user-authentication

# 6. **CRITICAL**: Rebase before pull
git fetch origin main
git rebase origin/main
git push origin feature/user-authentication --force-with-lease

# 7. Pull to ABAP system
abapgit-agent pull --files src/zcl_auth_handler.clas.abap
```

#### During Development: Always Rebase Before Pull

**CRITICAL**: Before every `pull` command, rebase to default branch to avoid activating outdated code.

```bash
# Before EVERY pull, always do this:
git fetch origin main  # main/master/develop (auto-detected)
git rebase origin/main

# If no conflicts:
git push origin feature/user-authentication --force-with-lease
abapgit-agent pull --files src/zcl_auth_handler.clas.abap

# If conflicts:
# 1. Fix conflicts in files
# 2. git add <resolved-files>
# 3. git rebase --continue
# 4. git push origin feature/user-authentication --force-with-lease
# 5. abapgit-agent pull --files ...
```

#### Completing the Feature

```bash
# 1. Final rebase and push
git fetch origin main
git rebase origin/main
git push origin feature/user-authentication --force-with-lease

# 2. Final activation and test
abapgit-agent pull --files src/zcl_auth_handler.clas.abap
abapgit-agent unit --files src/zcl_auth_handler.clas.testclasses.abap

# 3. Create PR (squash merge enabled on GitHub/GitLab)
# Go to GitHub and create PR from feature/user-authentication to main
# Select "Squash and merge" option to combine all commits into one
```

#### Why Rebase Before Pull?

ABAP is a **centralized system**. Multiple developers may modify the same files:

| Without Rebase | With Rebase |
|----------------|-------------|
| ✗ Your branch is based on old main | ✓ Your branch includes latest changes |
| ✗ Activate outdated code in ABAP | ✓ Activate current code |
| ✗ May overwrite others' work | ✓ Conflicts caught before activation |
| ✗ Hard to debug issues | ✓ Clear what changed |

**Example Scenario:**

```
Situation:
- You: working on feature/auth (based on main commit A)
- Colleague: pushed to main (now at commit B)
- Both modified: src/zcl_auth_handler.clas.abap

Without rebase:
  feature/auth pull → activates version from commit A ✗

With rebase:
  git rebase origin/main → either:
    - No conflict: includes colleague's changes ✓
    - Conflict: you see it and resolve ✓
```

#### Complete Example Workflow (Branch Mode)

```bash
# Day 1: Start feature
git checkout main && git pull origin main
git checkout -b feature/user-authentication
edit src/zcl_auth_handler.clas.abap
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap
git add . && git commit -m "wip: add basic auth logic"
git push origin feature/user-authentication
git fetch origin main && git rebase origin/main
git push origin feature/user-authentication --force-with-lease
abapgit-agent pull --files src/zcl_auth_handler.clas.abap

# Day 2: Continue (colleague pushed to main overnight)
git fetch origin main && git rebase origin/main
# If conflicts: resolve, git add, git rebase --continue
git push origin feature/user-authentication --force-with-lease
edit src/zcl_auth_handler.clas.abap
git add . && git commit -m "feat: complete auth logic"
git push origin feature/user-authentication
git fetch origin main && git rebase origin/main
git push origin feature/user-authentication --force-with-lease
abapgit-agent pull --files src/zcl_auth_handler.clas.abap

# Day 3: Finish feature
abapgit-agent unit --files src/zcl_auth_handler.clas.testclasses.abap
git fetch origin main && git rebase origin/main
git push origin feature/user-authentication --force-with-lease
# Create PR on GitHub/GitLab (squash 3 commits into 1)
```

### Trunk Workflow (`"mode": "trunk"`)

If workflow mode is `"trunk"` or not set, commit directly to the default branch:

```bash
git checkout main  # or master/develop (auto-detected)
git pull origin main
edit src/zcl_auth_handler.clas.abap
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap
git add . && git commit -m "feat: add authentication handler"
git push origin main
abapgit-agent pull --files src/zcl_auth_handler.clas.abap
```

### AI Tool Guidelines

**Read `.abapGitAgent` to determine workflow mode:**

**When `workflow.mode = "branch"`:**
1. ✓ Auto-detect default branch (main/master/develop)
2. ✓ Create feature branches (naming: `feature/description`)
3. ✓ Always `git fetch origin <default> && git rebase origin/<default>` before `pull` command
4. ✓ Use `--force-with-lease` after rebase (never `--force`)
5. ✓ Create PR with squash merge when feature complete
6. ✗ Never commit directly to default branch
7. ✗ Never use `git push --force` (always use `--force-with-lease`)

**When `workflow.mode = "trunk"` or not set:**
1. ✓ Commit directly to default branch
2. ✓ Keep commits clean and atomic
3. ✓ `git pull origin <default>` before push
4. ✗ Don't create feature branches

**Read `.abapgit-agent.json` to determine project safeguards and conflict detection:**

**When `safeguards.requireFilesForPull = true`:**
1. ✓ Always include `--files` in every `pull` command
2. ✓ Never run `abapgit-agent pull` without `--files`
3. ✗ Don't suggest or run a full pull without specifying files

**When `safeguards.requireFilesForPull = false` or not set:**
1. ✓ `--files` is optional — use it for speed, omit for full pull

**When `safeguards.disablePull = true`:**
1. ✗ Do not run `abapgit-agent pull` at all
2. ✓ Inform the user that pull is disabled for this project (CI/CD only)

**When `conflictDetection.mode = "ignore"` or not set:**
1. ✓ Run `pull` normally — no conflict flags needed
2. ✗ Don't add `--conflict-mode` unless user explicitly asks

**When `conflictDetection.mode = "abort"`:**
1. ✓ Conflict detection is active — pull aborts if ABAP system was edited since last pull
2. ✓ If pull is aborted with conflict error, inform user and suggest `--conflict-mode ignore` to override for that run
3. ✗ Don't silently add `--conflict-mode ignore` — always tell the user about the conflict

**When `transports.allowCreate = false`:**
1. ✗ Do not run `abapgit-agent transport create`
2. ✓ Inform the user that transport creation is disabled for this project

**When `transports.allowRelease = false`:**
1. ✗ Do not run `abapgit-agent transport release`
2. ✓ Inform the user that transport release is disabled for this project

---

## Development Workflow (Detailed)

```
1. Read .abapGitAgent → get folder value AND workflow.mode
       │
       ▼
2. Research → use ref command for unfamiliar topics
       │
       ▼
3. Write code → place in correct folder (e.g., src/zcl_*.clas.abap)
       │
       ▼
4. Syntax check (for CLAS, INTF, PROG, DDLS only)
       │
       ├─► CLAS/INTF/PROG/DDLS → abapgit-agent syntax --files <file>
       │       │
       │       ├─► Errors? → Fix locally (no commit needed), re-run syntax
       │       │
       │       └─► Clean ✅ → Proceed to commit
       │
       └─► Other types (FUGR, TABL, etc.) → Skip syntax, go to commit
               │
               ▼
5. Commit and push → git add . && git commit && git push
       │
       ▼
6. Activate → abapgit-agent pull --files src/file.clas.abap
       │         (behaviour depends on .abapgit-agent.json — see AI Tool Guidelines)
       │
       ▼
7. Verify → Check pull output
   - **"Error updating where-used list"** → SYNTAX ERROR (use inspect for details)
   - Objects in "Failed Objects Log" → Syntax error (use inspect)
   - Objects NOT appearing at all → XML metadata issue (check 08_abapgit.md)
       │
       ▼
8. (Optional) Run unit tests → abapgit-agent unit --files <testclass> (AFTER successful pull)
```

**Syntax Command - Supported Object Types:**

| Object Type | Syntax Command | What to Do |
|-------------|----------------|------------|
| CLAS (classes) | ✅ Supported | Run `syntax` before commit |
| CLAS (test classes: .testclasses.abap) | ✅ Supported | Run `syntax` before commit |
| INTF (interfaces) | ✅ Supported | Run `syntax` before commit |
| PROG (programs) | ✅ Supported | Run `syntax` before commit |
| DDLS (CDS views) | ✅ Supported | Run `syntax` before commit (requires annotations) |
| FUGR (function groups) | ❌ Not supported | Skip syntax, use `pull` then `inspect` |
| TABL/DTEL/DOMA/MSAG/SHLP | ❌ Not supported | Skip syntax, just `pull` |
| All other types | ❌ Not supported | Skip syntax, just `pull` |

**IMPORTANT**:
- **Use `syntax` BEFORE commit** for CLAS/INTF/PROG/DDLS - catches errors early, no git pollution
- **Syntax checks files INDEPENDENTLY** - syntax checker doesn't have access to uncommitted files
- **For dependent files** (interface + class): Create/activate underlying object FIRST, then dependent object (see workflow below)
- **DDLS requires proper annotations** - CDS views need `@AbapCatalog.sqlViewName`, view entities don't
- **ALWAYS push to git BEFORE running pull** - abapGit reads from git
- **Use `inspect` AFTER pull** for unsupported types or if pull fails

**Working with dependent objects (RECOMMENDED APPROACH):**

When creating objects with dependencies (e.g., interface → class), create and activate the underlying object FIRST:

```bash
# Step 1: Create interface, syntax check, commit, activate
vim src/zif_my_interface.intf.abap
abapgit-agent syntax --files src/zif_my_interface.intf.abap  # ✅ Works (no dependencies)
git add src/zif_my_interface.intf.abap src/zif_my_interface.intf.xml
git commit -m "feat: add interface"
git push
abapgit-agent pull --files src/zif_my_interface.intf.abap   # Interface now activated

# Step 2: Create class, syntax check, commit, activate
vim src/zcl_my_class.clas.abap
abapgit-agent syntax --files src/zcl_my_class.clas.abap     # ✅ Works (interface already activated)
git add src/zcl_my_class.clas.abap src/zcl_my_class.clas.xml
git commit -m "feat: add class implementing interface"
git push
abapgit-agent pull --files src/zcl_my_class.clas.abap
```

**Benefits:**
- ✅ Syntax checking works for both objects
- ✅ Each step is validated independently
- ✅ Easier to debug if something fails
- ✅ Cleaner workflow

**Alternative approach (when interface design is uncertain):**

If the interface might need changes while implementing the class, commit both together:

```bash
# Create both files
vim src/zif_my_interface.intf.abap
vim src/zcl_my_class.clas.abap

# Skip syntax (files depend on each other), commit together
git add src/zif_my_interface.intf.abap src/zif_my_interface.intf.xml
git add src/zcl_my_class.clas.abap src/zcl_my_class.clas.xml
git commit -m "feat: add interface and implementing class"
git push

# Pull both together
abapgit-agent pull --files src/zif_my_interface.intf.abap,src/zcl_my_class.clas.abap

# Use inspect if errors occur
abapgit-agent inspect --files src/zcl_my_class.clas.abap
```

**Use this approach when:**
- ❌ Interface design is still evolving
- ❌ Multiple iterations expected

**Working with mixed file types:**
When modifying multiple files of different types (e.g., 1 class + 1 CDS view):
1. Run `syntax` on independent supported files (CLAS, INTF, PROG, DDLS)
2. Commit ALL files together (including unsupported types)
3. Push and pull ALL files together

Example:
```bash
# Check syntax on independent files only
abapgit-agent syntax --files src/zcl_my_class.clas.abap,src/zc_my_view.ddls.asddls

# Commit and push all files
git add src/zcl_my_class.clas.abap src/zc_my_view.ddls.asddls
git commit -m "feat: add class and CDS view"
git push

# Pull all files together
abapgit-agent pull --files src/zcl_my_class.clas.abap,src/zc_my_view.ddls.asddls
```

**When to use syntax vs inspect vs view**:
- **syntax**: Check LOCAL code BEFORE commit (CLAS, INTF, PROG, DDLS)
- **inspect**: Check ACTIVATED code AFTER pull (all types, runs Code Inspector)
- **view**: Understand object STRUCTURE (not for debugging errors)

### Quick Decision Tree for AI

**When user asks to modify/create ABAP code:**

```
1. Identify file extension(s) AND dependencies
   ├─ .clas.abap or .clas.testclasses.abap → CLAS ✅ syntax supported
   ├─ .intf.abap → INTF ✅ syntax supported
   ├─ .prog.abap → PROG ✅ syntax supported
   ├─ .ddls.asddls → DDLS ✅ syntax supported (requires proper annotations)
   └─ All other extensions → ❌ syntax not supported

2. Check for dependencies:
   ├─ Interface + implementing class? → DEPENDENT (interface is underlying)
   ├─ Class A uses class B? → DEPENDENT (class B is underlying)
   ├─ CDS view uses table? → INDEPENDENT (table already exists)
   └─ Unrelated bug fixes across files? → INDEPENDENT

3. For SUPPORTED types (CLAS/INTF/PROG/DDLS):
   ├─ INDEPENDENT files → Run syntax → Fix errors → Commit → Push → Pull
   │
   └─ DEPENDENT files (NEW objects):
       ├─ RECOMMENDED: Create underlying object first (interface, base class, etc.)
       │   1. Create underlying object → Syntax → Commit → Push → Pull
       │   2. Create dependent object → Syntax (works!) → Commit → Push → Pull
       │   ✅ Benefits: Both syntax checks work, cleaner workflow
       │
       └─ ALTERNATIVE: If interface design uncertain, commit both together
           → Skip syntax → Commit both → Push → Pull → (if errors: inspect)

4. For UNSUPPORTED types (FUGR, TABL, etc.):
   Write code → Skip syntax → Commit → Push → Pull → (if errors: inspect)

5. For MIXED types (some supported + some unsupported):
   Write all code → Run syntax on independent supported files ONLY → Commit ALL → Push → Pull ALL
```

**Example workflows:**

**Scenario 1: Interface + Class (RECOMMENDED)**
```bash
# Step 1: Interface first
vim src/zif_calculator.intf.abap
abapgit-agent syntax --files src/zif_calculator.intf.abap  # ✅ Works
git commit -am "feat: add calculator interface" && git push
abapgit-agent pull --files src/zif_calculator.intf.abap    # Interface activated

# Step 2: Class next
vim src/zcl_calculator.clas.abap
abapgit-agent syntax --files src/zcl_calculator.clas.abap  # ✅ Works (interface exists!)
git commit -am "feat: implement calculator" && git push
abapgit-agent pull --files src/zcl_calculator.clas.abap
```

**Scenario 2: Multiple independent classes**
```bash
# All syntax checks work (no dependencies)
vim src/zcl_class1.clas.abap src/zcl_class2.clas.abap
abapgit-agent syntax --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap
git commit -am "feat: add utility classes" && git push
abapgit-agent pull --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap
```

**Error indicators after pull:**
- ❌ **"Error updating where-used list"** → SYNTAX ERROR - run `inspect` for details
- ❌ **Objects in "Failed Objects Log"** → SYNTAX ERROR - run `inspect`
- ❌ **Objects NOT appearing at all** → XML metadata issue (check `ref --topic abapgit`)
- ⚠️ **"Activated with warnings"** → Code Inspector warnings - run `inspect` to see details

### Commands

```bash
# 1. Syntax check LOCAL code BEFORE commit (CLAS, INTF, PROG, DDLS)
abapgit-agent syntax --files src/zcl_my_class.clas.abap
abapgit-agent syntax --files src/zc_my_view.ddls.asddls
abapgit-agent syntax --files src/zcl_class1.clas.abap,src/zif_intf1.intf.abap,src/zc_view.ddls.asddls

# 2. Pull/activate AFTER pushing to git
abapgit-agent pull --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap

# Override conflict detection for a single pull (e.g. deliberate branch switch)
abapgit-agent pull --files src/zcl_class1.clas.abap --conflict-mode ignore

# 3. Inspect AFTER pull (for errors or unsupported types)
abapgit-agent inspect --files src/zcl_class1.clas.abap

# Run unit tests (after successful pull)
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap

# View object definitions (multiple objects)
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE

# Preview table data (multiple tables/views)
abapgit-agent preview --objects ZTABLE1,ZTABLE2

# Explore table structures
abapgit-agent view --objects ZTABLE --type TABL

# Display package tree
abapgit-agent tree --package \$MY_PACKAGE

# Investigate runtime errors (ST22 short dumps)
abapgit-agent dump                                          # Last 7 days
abapgit-agent dump --user DEVELOPER --date TODAY           # Today's dumps for a user
abapgit-agent dump --program ZMY_PROGRAM                   # Dumps from a specific program
abapgit-agent dump --error TIME_OUT                        # Dumps by error type
abapgit-agent dump --user DEVELOPER --detail 1             # Full detail of first result
```

---

## Guidelines Index

Detailed guidelines are available in the `guidelines/` folder:

| File | Topic |
|------|-------|
| `guidelines/01_sql.md` | ABAP SQL Best Practices |
| `guidelines/02_exceptions.md` | Exception Handling |
| `guidelines/03_testing.md` | Unit Testing (including CDS) |
| `guidelines/04_cds.md` | CDS Views |
| `guidelines/05_classes.md` | ABAP Classes and Objects |
| `guidelines/06_objects.md` | Object Naming Conventions |
| `guidelines/07_json.md` | JSON Handling |
| `guidelines/08_abapgit.md` | abapGit XML Metadata Templates |

These guidelines are automatically searched by the `ref` command.

---

## Custom Guidelines

You can add your own guidelines:

1. Create `.md` files in `guidelines/` folder
2. Export to reference folder: `abapgit-agent ref export`
3. The `ref` command will search both cheat sheets and your custom guidelines

---

## For More Information

- [SAP ABAP Cheat Sheets](https://github.com/SAP-samples/abap-cheat-sheets)
- [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
