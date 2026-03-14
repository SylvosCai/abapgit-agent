---
layout: default
title: Claude Code Instructions (Template)
nav_order: 1
parent: ABAP Development
---

# Claude Code Instructions - Template

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

### 2. Read `.abapGitAgent` for Folder Location and Naming Conventions

**Before creating ANY ABAP object file, you MUST read `.abapGitAgent` to determine the correct folder.**

```
❌ WRONG: Assume files go in "abap/" folder
✅ CORRECT: Read .abapGitAgent to get the "folder" property value
```

The folder is configured in `.abapGitAgent` (property: `folder`):
- If `folder` is `/src/` → files go in `src/` (e.g., `src/zcl_my_class.clas.abap`)
- If `folder` is `/abap/` → files go in `abap/` (e.g., `abap/zcl_my_class.clas.abap`)

**Also check naming conventions before creating any new object:**

```
1. Check guidelines/objects.local.md  ← project-specific overrides (if file exists)
2. Fall back to guidelines/objects.md ← default Z/Y prefix conventions
```

`objects.local.md` is created by `abapgit-agent init` and is never overwritten by updates — it holds project-specific prefixes (e.g. `YCL_` instead of `ZCL_`).

---

### 3. Create XML Metadata / Local Classes

Each ABAP object needs an XML metadata file. Local helper/test-double classes use separate `.locals_def.abap` / `.locals_imp.abap` files.
→ See `guidelines/object-creation.md` for XML templates and local class setup

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

### 5. Local Helper / Test-Double Classes

→ See `guidelines/object-creation.md` for local class setup (`locals_def.abap` / `locals_imp.abap`)

---

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

### 7. CDS Unit Tests

Use `CL_CDS_TEST_ENVIRONMENT` for unit tests that read CDS views.
→ See `guidelines/cds-testing.md` for code examples

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

### 9. Never Run `run` Command Proactively

**Never call `abapgit-agent run` unless the user explicitly asks.** A class implementing `IF_OO_ADT_CLASSRUN` can modify data, send emails, or trigger RFCs — running it automatically is unsafe.

After activating a class, stop and tell the user: `"Class is activated. Run with: abapgit-agent run --class ZCL_MY_CLASS"`

---

### 10. Probe Classes — Use `scratchWorkspace` When Required

By default, probe/throwaway classes may be created in the current project. When `disableProbeClasses: true` is set in `.abapgit-agent.json`, they must go to `scratchWorkspace` instead. If `scratchWorkspace` is also not configured, refuse and guide the user to set it up.

→ See `guidelines/run-probe-classes.md` for naming conventions, full workflow, and refusal guidance

---

### 11. Troubleshooting ABAP Issues

| Symptom | Tool | When |
|---|---|---|
| HTTP 500 / runtime crash (ST22) | `dump` | Error already occurred |
| Wrong output, no crash | `debug` | Need to trace logic |

→ See `guidelines/debug-dump.md` for dump analysis workflow

**Critical rules for `debug` sessions:**

1. **Always use `--json`** for all debug commands (`attach`, `vars`, `stack`, `step`) — human output is not machine-parseable
2. **Attach BEFORE trigger** — start `debug attach --json` in background first, wait for `"Listener active"`, THEN fire the trigger (`unit`/`pull`/`run`)
3. **Never pull to trigger** if a simpler trigger works — use `unit` when a test exists, `run` for a class runner; use `pull` only when the bug is specifically in the pull flow
4. **Never pass `--session`** to `step/vars/stack` — it bypasses the daemon and causes errors
5. **Always finish with `step --type continue --json`** — releases the frozen ABAP work process

Minimal correct sequence:
```bash
abapgit-agent debug set --objects ZCL_FOO:42        # 1. set breakpoint
abapgit-agent debug attach --json > /tmp/a.json 2>&1 &   # 2. attach (background)
until grep -q "Listener active" /tmp/a.json 2>/dev/null; do sleep 0.3; done
abapgit-agent unit --files src/zcl_foo.clas.testclasses.abap > /tmp/t.json 2>&1 &  # 3. trigger
# poll for session, then inspect
abapgit-agent debug vars --json
abapgit-agent debug step --type continue --json     # 4. release
```

→ See `guidelines/debug-session.md` for full debug session guide, breakpoint tips, and pull flow architecture

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

Always work on feature branches. Before every `pull`: rebase to default branch. On completion: create PR with squash merge.
→ See `guidelines/branch-workflow.md` for step-by-step commands and examples

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

**When `safeguards.disableRun = true`:**
1. ✗ Do not run `abapgit-agent run` at all
2. ✓ Inform the user that run is disabled for this project

**When `safeguards.disableProbeClasses = true`:**
1. ✗ Do not create probe classes in the current project — see Rule 10 and `guidelines/run-command.md`

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

### Quick Decision Tree for AI

**When user asks to modify/create ABAP code:**

```
Modified ABAP files?
├─ CLAS/INTF/PROG/DDLS files?
│  ├─ Independent files (no cross-dependencies)?
│  │  └─ ✅ Use: syntax → commit → push → pull
│  └─ Dependent files (interface + class, class uses class)?
│     └─ ✅ Use: skip syntax → commit → push → pull
└─ Other types (DDLS, FUGR, TABL, etc.)?
   └─ ✅ Use: skip syntax → commit → push → pull → (if errors: inspect)
```

→ See `guidelines/workflow-detailed.md` for full workflow decision tree, error indicators, and complete command reference

---

## Guidelines Index

Detailed guidelines are available in the `guidelines/` folder:

| File | Topic |
|------|-------|
| `guidelines/index.md` | Overview and usage |
| `guidelines/sql.md` | ABAP SQL Best Practices |
| `guidelines/exceptions.md` | Exception Handling |
| `guidelines/testing.md` | Unit Testing (including CDS) |
| `guidelines/cds.md` | CDS Views |
| `guidelines/classes.md` | ABAP Classes and Objects |
| `guidelines/objects.md` | Object Naming Conventions (defaults) |
| `guidelines/objects.local.md` | **Project** Naming Conventions — overrides `objects.md` (created by `init`, never overwritten) |
| `guidelines/json.md` | JSON Handling |
| `guidelines/abapgit.md` | abapGit XML Metadata Templates |
| `guidelines/unit-testable-code.md` | Unit Testable Code Guidelines (Dependency Injection) |
| `guidelines/common-errors.md` | Common ABAP Errors - Quick Fixes |
| `guidelines/debug-session.md` | Debug Session Guide |
| `guidelines/debug-dump.md` | Dump Analysis Guide |
| `guidelines/run-probe-classes.md` | run Command — AI Guidelines (probe classes, scratchWorkspace) |
| `guidelines/branch-workflow.md` | Branch Workflow |
| `guidelines/workflow-detailed.md` | Development Workflow (Detailed) |
| `guidelines/object-creation.md` | Object Creation (XML metadata, local classes) |
| `guidelines/cds-testing.md` | CDS Testing (Test Double Framework) |

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
