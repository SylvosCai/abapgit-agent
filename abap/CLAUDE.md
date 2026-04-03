---
layout: default
title: AI Agent Instructions
nav_order: 1
parent: ABAP Development
---

# AI Agent Instructions

This file provides guidelines for **generating ABAP code** in abapGit repositories.

---

## Critical Rules

### 1. Use `guide` and `ref` Commands for Unfamiliar Topics

**When starting a new session or working on ANY unfamiliar ABAP topic, syntax, or pattern:**

```
❌ WRONG: Start writing code immediately based on assumptions
✅ CORRECT: Run guide first, then ref for specific topics
```

**Step 1 — Read the full dev guide at session start:**
```bash
abapgit-agent guide
```
Covers the complete workflow, all critical rules, object naming, unit testing, debugging, and the full guidelines index.

**Step 2 — Look up specific syntax or topics with ref:**

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
- If `folder` is `/src/` → files go in `src/`
- If `folder` is `/abap/` → files go in `abap/`

**Also check naming conventions before creating any new object:**

```
1. Check guidelines/objects.local.md  ← this project's actual conventions (if file exists)
2. No objects.local.md?               ← customer namespace project, use Z/Y defaults
```

`objects.local.md` is never overwritten by updates. It specifies the production naming
pattern — which may be customer namespace (`ZCL_*`, `YCL_*`), SAP namespace (`CL_*`),
or SAP registered namespace (`/NAMESPACE/CL_*` — also SAP namespace, not customer).
Never assume Z/Y prefix without checking.

**SAP namespace vs customer namespace:**
- **Customer namespace**: `Z*`, `Y*` objects; `Z*`, `Y*`, `$*` packages — owned by the customer
- **SAP namespace**: everything else (`CL_*`, `IF_*`, `/NAMESPACE/*`) — delivered by SAP

---

### 3. Creating a New ABAP Object — Files to Write and Package Assignment

```
❌ WRONG: Only write the .abap source file
✅ CORRECT: Every object needs both a source file AND an XML metadata file
           XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG) need ONLY the XML file
```

**Before choosing a name, verify the length limit for the object type:**

| Object type | Name limit | Field/sub-element limit |
|---|---|---|
| CLAS, INTF | **30 chars** | method names: **30 chars** |
| TABL, STRU | **30 chars** | **field names: 16 chars** |
| DDLS (CDS view entity) | **40 chars** | field alias: **30 chars** |
| DCLS (CDS access control) | **40 chars** | — (name must match its DDLS view) |
| FUGR (function group) | **26 chars** | function module name: **30 chars** |
| MSAG (message class) | **20 chars** | — |
| PROG, DTEL, DOMA, TTYP | **30 chars** | — |

→ Full length reference: `abapgit-agent ref --topic naming-limits`

Use the object name from `objects.local.md` (or `objects.md` as fallback) in place of `<name>`:

| Object Type | Source File | XML File |
|-------------|-------------|----------|
| Class (CLAS) | `<name>.clas.abap` | `<name>.clas.xml` |
| Interface (INTF) | `<name>.intf.abap` | `<name>.intf.xml` |
| Program (PROG) | `<name>.prog.abap` | `<name>.prog.xml` |
| CDS View (DDLS) | `<name>.ddls.asddls` | `<name>.ddls.xml` |
| CDS Access Control (DCLS) | `<name>.dcls.asdcls` | `<name>.dcls.xml` |
| Function Group (FUGR) | `<name>.fugr.abap` + includes | `<name>.fugr.xml` |
| Table (TABL) | *(none)* | `<name>.tabl.xml` |
| Structure (STRU) | *(none)* | `<name>.tabl.xml` ⚠️ NOT `.stru.xml` |
| Data Element (DTEL) | *(none)* | `<name>.dtel.xml` |
| Table Type (TTYP) | *(none)* | `<name>.ttyp.xml` |
| Domain (DOMA) | *(none)* | `<name>.doma.xml` |
| Message Class (MSAG) | *(none)* | `<name>.msag.xml` |

**Package assignment — determine the package, then follow the confirmation rule below:**

```
1. Check objects.local.md for package rules  ← use them directly
2. No package rules in objects.local.md?
   └── Read .abapGitAgent → get the package property → use as root
       Run: abapgit-agent tree --package <root>
       ├── Only one package found   →  use it directly
       └── Multiple packages found  →  present options, ask user to choose
3. No package in .abapGitAgent?
   └── Ask the user for the root package
```

**Confirmation before writing files — depends on namespace:**

```
Object name starts with Z* or Y*  AND  package starts with Z*, Y*, or $*?
  └── Customer namespace object in customer package
      → Write files directly. No confirmation needed.

Anything else (SAP namespace object, or SAP-delivered package)?
  └── Show a creation summary and wait for explicit confirmation:

      "I'm going to create the following:

        Object:   <NAME> (<Type>)
        Package:  <PACKAGE>
        Files:    <folder>/<name>.<ext>.abap    ← omit this line for XML-only types (TABL/STRU/DTEL/TTYP/DOMA/MSAG)
                  <folder>/<name>.<ext>.xml

      Shall I proceed?"
```

```
❌ WRONG: Write files without showing the summary for SAP namespace objects
❌ WRONG: Run abapgit-agent tree and pick a package yourself
✅ CORRECT: Customer namespace → write directly
✅ CORRECT: SAP namespace → always show summary, wait for confirmation
```

> **Tip for project setup**: Add package rules to `objects.local.md` so Claude never
> needs to ask. Run: `abapgit-agent ref --topic objects`

→ For exact XML templates: `abapgit-agent ref --topic abapgit` (CLAS/INTF/PROG/DDLS/DCLS/FUGR) or `abapgit-agent ref --topic abapgit-xml-only` (TABL/STRU/DTEL/TTYP/DOMA/MSAG)
→ For local helper/test-double class files: `abapgit-agent ref --topic object-creation`
→ For documentation comment format (shorttext, @parameter, CDS): `abapgit-agent ref --topic comments`

---

### 4. Use Syntax Command Before Commit (for CLAS, INTF, PROG, DDLS)

```
❌ WRONG: Make changes → Commit → Push → Pull → Find errors → Fix → Repeat
✅ CORRECT: Make changes → Run syntax → Fix locally → Commit → Push → Pull → Done
```

**For CLAS, INTF, PROG, DDLS files**: Run `syntax` command BEFORE commit to catch errors early.

```bash
# Check syntax of local code (no commit/push needed)
# Use the actual filename from your project (name comes from objects.local.md)
abapgit-agent syntax --files src/<name>.clas.abap
abapgit-agent syntax --files src/<name>.ddls.asddls

# Check multiple INDEPENDENT files
abapgit-agent syntax --files src/<name1>.clas.abap,src/<name2>.clas.abap
```

**For other types (FUGR, TABL, etc.)**: Skip syntax, proceed to commit/push/pull.

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
abapgit-agent syntax --files src/<intf_name>.intf.abap,src/<class_name>.clas.abap

# ✅ GOOD - Use pull instead for dependent files
git add . && git commit && git push
abapgit-agent pull --files src/<intf_name>.intf.abap,src/<class_name>.clas.abap
```

**Note**: `inspect` still runs against ABAP system (requires pull first). Use `syntax` for pre-commit checking.

---

### 5. Local Helper / Test-Double Classes

When a class needs local helper classes or test doubles, create separate include files alongside the main class file:

```
<name>.clas.locals_def.abap   ← local type/class definitions
<name>.clas.locals_imp.abap   ← local class implementations
<name>.clas.testclasses.abap  ← unit test classes (FOR TESTING)
```

**XML metadata when adding these files:**

```
Adding .clas.testclasses.abap to an existing class?
  └── Update the .clas.xml → set WITH_UNIT_TESTS flag:
        <clas:abapClassProperties ... abpUnitTestable="true" ... />
      WITHOUT this flag, abapGit will not push/activate the test include.

Adding .clas.locals_def.abap (local type definitions)?
  └── Update the .clas.xml → set CLSCCINCL flag:
        <CLSCCINCL>X</CLSCCINCL>
```

→ For exact XML flag placement and test double class patterns: `abapgit-agent ref --topic object-creation`
→ For exact XML flag placement: `abapgit-agent ref --topic abapgit` (search "WITH_UNIT_TESTS")

---

### 6. Use `guide`, `ref`, `view` and `where` Commands to Learn About Unknown Classes/Methods

**When working with unfamiliar ABAP classes or methods, follow this priority:**

```
1. First: Check local git repo for usage examples
2. Second: Run abapgit-agent guide for the full ABAP dev guide
3. Third: Use ref for specific syntax/topic details
4. Fourth: Use view/where commands to query ABAP system (if needed)
```

#### Priority 1: Check Local Git Repository

**Look for usage examples in your local ABAP project first:**
- Search for class/interface names in your codebase
- Check how similar classes are implemented
- This gives the most relevant context for your project

#### Priority 2: Read the ABAP Development Guide

```bash
# Read the full bundled ABAP dev guide (workflow, patterns, guidelines index)
abapgit-agent guide
```

This covers the complete development workflow, coding guidelines, object naming, unit testing patterns, and debugging guide. Always up-to-date with the installed package version.

#### Priority 3: Check ABAP References

```bash
# Search in ABAP cheat sheets and guidelines
abapgit-agent ref "CLASS"
abapgit-agent ref "INTERFACE"
abapgit-agent ref --topic classes
```

#### Priority 4: Use `where` and `view` Commands (Query ABAP System)

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

# View FULL source (definition + all method implementations)
abapgit-agent view --objects ZCL_UNKNOWN_CLASS --full

# View FULL source with dual line numbers (for setting breakpoints)
# G [N]  code — G = global line for debug set, [N] = include-relative
abapgit-agent view --objects ZCL_UNKNOWN_CLASS --full --lines

# View table/structure fields
abapgit-agent view --objects SFLIGHT --type TABL
abapgit-agent view --objects MY_STRUC --type STRU

# View data element or domain
abapgit-agent view --objects S_CARR_ID --type DTEL
abapgit-agent view --objects XFELD --type DOMA

# View message class (all messages)
abapgit-agent view --objects SY --type MSAG

# View function group (function module list)
abapgit-agent view --objects SUSR --type FUGR

# View CDS access control source
abapgit-agent view --objects SEPM_E_SALESORDER --type DCLS
```

**Example workflow for AI:**
```
User: "How do I use ZCL_ABGAGT_AGENT?"

AI thought process:
1. Search local repo for ZCL_ABGAGT_AGENT usage
2. Run: abapgit-agent guide  ← check if covered in the dev guide
3. Run: abapgit-agent ref "ZCL_ABGAGT_AGENT"  ← search guidelines
4. Still unclear? Run: abapgit-agent view --objects ZCL_ABGAGT_AGENT
```

**Key differences:**
- `where`: Shows WHERE an object is USED (references)
- `view`: Shows what an object DEFINES (structure, methods, source)

---

### 7. CDS Unit Tests

If a class under test reads a CDS view, use `CL_CDS_TEST_ENVIRONMENT` to provide test data — do **not** mock the database layer manually. Without this, test data setup is unreliable and tests may pass locally but fail on other systems.

**Trigger**: your class calls `SELECT FROM <cds_view>` directly or via a helper.

→ For full setup pattern and test double configuration: `abapgit-agent ref --topic cds-testing`

---

### 8. Writing and Running Unit Tests

#### Writing tests — use ABAP Test Double Framework by default

```
❌ WRONG: Write a manual test double class (ltd_mock_xxx) when the framework can do it
✅ CORRECT: Use cl_abap_testdouble=>create / configure_call for all interface mocking
```

**Decision — which double pattern to use:**

```
Does the mock need stateful behaviour (e.g. count calls, vary results per call, complex logic)?
  └── YES → manual test double class (ltd_mock_xxx DEFINITION FOR TESTING)
  └── NO  → ABAP Test Double Framework (cl_abap_testdouble=>create / configure_call)
             This covers 90 %+ of cases — simple return value / exception mocking
```

**ABAP Test Double Framework — quick pattern:**

```abap
" 1. Create double (declare with interface type)
DATA lo_agent TYPE REF TO zif_abgagt_agent.
lo_agent ?= cl_abap_testdouble=>create( 'ZIF_ABGAGT_AGENT' ).

" 2. Configure return value
cl_abap_testdouble=>configure_call( lo_agent )->returning( ls_result ).
lo_agent->pull( iv_url = 'https://...' ).   " registers config for these params

" 3. Inject and call
DATA(lo_cut) = NEW zcl_my_class( io_agent = lo_agent ).
DATA(ls_actual) = lo_cut->execute( ).
```

→ For full API reference (EXPORT params, exceptions, inherited methods, common mistakes) and class design rules (constructor injection, interfaces for dependencies):
  `abapgit-agent ref --topic unit-testable-code`

#### Running tests — use `unit` command

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

### 10. Probe and PoC Objects — Always Z/Y, Never in SAP Packages

```
❌ WRONG: Create a probe/PoC object with the project's SAP namespace prefix
❌ WRONG: Assign a probe/PoC object to an SAP-delivered package
✅ CORRECT: Probe/PoC objects always use Z* or Y* prefix
✅ CORRECT: Always assign to a customer namespace package (Z*, Y*, or $*)
```

This rule applies even on projects where production objects use SAP namespace (`CL_*`, `/NAMESPACE/*`).

**Trigger — when `objects.local.md` shows a SAP namespace prefix (`CL_*`, `IF_*`, `/NAMESPACE/*`)
and the user asks to create a new object, always ask first:**

```
"Is this a production object, a PoC (will persist, needs its own package/repo),
 or a probe (throwaway, run once)?"
```

Never assume — wait for the user's answer before proceeding.

→ For full decision flow (how to determine namespace, probe vs PoC, scratchWorkspace,
  pocWorkspace, setup instructions): `abapgit-agent ref --topic probe-poc`

---

### 11. Troubleshooting ABAP Issues

| Symptom | Tool | When |
|---|---|---|
| HTTP 500 / runtime crash (ST22) | `dump` | Error already occurred |
| Wrong output, no crash | `debug` | Need to trace logic |

→ `abapgit-agent ref --topic debug-dump`

**Critical rules for `debug` sessions:**

1. **Always use `--json`** for all debug commands (`attach`, `vars`, `stack`, `step`) — human output is not machine-parseable
2. **Attach BEFORE trigger** — start `debug attach --json` in background first, wait for `"Listener active"`, THEN fire the trigger (`unit`/`pull`/`run`)
3. **Never pull to trigger** if a simpler trigger works — use `unit` when a test exists, `run` for a class runner; use `pull` only when the bug is specifically in the pull flow
4. **Never pass `--session`** to `step/vars/stack` — it bypasses the daemon and causes errors
5. **Always finish with `step --type continue --json`** — releases the frozen ABAP work process

**Finding the right line number for a breakpoint:**

Use `view --full --lines` to get ready-to-use `debug set` commands per method:

```bash
abapgit-agent view --objects ZCL_FOO --full --lines
```

**Regular methods (CM\*):** output shows `G [N]  code` — the method header has the exact hint:
```
  * ---- Method: DO_SOMETHING (CM002) — breakpoint: debug set --objects ZCL_FOO:90 ----
   88 [  1]  METHOD do_something.
   89 [  2]    DATA lv_x TYPE i.
   90 [  3]    lv_x = 1.
```
`G` = global assembled-source line (for `debug set`), `[N]` = include-relative (navigation only).

**Unit test methods (CCAU) and local class methods (CCIMP):** use `--include` flag with section-local line numbers:
```
  * ---- Method: SETUP — breakpoint: debug set --objects ZCL_FOO:12 --include testclasses ----
  * ---- Method: ZIF_BAR~DO_IT — breakpoint: debug set --objects ZCL_FOO:5 --include locals_imp ----
```

**Function module (FUGR) methods:** use `--full --fm <name> --lines` — the hint uses the include name as the object:
```bash
abapgit-agent view --objects SUSR --type FUGR --full --fm AUTHORITY_CHECK --lines
# Output:  * ---- FM: AUTHORITY_CHECK (LSUSRU04) — breakpoint: debug set --objects LSUSRU04:50 ----
```

```bash
# Regular method:
abapgit-agent debug set --objects ZCL_FOO:90
# Unit test method:
abapgit-agent debug set --objects ZCL_FOO:12 --include testclasses
# Local class method:
abapgit-agent debug set --objects ZCL_FOO:5 --include locals_imp
# Function module (use include name, no --include flag needed):
abapgit-agent debug set --objects LSUSRU04:50
```

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

→ `abapgit-agent ref --topic debug-session`

---

### 12. abaplint — Static Analysis (Optional, Project-Controlled)

abaplint is **optional**. Only run it if `.abaplint.json` exists in the project root.
Each project defines its own rules — never assume which rules are active.

**Detection:**
```bash
# Check whether this project uses abaplint
ls .abaplint.json 2>/dev/null && echo "abaplint enabled" || echo "no abaplint"
```

**When to run:**

Run abaplint as step 4b — after `syntax`, before `git commit`:

```bash
# Only if .abaplint.json exists
abapgit-agent lint
```

Fix any reported issues, then commit.

**Before applying any quickfix:**

```
❌ WRONG: Accept abaplint quickfixes without checking
✅ CORRECT: Run abapgit-agent ref --topic abaplint FIRST, then decide
```

The `prefer_inline` quickfix is known to introduce a **silent type truncation bug**
when applied to variables that are later extended with `&&`. Read the guidelines
before applying it.

**When abaplint flags an issue you don't understand:**
```bash
abapgit-agent ref --topic abaplint        # bundled rule guidance
abapgit-agent ref "prefer_inline"         # search for specific rule
abapgit-agent ref "no_inline"             # search by keyword
```

**Project-specific rule guidance:**

Projects can add their own abaplint notes to `guidelines/abaplint-local.md` in the
project repository. After running `abapgit-agent ref export`, the `ref` command
surfaces both bundled and project-specific guidance together.

→ `abapgit-agent ref --topic abaplint`

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

```bash
git checkout main  # or master/develop (auto-detected)
git pull origin main
git checkout -b feature/my-change
# edit your ABAP file (name from objects.local.md)
abapgit-agent syntax --files src/<name>.clas.abap
ls .abaplint.json 2>/dev/null && abapgit-agent lint   # abaplint (if configured)
git add . && git commit -m "feat: description"
git push origin feature/my-change
git fetch origin main && git rebase origin/main
git push origin feature/my-change --force-with-lease
abapgit-agent pull --files src/<name>.clas.abap --sync-xml
```

→ `abapgit-agent ref --topic branch-workflow`

### Trunk Workflow (`"mode": "trunk"`)

If workflow mode is `"trunk"` or not set, commit directly to the default branch:

```bash
git checkout main  # or master/develop (auto-detected)
git pull origin main
# edit your ABAP file (name from objects.local.md)
abapgit-agent syntax --files src/<name>.clas.abap
ls .abaplint.json 2>/dev/null && abapgit-agent lint   # abaplint (if configured)
git add . && git commit -m "feat: description"
git push origin main
abapgit-agent pull --files src/<name>.clas.abap --sync-xml
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
1. ✗ Do not create probe classes in the current project
2. ✓ If `scratchWorkspace` is configured → create probe class there (see Rule 10)
3. ✗ If `scratchWorkspace` is NOT configured → refuse, tell user to configure it in `.abapGitAgent`

**When user requests a PoC object:**
1. ✓ Read `pocWorkspace.path` from `.abapGitAgent`
2. ✓ If configured → read `{path}/.abapGitAgent` for package, show confirmation summary, wait for user
3. ✗ If NOT configured → refuse, tell user to set up a PoC repo and configure `pocWorkspace.path`

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

**When pull result contains `missing_abapgit_xml: true` (JSON mode) or warning about `.abapgit.xml`:**
1. ✓ Inform the user that `.abapgit.xml` is missing from the repository root
2. ✓ Suggest running `abapgit-agent init --package <PACKAGE>` to create it
3. ✓ If `ACTIVATED_COUNT=0` with an empty log, suspect this as the cause
4. ✗ Do not retry the pull — fix the missing file first

**When `transports.allowRelease = false`:**
1. ✗ Do not run `abapgit-agent transport release`
2. ✓ Inform the user that transport release is disabled for this project

**After every pull that creates or modifies ABAP objects:**
1. ✓ Always pass `--sync-xml` — rewrites any XML metadata files that differ from the ABAP serializer output, amends the commit, and re-pulls so git and the ABAP system stay in sync
2. ✓ If pull output shows `⚠️  X XML file(s) differ from serializer output`, re-run immediately with `--sync-xml`
3. ✗ Never leave a pull without `--sync-xml` when you authored the objects — abapGit will show **M (modified)** permanently otherwise

---

### Quick Decision Tree for AI

**When user asks to modify/create ABAP code:**

```
Modified ABAP files?
├─ CLAS/INTF/PROG/DDLS files?
│  ├─ Independent files (no cross-dependencies)?
│  │  └─ ✅ Use: syntax → [abaplint] → commit → push → pull --sync-xml
│  └─ Dependent files (interface + class, class uses class)?
│     └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --sync-xml
└─ Other types (FUGR, TABL, STRU, DTEL, TTYP, etc.)?
   ├─ XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG)?
   │  └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --files abap/ztable.tabl.xml --sync-xml
   ├─ DCLS (CDS access control)?
   │  └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --files abap/zc_view.dcls.xml --sync-xml
   │         ⚠️  Pass the .xml file — pull --files does NOT accept .asdcls extensions
   └─ FUGR and other complex objects?
      └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --sync-xml → (if errors: inspect)

[abaplint] = run abapgit-agent lint only if .abaplint.json exists in repo root
             before applying any quickfix: run abapgit-agent ref --topic abaplint
```

→ For creating new objects (what files to write): `abapgit-agent ref --topic object-creation`
→ For full workflow decision tree and error indicators: `abapgit-agent ref --topic workflow-detailed`

---

## Guidelines Index

| Command | Topic |
|---------|-------|
| `ref --topic sql` | ABAP SQL Best Practices |
| `ref --topic exceptions` | Exception Handling |
| `ref --topic classes` | ABAP Classes and Objects |
| `ref --topic objects` | Object Naming Conventions (defaults) |
| `ref --topic naming-limits` | Naming Length Limits — 30/16/40 char rules per type (CRITICAL before naming anything) |
| `ref --topic comments` | Documentation Comments (ABAP DOC, shorttext, @parameter, CDS `//`, program `*&---`) |
| `ref --topic testing` | Unit Testing |
| `ref --topic unit-testable-code` | Unit Testable Code (Dependency Injection) |
| `ref --topic cds` | CDS Views |
| `ref --topic cds-testing` | CDS Testing (Test Double Framework) |
| `ref --topic json` | JSON Handling |
| `ref --topic common-errors` | Common ABAP Errors - Quick Fixes |
| `ref --topic abapgit` | abapGit XML Metadata — **use for CDS/DDLS XML**, also CLAS, INTF, PROG, DCLS, FUGR |
| `ref --topic abapgit-xml-only` | abapGit XML Metadata — XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG) |
| `ref --topic abapgit-fugr` | abapGit XML Metadata — Function Group (FUGR) details |
| `ref --topic abaplint` | abaplint Rule Guidelines (prefer_inline trap, safe patterns) |
| `ref --topic debug-session` | Debug Session Guide |
| `ref --topic debug-dump` | Dump Analysis Guide |
| `ref --topic branch-workflow` | Branch Workflow |
| `ref --topic workflow-detailed` | Development Workflow (Detailed) |
| `ref --topic object-creation` | Object Creation (XML metadata, local classes) |
| `ref --topic run-probe-classes` | run Command — AI Guidelines (probe classes, scratchWorkspace) |
| `ref --topic probe-poc` | Probe and PoC — Full Decision Flow |

> `objects.local.md` — **Project** Naming Conventions (created by `init`, never overwritten). Read directly from `guidelines/objects.local.md` — no ref topic.

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
