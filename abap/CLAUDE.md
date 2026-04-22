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
| Enhancement (ENHO) | `<name>.enho.<hash>.abap` (one per hook) | `<name>.enho.xml` |
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

### 4. Use Syntax Command Before Commit (for CLAS, INTF, PROG, DDLS, FUGR)

```
❌ WRONG: Make changes → Commit → Push → Pull → Find errors → Fix → Repeat
✅ CORRECT: Make changes → Run syntax → Fix locally → Commit → Push → Pull → Done
```

**For CLAS, INTF, PROG, DDLS, FUGR files**: Run `syntax` command BEFORE commit to catch errors early.

```bash
# Check syntax of local code (no commit/push needed)
# Use the actual filename from your project (name comes from objects.local.md)
abapgit-agent syntax --files src/<name>.clas.abap
abapgit-agent syntax --files src/<name>.ddls.asddls
abapgit-agent syntax --files src/<name>.fugr.<fm_name>.abap

# Check multiple INDEPENDENT files
abapgit-agent syntax --files src/<name1>.clas.abap,src/<name2>.clas.abap
```

**For other types (TABL, STRU, DCLS, etc.)**: Skip syntax, proceed to commit/push/pull.

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
        <WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>   (inside the <VSEOCLASS> block)
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

For ABAP programs (PROG type), use `--program` instead: `abapgit-agent run --program ZR_MY_REPORT`

For how to write a runner class (`out->write()` usage, output format): `abapgit-agent ref --topic run-probe-classes`

---

### 10. Never Run `drop` Command Without Explicit Permission

**Never call `abapgit-agent drop` unless the user explicitly confirms.** Dropping an object physically deletes it from the ABAP system — this is irreversible without a subsequent pull.

**When to SUGGEST `drop --pull`:**

When an object is in a broken or inconsistent state that cannot be resolved by re-pulling:
- `inspect` reports **"INCLUDE report ... not found"** (stale include in ABAP not present in git)
- Pull repeatedly fails with **"Activation cancelled"** and re-pulling doesn't fix it
- Object has a corrupt inactive version that permanently blocks activation

In these cases, suggest the fix and wait for confirmation:

```
"The object ZCL_FOO has an inconsistent state (stale include in ABAP).
 This can be fixed by dropping and re-pulling it:

   abapgit-agent drop --files abap/zcl_foo.clas.abap --pull

 This will delete ZCL_FOO from ABAP and immediately re-activate it from git.
 Confirm when ready."
```

**What `drop --pull` does:**
1. Physically deletes the object from the ABAP system
2. Immediately re-pulls and re-activates it from git (clean state)
3. Does NOT touch the git repository file

**Constraint:** Only CLAS, INTF, PROG, TABL, TTYP are supported. DTEL (data elements) are rejected — edit the XML and use `pull` instead.

---

### 11. Probe and PoC Objects — Always Z/Y, Never in SAP Packages

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

### 12. Troubleshooting ABAP Issues

| Symptom | Tool | When |
|---|---|---|
| HTTP 500 / runtime crash (ST22) | `dump` | Error already occurred |
| Wrong output, no crash | `debug` | Need to trace logic |

→ `abapgit-agent ref --topic debug-dump`

**Critical rules for `debug` sessions:**

```
❌ WRONG: abapgit-agent debug attach
❌ WRONG: abapgit-agent debug vars
❌ WRONG: abapgit-agent debug step --type continue
✅ CORRECT: abapgit-agent debug attach --json > /tmp/a.json 2>&1 &
✅ CORRECT: abapgit-agent debug vars --json
✅ CORRECT: abapgit-agent debug step --type continue --json
```

1. **Always use `--json`** for ALL debug commands (`attach`, `vars`, `stack`, `step`) — human output is not machine-parseable. This is non-negotiable.
2. **Attach BEFORE trigger** — start `debug attach --json` in background first, wait for `"Listener active"`, THEN fire the trigger (`unit`/`pull`/`run`)
3. **Never pull to trigger** if a simpler trigger works — use `unit` when a test exists, `run` for a class runner; use `pull` only when the bug is specifically in the pull flow
4. **Never pass `--session`** to `step/vars/stack` — it bypasses the daemon and causes errors
5. **Always finish with `step --type continue --json`** — releases the frozen ABAP work process

**Finding the right line number for a breakpoint:**

```
❌ WRONG: abapgit-agent debug set --objects ZCL_FOO:42   ← never guess a line number
✅ CORRECT: abapgit-agent view --objects ZCL_FOO --full --lines   ← get exact line from output
```

Always run `view --full --lines` first — it prints a ready-to-use `debug set` command for every method. Never guess or estimate line numbers.

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
abapgit-agent view --objects ZCL_FOO --full --lines  # 0. get exact line number from output hint
abapgit-agent debug set --objects ZCL_FOO:42        # 1. set breakpoint (use line from step 0)
#    ⚠️  Avoid: LOOP AT ... ASSIGNING headers — ADT registers them but ABAP runtime skips them
#    Set BP on the first statement INSIDE the loop body instead
abapgit-agent debug attach --json > /tmp/a.json 2>&1 &   # 2. attach (background)
until grep -q "Listener active" /tmp/a.json 2>/dev/null; do sleep 0.3; done
abapgit-agent unit --files src/zcl_foo.clas.testclasses.abap > /tmp/t.json 2>&1 &  # 3. trigger (background &)
# 4. POLL until session appears — do NOT call vars/stack before this completes
SESSION=""
for i in $(seq 1 30); do
  sleep 0.5
  SESSION=$(grep -o '"session":"[^"]*"' /tmp/a.json 2>/dev/null | head -1 | cut -d'"' -f4)
  [ -n "$SESSION" ] && break
done
# 5. inspect (session is now live)
abapgit-agent debug vars --json
abapgit-agent debug step --type continue --json     # 6. release
```

→ `abapgit-agent ref --topic debug-session`

---

### 13. abaplint — Static Analysis (Optional, Project-Controlled)

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
| `safeguards.disableImport` | `true`/`false` | `false` | Disables import entirely (one-time or managed operation) |
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
git fetch origin main && git rebase origin/main
abapgit-agent pull --files src/<name>.clas.abap --sync-xml  # --sync-xml amends + pushes internally
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
abapgit-agent pull --files src/<name>.clas.abap --sync-xml  # --sync-xml amends + pushes internally
```

<!-- AI-CONDENSED-START -->
# AI Agent Instructions

## Step 1: Read project config before doing anything

- Read `.abapGitAgent` → get `folder` (ABAP source folder) and `workflow.mode`
- Read `guidelines/objects.local.md` → naming conventions for this project (if file exists)
- Read `.abapgit-agent.json` → safeguards, conflict detection, inspect variant

---

### AI Tool Guidelines

**When working on an unfamiliar ABAP topic, syntax, or pattern:**
1. ✗ Never guess or assume ABAP syntax — it is strict and errors waste time
2. ✓ Search the Guidelines Index below and fetch the relevant `ref --topic` before writing code
3. ✓ For unknown classes or methods: search local git repo first, then `abapgit-agent ref`, then `abapgit-agent view --objects <CLASS>` to query the ABAP system

**Before modifying any existing ABAP object:**
1. ✓ Always read the current file(s) before editing — never overwrite blindly
2. ✓ For a class: read the `.clas.abap` file; also read `.clas.locals_def.abap`, `.clas.locals_imp.abap`, `.clas.testclasses.abap` if they exist and are relevant

**When `abapgit-agent syntax` fails:**
1. ✓ Fix the error locally and re-run syntax — do NOT commit or proceed
2. ✗ Never commit a file that failed syntax check
3. ✓ Only proceed to `[abaplint] → commit` once syntax passes

**When creating a new ABAP object:**
1. ✓ Customer namespace (Z*/Y* name AND Z*/Y*/$* package) → write files directly, no confirmation needed
2. ✓ SAP namespace or SAP-delivered package → show creation summary and wait for explicit user confirmation before writing any files
3. ✓ Summary format: object name, type, package, files to be created
4. ✗ Never pick a package yourself by running `abapgit-agent tree` — determine from `objects.local.md` or `.abapGitAgent`, or ask the user
5. ✓ Before naming anything: check naming length limits (`ref --topic naming-limits`) — TABL field names max 16 chars, most others 30 chars

**When writing unit tests for a class that reads a CDS view:**
1. ✓ Always use `CL_CDS_TEST_ENVIRONMENT` to provide test data — do NOT mock the database layer manually
2. ✗ Never insert test data directly into database tables for CDS view testing
3. ✓ For full setup pattern: `abapgit-agent ref --topic cds-testing`

**When writing unit tests (mocking dependencies):**
1. ✓ Default to ABAP Test Double Framework (`cl_abap_testdouble=>create` / `configure_call`) — covers 90%+ of cases
2. ✓ Use a manual test double class (`ltd_mock_xxx FOR TESTING`) only when stateful behaviour is needed (call counting, varying results per call)
3. ✗ Never write a manual mock class when the framework can do it
4. ✓ For full API reference and class design rules: `abapgit-agent ref --topic unit-testable-code`

**When adding local helper or test include files to a class:**
1. ✓ `clas.testclasses.abap` requires `<WITH_UNIT_TESTS>X</WITH_UNIT_TESTS>` in the `.clas.xml` `<VSEOCLASS>` block — without it abapGit will not activate the test include
2. ✓ `clas.locals_def.abap` requires `<CLSCCINCL>X</CLSCCINCL>` in the `.clas.xml`
3. ✓ For exact XML flag placement: `abapgit-agent ref --topic object-creation`
4. ✓ Test method names (`FOR TESTING`) are subject to the 30-char method name limit — count before writing (e.g. `test_returns_data_sorted_by_revenue` = 35 chars → invalid)

**When `abapgit-agent run` is relevant:**
1. ✗ Never call `abapgit-agent run` unless the user explicitly asks — a `run` command present in the user's prompt counts as explicit authorization; no confirmation step needed
2. ✓ After activating a class implementing `IF_OO_ADT_CLASSRUN`: stop and tell the user how to run it manually
3. ✓ Before calling `run`: verify the object type — `--class` for CLAS, `--program` for PROG. Check the file extension (`.clas.abap` vs `.prog.abap`) if unsure
4. ✓ For full guidelines: `abapgit-agent ref --topic run-probe-classes`

**When `abapgit-agent drop` is relevant:**
1. ✗ Never call `abapgit-agent drop` unless the user explicitly confirms — it physically deletes the object from ABAP, irreversible without re-pull
2. ✓ When an object is in a broken state (stale include, activation permanently blocked): suggest `drop --pull` and wait for confirmation
3. ✓ Supported types for drop: CLAS, INTF, PROG, TABL, TTYP only — DTEL is not supported

**When using `abapgit-agent debug` commands:**
1. ✓ Always use `--json` for ALL debug commands (`attach`, `vars`, `stack`, `step`) — human output is not machine-parseable
2. ✗ Never run `abapgit-agent debug attach` without `--json` and background redirect (`> /tmp/a.json 2>&1 &`)
3. ✓ Always attach BEFORE triggering — wait for `"Listener active"` in output before firing the trigger
4. ✓ **Run the trigger in background (`&`)** — `unit` or `run --class`, never foreground
5. ✓ **Poll attach.json for `"session":"..."` BEFORE calling `vars`/`stack`** — do NOT call vars/stack immediately after the trigger; the breakpoint has not fired yet. Loop until the session ID appears.
6. ✓ Always finish a debug session with `abapgit-agent debug step --type continue --json` to release the frozen work process
7. ✓ For full debug session guide (complete poll loop, stale session diagnosis): `abapgit-agent ref --topic debug-session`

**Read `.abapGitAgent` to determine workflow mode:**

**When `workflow.mode = "branch"`:**
1. ✓ Auto-detect default branch (main/master/develop)
2. ✓ Create feature branches (naming: `feature/description`)
3. ✓ Always `git fetch origin <default> && git rebase origin/<default>` before `pull` command
4. ✓ Use `--force-with-lease` after rebase (never `--force`)
5. ✓ Create PR with squash merge when feature complete
6. ✗ Never commit directly to default branch
7. ✗ Never use `git push --force` (always use `--force-with-lease`) — `pull --sync-xml` handles its own push internally; no manual push needed after it
8. ✓ **Push the feature branch to remote before the first `abapgit-agent pull`** — the ABAP system reads from the remote URL; a branch that only exists locally is invisible to it. If `git push` fails with "no upstream branch", use `git push --set-upstream origin <branch>` first.

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
1. ✓ Always use `--files` to pull only the files you changed — faster and safer
2. ✓ Full pull (without `--files`) is only acceptable when you explicitly need to activate all objects (e.g. initial repo setup)

**When `safeguards.disablePull = true`:**
1. ✗ Do not run `abapgit-agent pull` at all
2. ✓ Inform the user that pull is disabled for this project (CI/CD only)

**When `safeguards.disableRun = true`:**
1. ✗ Do not run `abapgit-agent run` at all
2. ✓ Inform the user that run is disabled for this project

**When `safeguards.disableImport = true`:**
1. ✗ Do not run `abapgit-agent import` at all
2. ✓ Inform the user that import is disabled for this project

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
2. ✓ If pull is aborted with conflict error: **stop, inform the user about the conflict, and wait for their decision** — do NOT automatically retry with `--conflict-mode ignore`
3. ✗ Don't add `--conflict-mode ignore` unless the user explicitly asks you to override the conflict
4. ✓ You may mention `--conflict-mode ignore` as an option the user can choose, but never run it on your own
5. ✓ **Distinguishing real vs. activation-noise conflicts (`SYSTEM_EDIT`):**
   - **Activation noise** (safe to inform user it can be ignored): `System changed by` is *your own user*, timestamp within seconds/minutes of your last pull — the ABAP serializer normalised the object hash after activation. Tell the user: "This is activation noise — safe to re-run with `--conflict-mode ignore`."
   - **Real conflict**: `System changed by` is a *different user*, or the timestamp is not recent. Stop and wait for user decision.

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

**Before every `git commit` (when `.abaplint.json` exists AND changed files include `.abap` or `.xml` ABAP source files):**
1. ✓ Run `abapgit-agent lint` before committing — no exceptions for ABAP file type or workflow mode (trunk or branch)
2. ✓ Run lint even when syntax is skipped (dependent files, XML-only types, DCLS, ENHO)
3. ✓ Correct pre-commit sequence: syntax (if applicable for CLAS/INTF/PROG/DDLS/FUGR) → `abapgit-agent lint` → `git commit`
4. ✗ Never commit ABAP files without running lint first when `.abaplint.json` exists
5. ✓ Non-ABAP changes only (docs, config, JS): skip lint and syntax — they do not apply
6. ✓ Before applying any abaplint quickfix, run `abapgit-agent ref --topic abaplint` first — the `prefer_inline` fix has a known silent type truncation bug

**Before running `abapgit-agent pull`:**
1. ✓ Always verify that ALL changed files are committed — `abapGit reads from git, not from local disk`
2. ✗ Never run `pull` if there are uncommitted changes (`git status` shows modified/untracked files that should be pulled)
3. ✓ In branch workflow: verify all commits are pushed — run `git log origin/<branch>..HEAD`. If this shows any commits, push first with `git push` (or `git push --set-upstream origin <branch>` if no upstream yet).
4. ✓ With `--sync-xml`: `git add` → `git commit` → `git push` → `abapgit-agent pull --sync-xml` — push first so the ABAP system can read your changes; `--sync-xml` will then amend and re-push automatically if XML diffs are found after activation
5. ✓ Without `--sync-xml`: `git add` → `git commit` → `git push` → `abapgit-agent pull` — same flow, push manually

**If pull returns `SUCCESS` with `ACTIVATED_COUNT: 0` and no errors:**
- ✓ **`LOCAL_XML_FILES` only contains `package.devc.xml`:** The branch does not exist on the remote. Fix: `git push --set-upstream origin <branch>`, then re-run the pull.
- ✓ **`LOCAL_XML_FILES` is empty:** Two possible causes:
  - (a) Objects are already at the latest version — verify with `abapgit-agent view --objects <NAME>`. If the object exists and looks correct, no action needed.
  - (b) Commits not pushed yet — run `git log origin/<branch>..HEAD` to check. If unpushed commits exist, run `git push` then retry pull.
- ✓ **Message is "Activation cancelled. Check the inactive objects."**: The ABAP system rejected the object during activation but returned no detailed error. Run `abapgit-agent inspect --files <file>` to surface the compile error (the object is already on the system after the failed pull). For DDLS: also verify that the `define view entity <NAME>` name in the `.asddls` source exactly matches the `<DDLNAME>` in the `.ddls.xml` (case-sensitive — the DDL name must be UPPER CASE). Fix the error locally, run `syntax` to verify, then re-commit and re-pull.

**After every pull:**
1. ✓ If pull output shows `⚠️  X XML file(s) differ from serializer output` — re-run immediately with `--sync-xml`, even on an initial setup pull with no ABAP objects (`.abapgit.xml` can drift too)
2. ✓ When you authored the objects: always pass `--sync-xml` — rewrites XML metadata files that differ from the ABAP serializer output, amends the commit, and re-pulls so git and the ABAP system stay in sync
3. ✗ Never leave a pull without `--sync-xml` when you authored the objects — abapGit will show **M (modified)** permanently otherwise
4. ✗ **Never use `--sync-xml` after a failed pull** — if the pull itself failed (errors, activation cancelled, object not created), the serializer output reflects the broken object state. Using `--sync-xml` will write an empty or partial XML back to disk and corrupt the file. Fix the error first, verify pull succeeds, then apply `--sync-xml`.

---

### Quick Decision Tree for AI

**Before modifying an existing ABAP object: always read the current file(s) first.**

**`<folder>` below = the `folder` value from `.abapGitAgent` (e.g. `abap/` or `src/`)**

**`[abaplint]` = run `abapgit-agent lint` only if `.abaplint.json` exists in repo root. If syntax or lint fails → fix locally and re-run before proceeding to commit.**

**When user asks to modify/create ABAP code:**

```
Modified ABAP files?
├─ CLAS/INTF/PROG/DDLS/FUGR files?
│  ├─ Independent files (no cross-dependencies)?
│  │  └─ ✅ Use: syntax → [abaplint] → commit → push → pull --files <folder>/<name>.<ext> --sync-xml
│  │         Examples:
│  │           syntax --files src/zcl_foo.clas.abap → commit → push → pull --files src/zcl_foo.clas.abap --sync-xml
│  │           syntax --files src/zc_my_view.ddls.asddls → commit → push → pull --files src/zc_my_view.ddls.asddls --sync-xml
│  │         ⚠️  DDLS: pass the .asddls file (not .xml) to both syntax and pull --files
│  └─ Dependent files (interface + class, class uses class)?
│     └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --files <folder>/<intf>.<ext>.abap,<folder>/<class>.<ext>.abap --sync-xml
└─ Other types (TABL, STRU, DTEL, TTYP, etc.)?
   ├─ XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG)?
   │  └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --files <folder>/<name>.tabl.xml --sync-xml
   ├─ DCLS (CDS access control)?
   │  └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --files <folder>/<name>.dcls.xml --sync-xml
   │         ⚠️  Pass the .xml file — pull --files does NOT accept .asdcls extensions
   ├─ ENHO (Enhancement)?
   │  └─ ✅ Use: syntax (optional) → [abaplint] → commit → push → pull --files <folder>/<name>.enho.xml --sync-xml
   │         ⚠️  syntax checks basic errors in the hook body; semantic checks require pull
   │         ⚠️  Pass the .enho.xml file to pull — hash .abap files also work but xml is preferred
   │         → see: abapgit-agent ref --topic enho
   └─ Other complex objects?
      └─ ✅ Use: skip syntax → [abaplint] → commit → push → pull --files <folder>/<name>.<ext> --sync-xml → (if errors: inspect)
```

**After pull succeeds:**
- ✓ If the class has test includes: run `abapgit-agent unit --files <folder>/<name>.clas.testclasses.abap` to verify all tests pass.

**First pull of a new CDS view + a class that depends on it:**
- ✓ Pull in two steps: first `pull --files <folder>/<name>.ddls.asddls --sync-xml` (activates the CDS view), then `pull --files <folder>/<name>.clas.abap --sync-xml` (activates the class against the now-live view).
- ⚠️  "Error updating where-used list" almost always means a **syntax error** in the class. Since the pull already attempted activation, run `abapgit-agent inspect --files <folder>/<name>.clas.abap` to surface the error. Fix it locally, run `syntax` to verify, then re-commit and re-pull. Only if syntax and inspect both pass cleanly does the dependency order matter — in that case, the CDS view was not yet active; retry after pulling the CDS view first.

**First pull of an interface + implementing class together:**
- ⚠️  "Error updating where-used list" almost always means a **syntax error** in the class. Since the pull already attempted activation, run `abapgit-agent inspect --files <folder>/<name>.clas.abap` to surface the error. Fix it locally, run `syntax` to verify, then re-commit and re-pull. Only if inspect passes cleanly is the ordering the cause — in that case, the interface was just activated and the class can now compile on a second attempt.

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
| `ref --topic string-template` | String Templates — syntax, escaping `\{` `\}`, JSON payloads |
| `ref --topic abapgit` | abapGit XML Metadata — **use for CDS/DDLS XML**, also CLAS, INTF, PROG, DCLS, FUGR |
| `ref --topic abapgit-xml-only` | abapGit XML Metadata — XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG) |
| `ref --topic abapgit-fugr` | abapGit XML Metadata — Function Group (FUGR) details |
| `ref --topic enho` | Enhancement Objects (ENHO) — workflow, hash algorithm, creation guide |
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
