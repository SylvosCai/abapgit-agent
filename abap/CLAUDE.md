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

Use `view --objects ZCL_MY_CLASS --full` to see the full source with **both** global assembled-source line numbers and include-relative `[N]` numbers:

```bash
abapgit-agent view --objects ZCL_MY_CLASS --full
```

Each line is shown as `G [N]  code` where:
- **G** = assembled-source global line → use with `debug set --objects CLASS:G` or `debug set --files src/cls.clas.abap:G`
- **[N]** = include-relative (restarts at 1 per method) → for code navigation only, not for breakpoints

The method header shows the ready-to-use `debug set` command using the global line:

```
   1  CLASS zcl_my_class DEFINITION.
   2    PUBLIC SECTION.
   3  ENDCLASS.
  * ---- Method: EXECUTE (CM002) — breakpoint: debug set --objects ZCL_MY_CLASS:7 ----
   7 [  1]  METHOD execute.
   8 [  2]    DATA lv_x TYPE i.
   9 [  3]    lv_x = 1.
  10 [  4]  ENDMETHOD.
```

**Two scenarios:**

| Scenario | Command |
|---|---|
| Source available locally (your own classes) | `debug set --files src/zcl_my_class.clas.abap:9` |
| No local source (abapGit library, SAP standard) | `debug set --objects ZCL_MY_CLASS:9` |

Both use the same assembled-source global line number **G** shown in the output. To set a breakpoint at `lv_x = 1.` (global line 9):
```bash
# With local file:
abapgit-agent debug set --files src/zcl_my_class.clas.abap:9
# Without local file:
abapgit-agent debug set --objects ZCL_MY_CLASS:9
abapgit-agent debug list    # confirm it was registered
```

> **Line number must point to an executable statement.** Two common mistakes when reading `view --full` output:
>
> 1. **Comment lines** — lines starting with `"` are never executable. ADT silently rejects the breakpoint.
>    Pick the next non-comment line instead.
>    ```
>     95 [ 70]    " --- Conflict detection ---   ← NOT valid (comment)
>     96 [ 71]    " Build remote file entries…   ← NOT valid (comment)
>     97 [ 73]    DATA(lt_file_entries) = …      ← valid ✅ (use global 97)
>    ```
>
> 2. **First line of a multi-line inline `DATA(x) = call(`** — the ABAP debugger treats the
>    `DATA(x) =` line as a declaration, not an executable step. Set the breakpoint on the
>    **next standalone executable statement** after the closing `).` instead.
>    ```
>    100 [ 92]    DATA(ls_checks) = prepare_deserialize_checks(   ← NOT valid (inline decl)
>    101 [ 93]      it_files = it_files                           ← NOT valid (continuation)
>    104 [ 96]      io_repo_desc = lo_repo_desc1 ).               ← NOT valid (continuation)
>    106 [ 98]    mo_repo->create_new_log( ).                     ← valid ✅ (use global 106)
>    ```
>
> Other non-executable lines: blank lines, `METHOD`/`ENDMETHOD`, `DATA:` declarations,
> `CLASS`/`ENDCLASS`. When in doubt, prefer a simple method call or assignment.

**Step 2 — attach and trigger**

Best practice: individual sequential calls. Once the daemon is running and
the session is saved to the state file, each `vars/stack/step` command is a
plain standalone call — no `--session` flag needed.

```bash
# Start attach listener in background (spawns a daemon, saves session to state file)
abapgit-agent debug attach --json > /tmp/attach.json 2>&1 &

# Rule 1: wait for "Listener active" in the output, THEN fire the trigger.
# attach --json prints "Listener active" to stderr (captured in attach.json) the
# moment the long-poll POST is about to be sent to ADT.  Waiting for this marker
# is reliable under any system load; a blind sleep may fire the trigger before
# ADT has a registered listener, causing the breakpoint hit to be missed.
until grep -q "Listener active" /tmp/attach.json 2>/dev/null; do sleep 0.3; done
sleep 1   # brief extra window for the POST to reach ADT

# Trigger in background — MUST stay alive for the whole session
abapgit-agent unit --files src/zcl_my_class.clas.testclasses.abap > /tmp/trigger.json 2>&1 &

# Poll until breakpoint fires and session JSON appears in attach output
SESSION=""
for i in $(seq 1 30); do
  sleep 0.5
  SESSION=$(grep -o '"session":"[^"]*"' /tmp/attach.json 2>/dev/null | head -1 | cut -d'"' -f4)
  [ -n "$SESSION" ] && break
done

# Inspect and step — each is an individual call, no --session needed
abapgit-agent debug stack --json
abapgit-agent debug vars  --json
abapgit-agent debug vars  --expand LS_OBJECT --json
abapgit-agent debug step  --type over --json
abapgit-agent debug vars  --json

# ALWAYS release the ABAP work process before finishing
abapgit-agent debug step --type continue --json

# Check trigger result
cat /tmp/trigger.json
rm -f /tmp/attach.json /tmp/trigger.json
```

> **Four rules for scripted mode:**
> 1. Wait for `"Listener active"` in the attach output before firing the trigger — this message is printed to stderr (captured in `attach.json`) the moment the listener POST is about to reach ADT. A blind `sleep` is not reliable under system load.
> 2. Keep the trigger process alive in the background for the entire session — if it exits, the ABAP work process is released and the session ends
> 3. Always finish with `step --type continue` — this releases the frozen work process so the trigger can complete normally
> 4. **Never pass `--session` to `step/vars/stack`** — it bypasses the daemon IPC and causes `noSessionAttached`. Omit it and let commands auto-load from the saved state file.

**Step 3 — step through and inspect**

```bash
abapgit-agent debug vars   --json                        # all variables
abapgit-agent debug vars   --name LV_RESULT --json       # one variable
abapgit-agent debug vars   --expand LT_DATA --json       # drill into table/structure
abapgit-agent debug step   --type over --json            # step over
abapgit-agent debug step   --type into --json            # step into
abapgit-agent debug step   --type continue --json        # continue to next breakpoint / finish
abapgit-agent debug stack  --json                        # call stack (shows which test method is active)
```

> **`step --type continue` return values:**
> - `{"continued":true,"finished":true}` — program ran to **completion** (ADT returned HTTP 500, session is over). Do not re-attach.
> - `{"continued":true}` (no `finished` field) — program hit the **next breakpoint** and is still paused. You must re-attach to receive the suspension and continue inspecting:
>   ```bash
>   abapgit-agent debug attach --json > /tmp/attach2.json 2>&1 &
>   until grep -q "Listener active" /tmp/attach2.json 2>/dev/null; do sleep 0.3; done
>   # session auto-resumes — no trigger needed (trigger is still running in background)
>   SESSION=""
>   for i in $(seq 1 30); do
>     sleep 0.5
>     SESSION=$(grep -o '"session":"[^"]*"' /tmp/attach2.json 2>/dev/null | head -1 | cut -d'"' -f4)
>     [ -n "$SESSION" ] && break
>   done
>   abapgit-agent debug vars --json   # inspect at next breakpoint
>   abapgit-agent debug step --type continue --json   # release again
>   ```
> Missing this re-attach step causes the session to appear dead when it is actually paused at the next breakpoint.

**Clean up** when done:
```bash
abapgit-agent debug delete --all
```

> **If the stale debug daemon holds an ABAP lock** (symptom: `Requested object EZABAPGIT is currently locked by user CAIS`):
> ```bash
> pkill -f "debug-daemon"   # kills daemon, SIGTERM triggers session.terminate() internally
> ```
> Wait ~10 seconds for the lock to release, then retry.

---

### Debugged Pull Flow Architecture

The following call chain was traced by live debugging (2026-03), verified with two breakpoints firing successfully. Use as a reference for setting breakpoints when investigating pull issues:

```
HTTP Request
  → SAPMHTTP (%_HTTP_START) [frame 1]
  → SAPLHTTP_RUNTIME (HTTP_DISPATCH_REQUEST) [frame 2]
  → CL_HTTP_SERVER (EXECUTE_REQUEST) [frame 3]
  → CL_REST_HTTP_HANDLER (IF_HTTP_EXTENSION~HANDLE_REQUEST) [frame 4]
  → CL_REST_ROUTER (IF_REST_HANDLER~HANDLE) [frame 5]
  → CL_REST_RESOURCE (IF_REST_HANDLER~HANDLE, DO_HANDLE_CONDITIONAL, DO_HANDLE) [frames 6-8]
  → ZCL_ABGAGT_RESOURCE_BASE (IF_REST_RESOURCE~POST, CM004:84) [frame 9]
  → ZCL_ABGAGT_COMMAND_PULL (ZIF_ABGAGT_COMMAND~EXECUTE, CM001:21) [frame 10]
  → ZCL_ABGAGT_AGENT (ZIF_ABGAGT_AGENT~PULL, CM00D) [frame 11]
      CM00D: build_file_entries_from_remote()           — fetch remote files + acquire EZABAPGIT lock
      CM00D: check_conflicts()                          — abort if conflicts found (LT_CONFLICTS)
      CM00D: prepare_deserialize_checks()               — transport + requirements; returns LS_CHECKS
                                                          (LS_CHECKS-OVERWRITE[n] = objects to overwrite)
      CM00D: mo_repo->create_new_log()            :207  — init abapGit log object
      CM00D: RTTI check (lv_deser_has_filter)           — does ZIF_ABAPGIT_REPO~DESERIALIZE have
                                                          II_OBJ_FILTER parameter? (X = yes)
      CM00D: CALL METHOD mo_repo->('DESERIALIZE') :236  — dynamic dispatch with PARAMETER-TABLE
                                                          (IS_CHECKS, II_LOG, II_OBJ_FILTER)
  → ZCL_ABAPGIT_REPO (ZIF_ABAPGIT_REPO~DESERIALIZE, CM00L) [frame 12]
      CM00L: find_remote_dot_abapgit()            :525  — fetch .abapgit from remote; COMMIT WORK
                                                          AND WAIT inside → releases EZABAPGIT lock
      CM00L: find_remote_dot_apack()              :526  — fetch .apack-manifest
      CM00L: check_write_protect()                :528  — package write-protect check
      CM00L: check_language()                     :529  — language check
      CM00L: (requirements/dependencies checks)   :531  — abort if not met
      CM00L: deserialize_dot_abapgit()            :543  — update .abapgit config in ABAP
                                                          populates LT_UPDATED_FILES (e.g. .abapgit.xml)
      CM00L: deserialize_objects(is_checks, ii_log, ii_obj_filter) :545
  → ZCL_ABAPGIT_REPO (DESERIALIZE_OBJECTS, CM007:258) [frame 13]
      CM007: zcl_abapgit_objects=>deserialize(ii_repo=me, ..., ii_obj_filter=...) :259
  → ZCL_ABAPGIT_OBJECTS (DESERIALIZE static, CM00A) [frame 14]
      CM00A: lt_steps = get_deserialize_steps()   :617  — 4-step pipeline
      CM00A: lv_package = ii_repo->get_package()  :619
      CM00A: lt_remote = get_files_remote(ii_obj_filter=...) :628  — filtered remote files
      CM00A: lt_results = zcl_abapgit_file_deserialize=>get_results(...) :631
      CM00A: zcl_abapgit_objects_check=>checks_adjust(...) :640
      CM00A: check_objects_locked(lt_items)        :657
      CM00A: LOOP steps → LOOP objects → call type handler:
             Step 1 Pre-process:  ZCL_ABGAGT_VIEWER_CLAS imported
             Step 2 DDIC:         (nothing for CLAS)
             Step 3 Non-DDIC:     ZCL_ABGAGT_VIEWER_CLAS imported + activated
             Step 4 Post-process: ZCL_ABGAGT_VIEWER_CLAS imported
      CM00A: returns RT_ACCESSED_FILES
  ← back in CM00L:
      CM00L: checksums()->update()
      CM00L: update_last_deserialize()
      CM00L: COMMIT WORK AND WAIT                       — commit the whole transaction
```

**Key variables observed during trace** (for `pull --files abap/zcl_abgagt_viewer_clas.clas.abap`):

| Variable at BP1 (CM00D:207) | Value |
|---|---|
| `IT_FILES[1]` | `abap/zcl_abgagt_viewer_clas.clas.abap` |
| `LI_REPO` (class) | `ZCL_ABAPGIT_REPO_ONLINE` |
| `LO_OBJ_FILTER` (class) | `ZCL_ABAPGIT_OBJECT_FILTER_OBJ` |
| `LT_CONFLICTS` | empty (no conflicts) |
| `LS_CHECKS-OVERWRITE[1]` | CLAS `ZCL_ABGAGT_VIEWER_CLAS`, DEVCLASS `$ABAP_AI_BRIDGE`, DECISION=Y |
| `LV_DESER_HAS_FILTER` | `X` (II_OBJ_FILTER supported) |

| Variable at BP2 (CM00L:545) | Value |
|---|---|
| `II_OBJ_FILTER` (class) | `ZCL_ABAPGIT_OBJECT_FILTER_OBJ` |
| `LT_UPDATED_FILES[1]` | `/.abapgit.xml` (from deserialize_dot_abapgit) |

**Verified breakpoint locations** (assembled-source global line numbers, confirmed accepted by ADT):

| Location | Command |
|---|---|
| Before DESERIALIZE call | `debug set --objects ZCL_ABGAGT_AGENT:207` (create_new_log) |
| abapGit DESERIALIZE entry | `debug set --objects ZCL_ABAPGIT_REPO:545` (deserialize_objects call) |
| abapGit objects pipeline entry | `debug set --objects ZCL_ABAPGIT_OBJECTS:<CM00A first exec line>` |

> **Note**: The EZABAPGIT lock is acquired during `build_file_entries_from_remote()` and released
> inside `find_remote_dot_abapgit()` via `COMMIT WORK AND WAIT` — i.e., the lock is released before
> BP2 fires. There is no active lock between BP1 and the end of CM00L.

### Known Limitations and Planned Improvements

The following issues were identified during a live debugging session (2026-03) and should be fixed to make future debugging easier:

#### ~~1. `view --full` global line numbers don't match ADT line numbers~~ ✅ Fixed

**Fixed**: `view --full` now shows dual line numbers per line: `G [N]  code` where G is the assembled-source global line (usable directly with `--objects CLASS:G` or `--files src/cls.clas.abap:G`) and `[N]` is the include-relative counter for navigation. Method headers show the ready-to-use `debug set --objects CLASS:G` command.

Global line numbers are computed **client-side** in Node.js, not in ABAP:
- **Own classes** (local `.clas.abap` file exists): reads the local file — guaranteed exact match with ADT
- **Library classes** (no local file, e.g. abapGit): fetches assembled source from `/sap/bc/adt/oo/classes/<name>/source/main`

Both strategies scan for `METHOD <name>.` as the first token on the line to find `global_start`. The METHOD statement line itself is shown as `G [1]`, but is not executable — use the next executable statement (typically a few lines after METHOD) as the actual breakpoint target.

#### ~~2. Include-relative breakpoint form (`=====CMxxx:N`) not implemented in the CLI~~ ✅ Superseded

**Superseded**: The `/programs/includes/` ADT endpoint was found to not accept breakpoints for OO class method includes — ADT only accepts the `/oo/classes/.../source/main` URI with assembled-source line numbers. The `=====CMxxx:N` approach was dropped. Instead, `view --full` now provides the correct assembled-source global line number G directly, and both `--objects CLASS:G` and `--files src/cls.clas.abap:G` work reliably.

#### ~~3. `stepContinue` re-attach pattern missing from docs~~ ✅ Fixed

**Fixed**: Step 3 of the debug guide now documents the two possible return values of `step --type continue` and includes the re-attach pattern for when the program hits a second breakpoint instead of finishing.

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
