---
layout: default
title: Debug Session Guide
nav_order: 18
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Debug Session Guide

#### When There Is No Dump — Use `debug`

Use `debug` when:
- The bug is a logic error (wrong output, no crash)
- You need to inspect variable values mid-execution
- You want to verify which branch of code runs

**Step 1 — set a breakpoint** on the first executable statement you want to inspect:

Use `view --objects ZCL_MY_CLASS --full --lines` to see the full source with line numbers and ready-to-use `debug set` commands:

```bash
abapgit-agent view --objects ZCL_MY_CLASS --full --lines
```

> **Tip**: `view --full` (without `--lines`) shows the same full source as clean readable code without line numbers — useful for understanding logic. Add `--lines` when you need line numbers for breakpoints.

The output varies by section type:

**Regular methods (CM\* sections)** — dual line numbers `G [N]` per line, with a `debug set` hint at the method header:

```
  * ---- Method: EXECUTE (CM002) — breakpoint: debug set --objects ZCL_MY_CLASS:9 ----
   7 [  1]  METHOD execute.
   8 [  2]    DATA lv_x TYPE i.
   9 [  3]    lv_x = 1.
  10 [  4]  ENDMETHOD.
```

- **G** = assembled-source global line → use with `debug set --objects CLASS:G` or `--files src/cls.clas.abap:G`
- **[N]** = include-relative (restarts at 1 per method) → for code navigation only, not for breakpoints

The hint already points to the first **executable** line, skipping `METHOD`, blank lines, comments, and all declaration forms (`DATA`, `DATA:`, `DATA(`).

**Classes with active ENHO hook enhancements** — `view --full --lines` injects the ENHO hook lines into the CM section so you can see the runtime code layout. The `G` numbers for injected lines are shown for reference, but the BP hint uses **base-source coordinates** (subtracting the injected ENHO lines) because ADT validates breakpoints against the non-injected `/source/main`:

```
  * ---- Method: COMPUTE (CM001) — breakpoint: debug set --objects ZCL_CAIS_DBG_TRIGGER:33 ----
    29 [  1]    METHOD compute.
    30 [  2]  *"* ENHO: ZCAIS_DBG_ENHO (BEGIN)
    31 [  3]    DATA lv_enho_marker TYPE i.
    32 [  4]    lv_enho_marker = iv_a * iv_b.
    33 [  5]  *"* ENHO END
    34 [  6]      " compute: add two integers...
    35 [  7]      DATA lv_sum TYPE i.
    36 [  8]
    37 [  9]      lv_sum = iv_a + iv_b.       ← lives at G=37 in runtime layout
    38 [ 10]      rv_result = lv_sum.
    39 [ 11]    ENDMETHOD.
```

The first executable original line is `lv_sum = iv_a + iv_b.` at runtime G=37, but its position in ADT's base source (without the 4 injected ENHO lines) is G=33 (37 − 4). The hint says `:33`, not `:37`. ADT accepts `:33` and the breakpoint fires correctly inside the ENHO-affected method.

> **ENHO body lines (G 30–32 in the example) cannot be used as breakpoints.** Setting a BP at those G numbers returns "Not registered on server" — ADT's base source has no ENHO lines. The BP hint already points past the ENHO block to the first original executable line.

**Two scenarios for CM* methods:**

| Scenario | Command |
|---|---|
| Source available locally (your own classes) | `debug set --files src/zcl_my_class.clas.abap:9` |
| No local source (abapGit library, SAP standard) | `debug set --objects ZCL_MY_CLASS:9` |

```bash
abapgit-agent debug set --objects ZCL_MY_CLASS:9
abapgit-agent debug list    # confirm it was registered
```

**Unit test methods (CCAU section)** and **local class methods (CCIMP section)** live in separate ADT includes — they cannot be addressed by the assembled-source global line. Their sections show section-local line numbers with a `--include` hint per method:

```
  * ---- Section: Unit Test (from .clas.testclasses.abap) ----
  * ---- Method: SETUP — breakpoint: debug set --objects ZCL_MY_CLASS:12 --include testclasses ----
    10    METHOD setup.
    11      DATA lv_x TYPE i.
    12      mo_cut = NEW #( ).
    13    ENDMETHOD.
  * ---- Method: TEST_PULL_SUCCESS — breakpoint: debug set --objects ZCL_MY_CLASS:18 --include testclasses ----
    15    METHOD test_pull_success.
    ...

  * ---- Section: Local Implementations (from .clas.locals_imp.abap) ----
  * ---- Method: ZIF_FOO~DO_SOMETHING — breakpoint: debug set --objects ZCL_MY_CLASS:5 --include locals_imp ----
    3     METHOD zif_foo~do_something.
    4       DATA lv_x TYPE i.
    5       lv_x = iv_input.
    6     ENDMETHOD.
```

The line numbers in these sections are **section-local** (same coordinate system as the `.clas.testclasses.abap` / `.clas.locals_imp.abap` file). Use the `--include` flag to target the correct ADT sub-include:

| Section | `--include` value |
|---|---|
| Unit Test (`.clas.testclasses.abap`) | `testclasses` |
| Local Implementations (`.clas.locals_imp.abap`) | `locals_imp` |
| Local Definitions (`.clas.locals_def.abap`) | `locals_def` |

```bash
# Unit test method:
abapgit-agent debug set --objects ZCL_MY_CLASS:12 --include testclasses
# Local class method:
abapgit-agent debug set --objects ZCL_MY_CLASS:5 --include locals_imp
abapgit-agent debug list    # confirm both were registered
```

**Function module (FUGR) methods** live in per-FM source includes (`L<GROUP>U<NN>`). Use `view` with `--full --fm <name> --lines` to see the source and breakpoint hint:

```bash
abapgit-agent view --objects SUSR --type FUGR --full --fm AUTHORITY_CHECK --lines
```

```
  * ---- FM: AUTHORITY_CHECK (LSUSRU04) — breakpoint: debug set --objects LSUSRU04:50 ----
     1  function authority_check.
    ...
    36    constants:
    ...
    45    data:
    ...
    50    ld_syuname = cl_abap_syst=>get_user_name( ).
```

```bash
abapgit-agent debug set --objects LSUSRU04:50
abapgit-agent debug list    # confirm it was registered
```

> **Line number must point to an executable statement.** The hints already skip `METHOD`, blank lines, comments (`"`, `*`), and all declaration forms (`DATA`, `DATA:`, `DATA(`). One case still requires manual attention:
>
> **Multi-line inline `DATA(x) = call(`** — the ABAP debugger treats the whole expression as a declaration. Set the breakpoint on the **next standalone statement** after the closing `).`:
> ```
>  100 [  1]    DATA(ls_checks) = prepare_deserialize_checks(   ← NOT valid (inline decl)
>  101 [  2]      it_files = it_files                           ← NOT valid (continuation)
>  104 [  5]      io_repo_desc = lo_repo_desc1 ).               ← NOT valid (continuation)
>  106 [  7]    mo_repo->create_new_log( ).                     ← valid ✅ (use global 106)
>  ```

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

> **If the stale debug daemon holds an ABAP lock** (symptom: `Requested object EZABAPGIT is currently locked by user <USER>`):
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

