# Debug Findings: How abapGit Decides Which Files to Pull

**Date:** 2026-03-08
**Branch:** feature/debug-command
**Method used:** `abapgit-agent debug` with breakpoints in `ZCL_ABGAGT_AGENT:291`

---

## How it Works (Confirmed by Debugger)

The entry point is `prepare_deserialize_checks` in `zcl_abgagt_agent.clas.abap:261`.

### Step 1 — `deserialize_checks()` returns all repo objects

`mo_repo->deserialize_checks()` returns **all 70 repository objects** in `rs_checks-overwrite`.
Each entry has:

| Field | Observed value | Meaning |
|-------|---------------|---------|
| `OBJ_TYPE` | `CLAS`, or empty | Object type |
| `OBJ_NAME` | `ZCL_ABGAGT_AGENT`, `ZCL_ABGAGT_BG_DECISION`, or empty | Object name |
| `STATE` | `M_` | Modified (differs from remote) |
| `ACTION` | `3` | abapGit's planned action (overwrite/update) |
| `DECISION` | `` (empty at this point) | We set this in the loop |
| `INACTIVE` | `` | Whether the object is currently inactive |

All 70 objects show `STATE='M_'` because abapGit compares the git remote to the ABAP system — on a feature branch, everything looks modified.

### Step 2 — We loop and set DECISION

`prepare_deserialize_checks` (lines 291–307):
1. Parses each `--files` argument to `(OBJ_TYPE, OBJ_NAME)` — e.g. `abap/zcl_abgagt_agent.clas.abap` → `CLAS / ZCL_ABGAGT_AGENT`
2. Builds `LT_VALID_FILES` hashed table (1 entry in this test)
3. Loops all 70 overwrite entries and sets:
   - Matching object → `DECISION = c_yes` (`'Y'`)
   - Everything else → `DECISION = c_no` (`'N'`)

### Step 3 — abapGit deserializes only `DECISION='Y'` objects

Confirmed by pull log: `>>> Deserializing 1 object` when `--files` specified one file.

---

## Bugs / Issues Found

### Bug 1 — Empty OBJ_TYPE/OBJ_NAME entries get `DECISION='N'`

**Location:** `zcl_abgagt_agent.clas.abap:291–306` (the decision loop)

The first row in `rs_checks-overwrite` had **empty** `OBJ_TYPE` and `OBJ_NAME`. These appear to be internal header/metadata rows that abapGit uses internally. Our code sets `DECISION='N'` on them, which may suppress a processing step abapGit expects to always run.

**Debugger evidence:**
```
Iteration 1: OBJ_TYPE='', OBJ_NAME='', DECISION set to 'N'   ← first row, empty
Iteration 2: OBJ_TYPE='CLAS', OBJ_NAME='ZCL_ABGAGT_AGENT', DECISION set to 'Y'
Iteration 4: OBJ_TYPE='CLAS', OBJ_NAME='ZCL_ABGAGT_BG_DECISION', DECISION set to 'N'
```

**Fix:** Add a guard in the loop — skip setting `DECISION` for entries where `OBJ_TYPE` or `OBJ_NAME` is initial:

```abap
LOOP AT rs_checks-overwrite INTO DATA(ls_overwrite).
  " Skip internal/header rows with no object identity
  IF ls_overwrite-obj_type IS INITIAL OR ls_overwrite-obj_name IS INITIAL.
    CONTINUE.
  ENDIF.
  ...
```

---

### Bug 2 — Silent failure when `--files` entry doesn't match any overwrite entry

**Location:** `zcl_abgagt_agent.clas.abap:272–288` (file-to-object parsing loop)

If a user passes a filename that parses to an `(OBJ_TYPE, OBJ_NAME)` pair not present in the overwrite table (e.g. a new object not yet pushed, a typo, or an unsupported file type), the filtering silently skips it. The pull succeeds with zero objects activated and no warning.

**Example:**
```bash
abapgit-agent pull --files abap/zcl_nonexistent.clas.abap
# → Pull completed successfully, 0 objects activated — no warning
```

**Fix:** After the decision loop, check that every entry in `LT_VALID_FILES` was actually found in the overwrite table, and log a warning for any that weren't:

```abap
" After decision loop: warn about files not found in overwrite table
LOOP AT lt_valid_files INTO DATA(ls_requested).
  READ TABLE rs_checks-overwrite WITH KEY obj_type = ls_requested-obj_type
                                          obj_name = ls_requested-obj_name
                                  TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    " Log warning: requested object not in deserialize_checks result
    ...
  ENDIF.
ENDLOOP.
```

---

### Bug 3 — `parse_file_to_object` doesn't handle `.clas.xml` or `.intf.xml` files

**Location:** `zcl_abgagt_agent.clas.abap:204–259`

The parser requires the last extension to be `.abap` or `.asddls` (line 226). If a user passes an XML metadata file (e.g. `zcl_foo.clas.xml`), it returns empty and the file is silently ignored. This is probably fine since XML files don't map to independent objects, but it could be confusing.

No fix strictly needed — could add a comment clarifying this is intentional.

---

### Observation — All objects always marked `STATE='M_'`

All 70 objects in `rs_checks-overwrite` had `STATE='M_'` (modified). This is because abapGit fetches the remote git state and compares it to the ABAP system. On a feature branch, all objects diverge from the ABAP system's version (which tracks a different branch), so everything appears modified.

This means:
- The `--files` filter is the **only** thing preventing mass activation
- `deserialize_checks()` always does a full remote fetch regardless of how many files are requested — no optimization possible here (it's internal to abapGit)

---

## Files & Lines Referenced

| File | Lines | Topic |
|------|-------|-------|
| `abap/zcl_abgagt_agent.clas.abap:261` | `prepare_deserialize_checks` method start |
| `abap/zcl_abgagt_agent.clas.abap:262` | `deserialize_checks()` call |
| `abap/zcl_abgagt_agent.clas.abap:272` | File-to-object parsing loop |
| `abap/zcl_abgagt_agent.clas.abap:291` | Decision loop (breakpoint used) |
| `abap/zcl_abgagt_agent.clas.abap:204` | `parse_file_to_object` method |
