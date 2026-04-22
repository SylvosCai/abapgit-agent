---
layout: default
title: run Command Guide
nav_order: 23
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# run Command — AI Guidelines

## Never Run Proactively

`abapgit-agent run` executes live ABAP code. **Never call it unless the user explicitly asks.**

A class implementing `IF_OO_ADT_CLASSRUN` can do anything — modify database records, send emails, trigger RFCs. The interface signature gives no indication of side effects.

```
User: "Write a class that reads flight data and prints it"
→ ✓ Create the class, pull it, STOP. Do NOT run it.
→ ✓ Tell the user: "Class is activated. Run with: abapgit-agent run --class ZCL_MY_CLASS"

User: "Now run it"
→ ✓ Run it
```

**For ABAP programs (PROG type)** — use `--program` instead of `--class`:

```bash
abapgit-agent run --program ZR_MY_REPORT
```

`--program` works identically to `--class` for programs that implement `IF_OO_ADT_CLASSRUN`. Use it whenever the object type is PROG, not CLAS.

---

## Writing a Runner Class (`IF_OO_ADT_CLASSRUN`)

`out->write()` accepts any ABAP data object and formats it automatically — no manual `WRITE` statements needed.

```abap
CLASS zcl_my_runner DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_my_runner IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    " Scalar
    out->write( 'Hello!' ).

    " Internal table — rendered as column headers + one row per entry
    SELECT carrid, connid, price FROM sflight INTO TABLE @DATA(lt_flights).
    out->write( data = lt_flights name = 'Flights' ).
  ENDMETHOD.
ENDCLASS.
```

**Output format (verified on live system):**

| Call | Output |
|------|--------|
| `out->write( 'text' )` | `text` |
| `out->write( lv_int )` | `42` |
| `out->write( data = ls_struc name = 'Label' )` | `Label` + field-name headers + one data row |
| `out->write( data = lt_itab name = 'Label' )` | `Label` + field-name headers + one row per table entry |

The `name =` parameter is optional — it adds a label above the output. Structures and internal tables are rendered as a columnar table with ABAP field names as headers.

---

## Probe Classes and `scratchWorkspace`

### Decision flow

```
User asks to create a probe class
├── disableProbeClasses = false / not set  →  create in current project (default)
└── disableProbeClasses = true
    ├── scratchWorkspace configured  →  create there (see workflow below)
    └── scratchWorkspace not configured  →  refuse, guide user to set it up
```

### When `disableProbeClasses = true` and `scratchWorkspace` is configured

**Naming** — derive from `scratchWorkspace` config in `.abapGitAgent`:
- `classPrefix` (default: `ZCL_{USER}_`) + `<PURPOSE>`, max 30 chars
- Example: user=`JOHN`, purpose=`OPEN_TRANSPORTS` → `ZCL_JOHN_OPEN_TRANSPORTS`
- If name already exists in `{path}/src/`, append `_2`, `_3`, etc.

**Workflow:**
1. Read `{path}/.abapGitAgent` to confirm `folder` property (e.g. `/src/`)
2. Write class files in `{path}/src/`
3. Commit and push from `{path}`:
   ```bash
   cd {path} && git add . && git commit -m "probe: <description>" && git push
   ```
4. Activate:
   ```bash
   cd {path} && abapgit-agent pull --files src/<classname>.clas.abap
   ```
5. Tell user (do NOT auto-run):
   ```
   Class activated. Run with: abapgit-agent run --class <CLASSNAME>
   ```
   Run the command from the original project directory, not `{path}`.

### When `disableProbeClasses = true` and `scratchWorkspace` is NOT configured

Refuse and tell the user to configure `scratchWorkspace` in `.abapGitAgent`:

```json
{
  "scratchWorkspace": {
    "path": "/absolute/path/to/scratch-repo"
  }
}
```

The path must point to a separate git repo initialized with `abapgit-agent init --package <SCRATCH_PACKAGE>`.
