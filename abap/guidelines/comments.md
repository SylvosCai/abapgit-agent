---
layout: default
title: Documentation Comments
nav_order: 22
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP Documentation Comments

**Searchable keywords**: ABAP DOC, "! comment, shorttext synchronized, @parameter, @raising, inline comment, CDS comment, // comment, program header, *&---, documentation comment

## TOPICS IN THIS FILE
1. Two Types of Comments - line 18
2. OO Class Documentation (CLAS) - line 37
3. Interface Documentation (INTF) - line 101
4. Program Header (PROG) - line 120
5. CDS View Comments (DDLS) - line 137
6. When NOT to Comment - line 161
7. Quick Decision Table - line 182

---

## 1. Two Types of Comments

ABAP has two distinct comment styles with different purposes:

**`"!` (ABAP DOC)** — parsed by the ABAP system; surfaced in ADT and SE24 as F2 help.
Use for API-level documentation: class declarations, interface declarations, and method signatures.
This is the equivalent of Javadoc. The system reads it — it is metadata, not decoration.

**`"!` is only valid immediately before these statements:**
`CLASS ... DEFINITION`, `INTERFACE`, `METHODS`, `CLASS-METHODS`, `EVENTS`, `DATA`, `CLASS-DATA`, `CONSTANTS`.

**`"!` before `TYPES` is a syntax error** — the Code Inspector reports "ABAP Doc comment is in the wrong position."
Use a regular `"` comment to describe type structures instead.

**`"` (regular inline comment)** — source-only; not parsed by the ABAP system.
Use inside method bodies to explain non-obvious logic, and to label `TYPES` blocks.
Never use `"!` inside method implementations — it has no effect there.

**Default behaviour (when to add automatically vs. only on request):**

| Comment Type | Default Behaviour |
|---|---|
| `"! <p class="shorttext synchronized">` on CLASS/INTF | **Always auto-add** — synced to XML `<DESCRIPT>`; it IS object metadata |
| `@EndUserText.label` on CDS view | **Always auto-add** — same: it IS the CDS description |
| `"!` + `@parameter` on INTF public methods | **Always auto-add** — interfaces are the contract; self-documenting |
| `"!` + `@parameter` on CLAS public methods | **Auto-add** when creating a new class from scratch |
| `"!` on existing CLAS private/protected methods | **Only if requested** — adding to existing code is refactoring |
| `"` inline comments inside method bodies | **Only when non-obvious** — never add redundant comments |
| `*&---` program header | **Always auto-add** — standard for every PROG |

---

## 2. OO Class Documentation (CLAS)

### Class-level shorttext

Place the `"!` shorttext immediately before `CLASS ... DEFINITION`:

```abap
"! <p class="shorttext synchronized">Pull ABAP objects from a remote git repository</p>
CLASS zcl_abgagt_agent DEFINITION PUBLIC FINAL CREATE PUBLIC.
```

This shorttext is **synced to the XML `<DESCRIPT>` field** by abapGit. Creating a class without
it means the XML description is blank and the object cannot be searched by description in SE24/SE80.
**Always add it** when creating any new class.

**Note on `*"*` auto-header:** When a class has local definitions or inherits from another class,
abapGit writes a 3-line `*"*"` block at the top of the `.clas.abap` file:

```abap
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* temporary on-the-fly implementation
```

In that case, the `"!` shorttext goes on line 4 (after the block), not line 1.

### Method documentation

Place `"!` documentation in the DEFINITION section, PUBLIC SECTION only, immediately before the
`METHODS` statement:

```abap
"! Pull files from remote and activate them
"! @parameter it_files | List of files to activate (relative paths)
"! @parameter rv_success | True if all objects activated successfully
"! @raising zcx_abapgit_exception | If pull or activation fails
METHODS pull_files
  IMPORTING it_files          TYPE string_table
  RETURNING VALUE(rv_success) TYPE abap_bool
  RAISING   zcx_abapgit_exception.
```

**Rules:**

- **ALWAYS add** `"! <p class="shorttext synchronized">` when creating any new CLASS — no exceptions.
- **ALWAYS add** `"!` + `@parameter` for all PUBLIC methods when creating a new class from scratch.
- When **modifying existing** code, only add or update comments if explicitly asked.
- `@parameter <name> | <description>` — one line per IMPORTING/EXPORTING/RETURNING/CHANGING param.
- `@raising <exception> | <reason>` — one line per RAISING exception.
- PRIVATE and PROTECTED methods: optional; add only if the purpose is non-obvious.
- Place ONLY in the DEFINITION section — do NOT repeat `"!` in the IMPLEMENTATION section.

**Full example:**

```abap
"! <p class="shorttext synchronized">Syntax checker for ABAP class source</p>
CLASS zcl_abgagt_syntax_chk_clas DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abgagt_syntax_checker.

    "! Create a new syntax checker instance
    "! @parameter ii_agent | Agent used for HTTP communication
    CLASS-METHODS create
      IMPORTING ii_agent        TYPE REF TO zif_abgagt_agent
      RETURNING VALUE(ro_check) TYPE REF TO zif_abgagt_syntax_checker.

  PRIVATE SECTION.
    DATA mi_agent TYPE REF TO zif_abgagt_agent.
ENDCLASS.
```

---

## 3. Interface Documentation (INTF)

Interfaces follow the same `"!` pattern as classes. The shorttext goes immediately before
`INTERFACE ... PUBLIC`, and every method gets `"!` + `@parameter`.

```abap
"! <p class="shorttext synchronized">Contract for all syntax checker implementations</p>
INTERFACE zif_abgagt_syntax_checker PUBLIC.

  "! Check the syntax of ABAP source code
  "! @parameter iv_source | Full ABAP source text to check
  "! @parameter rv_result | JSON-encoded result with errors list
  "! @raising zcx_abapgit_exception | On communication failure
  METHODS check_syntax
    IMPORTING iv_source        TYPE string
    RETURNING VALUE(rv_result) TYPE string
    RAISING   zcx_abapgit_exception.

ENDINTERFACE.
```

**Canonical example:** `zif_abgagt_syntax_checker.intf.abap` in this repository.

**Rules:**
- **ALWAYS add** `"! <p class="shorttext synchronized">` for every new interface.
- **ALWAYS add** `"!` + `@parameter` for every method — interfaces are the published contract.
- No exceptions for "obvious" method names — document them anyway.

---

## 4. Program Header (PROG)

Programs use the traditional `*&---` block, NOT ABAP DOC. Place it before the `REPORT` statement:

```abap
*&---------------------------------------------------------------------*
*& Report Z_MY_PROGRAM
*&---------------------------------------------------------------------*
*& Brief description of what the program does
*&---------------------------------------------------------------------*
REPORT z_my_program.
```

**Rules:**
- **ALWAYS add** this header block when creating any new program.
- Do NOT add `"! <p class="shorttext synchronized">` before `REPORT` — it is not parsed there.
- Inline comments inside `START-OF-SELECTION` and other events use the regular `"` style.

---

## 5. CDS View Comments (DDLS)

CDS DDL source (`.ddls.asddls`) uses a completely different comment syntax from ABAP:

- Line comment: `// text`
- Block comment: `/* text */`

**Never use ABAP-style `"` inside CDS source** — it causes a CDS syntax error.

### `@EndUserText.label` — the CDS equivalent of shorttext

This annotation IS the object description. Always include it before the view entity definition:

```cds
@EndUserText.label: 'Revenue summary by carrier'
define view entity ZC_FlightRevenue
  as select from sflight
{
  key carrid,         // IATA carrier code
  key connid,         // Connection number
  sum( price ) as TotalRevenue
}
```

**Rules:**
- **ALWAYS add** `@EndUserText.label` when creating any new CDS view entity.
- Add `// text` line comments after non-obvious field names.
- Use `/* ... */` for multi-line block comments if needed (e.g., explaining a complex join).
- No `"!` or `"` comments anywhere in `.ddls.asddls` files.

---

## 6. When NOT to Comment

The guiding principle: **comments explain *why*, not *what***.

```abap
" BAD — repeats the code, adds no value
ls_params-mode = 'abort'.   " Set mode to abort

" GOOD — explains a non-obvious rule
ls_params-mode = 'abort'.   " INITIAL means abort (caller omitting the field = abort semantics)
```

**Do NOT add comments when:**
- The comment would just restate the variable name or method call.
- A private helper method simply delegates to an interface method (the name is self-explanatory).
- `@parameter` description would be identical to the parameter name (e.g., `"! @parameter iv_name | Name`).
- You are modifying existing code and the task did not ask for documentation changes.

**DO add comments when:**
- The logic involves a non-obvious ABAP-specific behaviour or workaround.
- There is a caller precondition that cannot be expressed in the method signature.
- A COND/SWITCH mapping table needs to explain the business rule behind each branch.
- A loop exit or CONTINUE has a non-obvious condition.

---

## 7. Quick Decision Table

| Object | Where | Style | Required? |
|--------|-------|-------|-----------|
| CLAS (global) | Before `CLASS DEFINITION` | `"! <p class="shorttext synchronized">` | **Always** |
| CLAS public method | Before `METHODS` in DEFINITION | `"! desc` + `"! @parameter` | Yes (new objects) |
| CLAS private/protected method | Before `METHODS` in DEFINITION | `"! desc` | Only if non-obvious |
| INTF method | Before `METHODS` in interface | `"! desc` + `"! @parameter` | **Always** |
| PROG | First lines before `REPORT` | `*&---` block | **Always** |
| DDLS view entity | Before `define view entity` | `@EndUserText.label` annotation | **Always** |
| DDLS field | After field name | `// text` | For non-obvious fields |
| Method body | Inside `METHOD...ENDMETHOD` | `" text` | Only for non-obvious logic |
| TYPES block | Before `TYPES:` in interface/class | `" text` | Optional — `"!` is invalid here |
