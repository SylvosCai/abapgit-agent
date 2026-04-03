---
layout: default
title: abaplint Local Rules
nav_order: 17
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abaplint Local Rules — abapgit-agent project

**Searchable keywords**: naming, local_variable_names, method_parameter_names,
prefix, lv, lt, ls, lo, li, lx, iv, it, is, io, rv, rs, rt, ro

This project enforces **type-specific Hungarian notation** via `local_variable_names`
and `method_parameter_names` in `.abaplint.json`.

---

## Variable Naming — Required Prefixes

### Local Variables (inside methods)

| Prefix | Type | Example |
|--------|------|---------|
| `lv_` | Scalar / value (i, string, char, …) | `lv_count TYPE i` |
| `lt_` | Internal table | `lt_files TYPE ty_files` |
| `ls_` | Structure | `ls_result TYPE ty_result` |
| `lo_` | Object reference | `lo_agent TYPE REF TO zcl_abgagt_agent` |
| `li_` | Interface reference | `li_repo TYPE REF TO zif_abapgit_repo` |
| `lx_` | Exception reference | `lx_error TYPE REF TO cx_static_check` |
| `lr_` | Data reference | `lr_data TYPE REF TO data` |
| `lc_` | Constant | `lc_max TYPE i VALUE 100` |

### Field-Symbols (inside methods)

| Prefix | Example |
|--------|---------|
| `<lv_>`, `<lt_>`, `<ls_>`, `<lo_>`, `<li_>` | `FIELD-SYMBOLS <ls_item> TYPE ty_item` |
| `<comp>` | Allowed for generic component iteration |

### Method Parameters

| Direction | Prefix | Type |
|-----------|--------|------|
| IMPORTING | `iv_` | scalar |
| IMPORTING | `it_` | table |
| IMPORTING | `is_` | structure |
| IMPORTING | `io_` | object ref |
| IMPORTING | `ii_` | interface ref |
| IMPORTING | `ix_` | exception ref |
| RETURNING | `rv_` | scalar |
| RETURNING | `rs_` | structure |
| RETURNING | `rt_` | table |
| RETURNING | `ro_` | object ref |
| RETURNING | `ri_` | interface ref |
| RETURNING | `rr_` | data ref |
| EXPORTING | `ev_`, `es_`, `et_` | scalar / structure / table |
| CHANGING  | `cv_`, `cs_`, `ct_` | scalar / structure / table |

---

## Quick Reference

```abap
" Local variables
DATA lv_name    TYPE string.
DATA lt_files   TYPE ty_files.
DATA ls_result  TYPE ty_result.
DATA lo_agent   TYPE REF TO zcl_abgagt_agent.
DATA li_repo    TYPE REF TO zif_abapgit_repo.
DATA lx_error   TYPE REF TO cx_static_check.
FIELD-SYMBOLS <ls_item> TYPE ty_item.

" Method signature
METHODS process
  IMPORTING
    iv_url    TYPE string
    it_files  TYPE ty_files
    is_config TYPE ty_config
    io_agent  TYPE REF TO zcl_abgagt_agent
    ii_repo   TYPE REF TO zif_abapgit_repo
  RETURNING
    VALUE(rv_result) TYPE string.

METHODS get_repo
  RETURNING
    VALUE(ro_repo) TYPE REF TO zcl_abgagt_agent.
```

---

## Rule: Never Use Generic `lv_` for Objects, Tables, or Structures

```abap
" WRONG — abaplint will flag these
DATA lv_repo    TYPE REF TO zcl_abgagt_agent.   " use lo_
DATA lv_files   TYPE ty_files.                   " use lt_
DATA lv_result  TYPE ty_result.                  " use ls_

" CORRECT
DATA lo_repo    TYPE REF TO zcl_abgagt_agent.
DATA lt_files   TYPE ty_files.
DATA ls_result  TYPE ty_result.
```

---

## See Also

- `.abaplint.json` — rule definitions (`local_variable_names`, `method_parameter_names`)
- `guidelines/abaplint.md` — bundled guidance on `prefer_inline` trap
