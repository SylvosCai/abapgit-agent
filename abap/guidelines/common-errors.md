---
layout: default
title: Common ABAP Errors
nav_order: 12
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Common ABAP Errors - Quick Fixes

**Searchable keywords**: error, syntax error, field mismatch, fixed point, comma, host variable, @ prefix, type incompatible, string template, expression limiter, literal brace, http 500, xml metadata mismatch, interface description, descript, recompilation, offset notation, cond trap, cx_sy_range_out_of_bounds

## Fixed Point Arithmetic Error

```
This ABAP SQL statement uses additions that can only be used when
the fixed point arithmetic flag is activated
```

**Fix**: Add `<FIXPT>X</FIXPT>` to class XML.

```xml
<VSEOCLASS>
  ...
  <FIXPT>X</FIXPT>
</VSEOCLASS>
```

---

## Field Name Mismatch

```
The field 'CARRIERID' does not have a corresponding field in the work area
```

**Fix**: Match structure field names with CDS view.

Check actual names: `abapgit-agent preview --objects <VIEW> --limit 1`

**Better fix**: Use CDS as type: `TYPE STANDARD TABLE OF zc_view WITH DEFAULT KEY`

---

## Missing Commas in SELECT

```
The elements in the 'SELECT LIST' list must be separated using commas
```

**Fix**: `SELECT carrid, connid, fldate` (add commas between fields)

---

## Missing @ Escape

```
Either 'LT_RESULT' has to be escaped using '@' or qualified by a table name
```

**Fix**: `INTO TABLE @lt_result WHERE field = @lv_param` (add @ prefix)

---

## Type Incompatible

```
The data type of the component 'OCCUPANCYPERCENT' is not compatible
```

**Fix for calculated CDS fields**: Use `TYPE decfloat34`

**Fix for other fields**: Use correct data element.

Find: `abapgit-agent view --objects <TABLE> --type TABL`

---

## Using Raw Types

```abap
❌ carrierid TYPE c LENGTH 3,
✅ carrierid TYPE s_carr_id,
```

**Rule**: Always use data elements, never raw types (c, n, p without reference).

Find data elements: `abapgit-agent view --objects <TABLE> --type TABL`

---

## Inline Declaration from String Literal — Silent Truncation

**Symptom**: A `&&` concatenation has no effect; the variable retains its initial value.

**Root cause**: `DATA(var) = 'literal'` infers type `C LENGTH N` (N = literal length).
Any subsequent `&&` computes the correct longer string but truncates it back to N chars.
The abaplint `prefer_inline` quickfix can introduce this bug automatically.

```abap
* WRONG — lv_response stays '{"success":"X",' after && (16 chars, always truncated)
DATA(lv_response) = '{"success":"X",'.
lv_response = lv_response && '"key":"value"}'.   " no effect!

* CORRECT — use string template
DATA(lv_key) = condense( val = CONV string( li_repo->get_key( ) ) ).
rv_result = |\{"success":"X","key":"{ lv_key }"\}|.

* ALSO CORRECT — explicit TYPE string
DATA lv_response TYPE string.
lv_response = '{"success":"X",'.
lv_response = lv_response && '"key":"value"}'.   " works correctly
```

**Fix**: Replace the inline declaration + `&&` chain with a string template,
or declare the variable explicitly with `TYPE string`.

→ See `abaplint.md` for full guidance on the `prefer_inline` rule.

---

## Offset Notation on String in COND — CX_SY_RANGE_OUT_OF_BOUNDS

**Symptom**: Pull succeeds but ABAP crashes at runtime with `CX_SY_RANGE_OUT_OF_BOUNDS`
inside a method using `COND`.

**Root cause**: Offset/length notation on a `string` variable (`str+off`, `str(len)`)
**cannot be used as a result expression inside `COND`** — even with an explicit `COND string(...)` type.
The abaplint `prefer_inline` quickfix can introduce this automatically when converting
an `IF/ELSE` block that assigns from a string-variable offset.

```abap
* WRONG — crashes at runtime with CX_SY_RANGE_OUT_OF_BOUNDS
DATA(lv_path) = COND string(
  WHEN lv_pos > 0 THEN '/' && lv_file(lv_pos + 1)   " ← string offset in COND
  ELSE '/' ).

* CORRECT — use substring() instead
DATA(lv_path) = COND string(
  WHEN lv_pos > 0 THEN '/' && substring( val = lv_file len = lv_pos + 1 )
  ELSE '/' ).
```

**Fix**: Replace `str+off` / `str(len)` inside `COND` results with `substring( val = str off = off )` or
`substring( val = str len = len )`, or revert to the `IF/ELSE` form with `DATA x TYPE string`.

→ See `abaplint.md` for the full safe/unsafe table for `prefer_inline`.

---

## XML Metadata Mismatch — HTTP 500 on Pull

**Symptom**: `pull` returns HTTP 500 with no short dump (ST22) for the request user.
Other objects pull fine; only a specific interface or class fails.

**Root cause**: The `.intf.xml` or `.clas.xml` file in git has a metadata field (e.g. `DESCRIPT`,
`UNICODE`, `STATE`) that differs from the active ABAP system value. abapGit writes the git
value back to VSEOINTERF/VSEOCLASS, which forces the SAP class framework to recompile all
implementing/inheriting objects. If any implementing class has a syntax or generation
issue with the current active source, the recompilation crashes outside the normal exception
handler — producing HTTP 500 with no dump.

**Diagnosis**:
1. No dump in ST22 for your user → not a user-code crash
2. Only one specific INTF/CLAS fails → metadata delta in its XML triggers recompilation
3. Check what differs: `abapgit-agent view --objects ZIF_MY_INTF --type INTF` and compare
   `DESCRIPT` / other fields with the XML file in git

**Fix**: Sync the XML to match the ABAP system value. The easiest way:

```bash
# Pull without --sync-xml first to let abapGit rewrite the XML from the system
abapgit-agent pull --files abap/zif_my_intf.intf.abap --sync-xml
```

Or manually correct the field in the XML file to match the system value, then push and pull.

**Prevention**: Always use `--sync-xml` after pulling new objects so the XML stays in sync
with what the ABAP serializer produces.

---

**Error**: `Expression limiter '{' in string template not followed by space`

Literal `{` and `}` (e.g. in JSON payloads) must be escaped as `\{` and `\}` inside `|...|`.

```abap
" WRONG — unescaped { in JSON triggers parse error
rv_result = |{"success":"X","name":"{ lv_name }"}|.

" CORRECT
rv_result = |\{"success":"X","name":"{ lv_name }"\}|.
```

→ Full escaping rules and examples: `abapgit-agent ref --topic string-template`

---

## See Also
- **ABAP SQL** (sql.md) - for SQL syntax rules
- **CDS Views** (cds.md) - for CDS selection patterns
- **abapGit** (abapgit.md) - for XML metadata templates
- **abaplint** (abaplint.md) - for abaplint rule guidance and known quickfix traps
