---
layout: default
title: abaplint Rule Guidelines
nav_order: 17
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# abaplint Rule Guidelines

**Searchable keywords**: abaplint, prefer_inline, inline declaration, char literal, string truncation,
no_inline_in_optional_branches, fully_type_constants, linting, static analysis

This file covers rules that have **non-obvious or dangerous implications** — cases where applying
a rule mechanically (or accepting its quickfix) can introduce subtle bugs.

For project-specific rule guidance, add a `guidelines/abaplint-local.md` file to the project
repository. The `ref` command searches both bundled and project guidelines automatically.

---

## prefer_inline — Inline Declarations

### What the rule does

Flags up-front `DATA` declarations and suggests replacing them with inline `DATA(var) = expr`:

```abap
* Bad (flagged by rule)
DATA lv_count TYPE i.
lv_count = lines( lt_table ).

* Good (preferred by rule)
DATA(lv_count) = lines( lt_table ).
```

This is safe when the RHS expression is a **function call, method call, or constructor
operator** — because the return type is fully defined.

### The char-literal trap — NEVER apply the quickfix here

```
❌ DANGEROUS: DATA(var) = 'literal'.
```

When the RHS is a quoted string literal, ABAP infers type `C LENGTH N` where N equals
the exact character count of the literal. Any subsequent `&&` concatenation computes the
correct longer string but **silently truncates it back to N characters**. The variable
never grows beyond the length of its initial value.

**This is the most dangerous quickfix the rule offers — it changes the runtime type.**

```abap
* WRONG — produced by prefer_inline quickfix, causes silent truncation
DATA(lv_response) = '{"success":"X",'.      " → TYPE C LENGTH 16
lv_response = lv_response && '"key":"val"'. " computed correctly, truncated to 16 chars
" lv_response is STILL '{"success":"X",' — the && had no effect

* CORRECT — use a string template to build the full value in one step
DATA(lv_key) = condense( val = CONV string( li_repo->get_key( ) ) ).
rv_result = |\{"success":"X","key":"{ lv_key }"\}|.

* ALSO CORRECT — explicit TYPE string, safe to concatenate
DATA lv_response TYPE string.
lv_response = '{"success":"X",'.
lv_response = lv_response && '"key":"val"}'.
```

### Safe vs unsafe patterns

| Pattern | Safe? | Why |
|---|---|---|
| `DATA(n) = lines( lt_tab ).` | ✅ | Return type `I` — no truncation risk |
| `DATA(lo) = NEW zcl_foo( ).` | ✅ | Object reference — fully typed |
| `DATA(ls) = CORRESPONDING #( ls_src ).` | ✅ | Inherits structure type |
| `DATA(lv) = lv_other.` | ✅ | Inherits type from source variable |
| `SELECT ... INTO TABLE @DATA(lt).` | ✅ | Type from DB dictionary |
| `DATA(lv) = 'literal'.` followed by `&&` | ❌ | Infers `C LENGTH N`, truncates |
| `DATA(lv) = 'literal'.` used only in `\|{ lv }\|` | ⚠️ | Technically works but misleading — prefer explicit type |
| `DATA(lv) = 'X'.` used as abap_bool flag | ✅ | `C LENGTH 1` is correct for flags |

### Rule of thumb

> If the inline-declared variable will ever appear on the left side of `&&`,
> or be passed to a parameter typed `TYPE string`, declare it explicitly:
> `DATA lv_foo TYPE string.`

---

## no_inline_in_optional_branches

### What the rule does

Flags inline `DATA(var)` declarations inside `IF`, `CASE/WHEN`, `LOOP`, `WHILE`, `DO`,
and `SELECT` loops — branches that may not execute, leaving the variable uninitialized
when code after the branch reads it.

```abap
* Bad (flagged)
LOOP AT lt_items INTO DATA(ls_item).
  DATA(lv_key) = ls_item-key.   " declared inside LOOP — only set when loop runs
ENDLOOP.
WRITE lv_key.                   " undefined if lt_items was empty

* Good
DATA lv_key TYPE string.
LOOP AT lt_items INTO DATA(ls_item).
  lv_key = ls_item-key.
ENDLOOP.
WRITE lv_key.
```

**Exception**: `TRY/CATCH/CLEANUP` is explicitly NOT considered an optional branch by
the rule — inline declarations inside `TRY` are allowed.

### When you see this rule triggered

Move the `DATA(var)` declaration out of the branch to the top of the method, giving it
an explicit type:

```abap
* Before (flagged)
IF condition.
  DATA(lv_result) = compute( ).
ENDIF.

* After (clean)
DATA lv_result TYPE string.
IF condition.
  lv_result = compute( ).
ENDIF.
```

---

## Project-Specific Rule Overrides

Each project can add its own abaplint guidance by creating a file in the `guidelines/`
folder of the project repository:

```
guidelines/
  abaplint-local.md    ← project-specific rule notes
```

After creating the file, export it so the `ref` command can find it:

```bash
abapgit-agent ref export
```

Then `abapgit-agent ref "prefer_inline"` will surface both this bundled guidance
and the project-specific notes together.

**Example `guidelines/abaplint-local.md`:**

```markdown
## prefer_inline — project rules

This project's .abaplint.json enables prefer_inline.

Additional constraint: never inline-declare response-building variables.
All JSON response strings must use string templates (| ... |) directly
on rv_result — no intermediate lv_response variable at all.
```

---

## See Also

- **common-errors.md** — char-literal truncation listed as a known error pattern
- **json.md** — safe patterns for building JSON strings in ABAP
- **workflow-detailed.md** — where abaplint fits in the development workflow
