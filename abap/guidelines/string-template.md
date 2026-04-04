---
layout: default
title: String Templates
nav_order: 25
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# String Templates

**Searchable keywords**: string template, pipe, `|...|`, literal brace, escape, expression limiter, JSON, `\{`, `\}`

## Syntax

String templates are delimited by `|...|`. Embedded expressions use `{ expr }`:

```abap
DATA(s) = |Hello { lv_name }!|.
DATA(s) = |User: { cl_abap_context_info=>get_user_technical_name( ) }|.
DATA(s) = |Length: { strlen( lv_text ) }|.
```

## Escaping Special Characters

Inside `|...|`, four characters have special meaning and must be escaped with `\`:

| Character | Escape as | Produces |
|-----------|-----------|---------|
| `{`       | `\{`      | literal `{` |
| `}`       | `\}`      | literal `}` |
| `\`       | `\\`      | literal `\` |
| `\|`      | `\|`      | literal `\|` |

```abap
" Produces: \ | { }
DATA(s) = |\\ \| \{ \}|.
```

## JSON Payloads — The Most Common Mistake

JSON objects start with `{` — which ABAP treats as an expression delimiter.
**Always escape outer JSON braces as `\{` and `\}`.**

```abap
" WRONG — "Expression limiter '{' in string template not followed by space"
rv_result = |{"success":"X","name":"{ lv_name }"}|.
"            ^ unescaped { triggers parse error

" CORRECT — outer JSON braces escaped, expression { } left as-is
rv_result = |\{"success":"X","name":"{ lv_name }"\}|.

" CORRECT — error with method call
rv_result = |\{"success":"","error":"{ lx_error->get_text( ) }"\}|.

" CORRECT — multiple embedded fields
rv_result = |\{"success":"X","object":"{ lv_obj_name }","type":"{ lv_obj_type }"\}|.
```

## Control Characters

```abap
DATA(s) = |line1\nline2|.   " newline
DATA(s) = |col1\tcol2|.     " tab
DATA(s) = |line1\r\nline2|. " CR+LF
```

## Chaining with `&&`

```abap
DATA(s) = |{ lv_a }| && ` separator ` && |{ lv_b }|.
```

## Performance Note

A string template containing only literal text (no `{ }` expressions) is evaluated at
runtime. Prefer backquote literals for pure text:

```abap
" Prefer this for literal-only strings
DATA(s) = `Hello World`.

" Not this (runtime overhead with no benefit)
DATA(s) = |Hello World|.
```
