---
layout: default
title: String Templates
nav_order: 25
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# String Templates

**Searchable keywords**: string template, pipe, `|...|`, literal brace, escape, expression limiter, JSON, `\{`, `\}`, WIDTH, ALIGN, ALPHA, CASE, ZERO, NUMBER, DATE, TIME, padding, formatting

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

## Formatting Options (verified on live system)

Formatting options go inside `{ expr OPTION = VALUE }`:

### Alignment and padding

```abap
" WIDTH pads/truncates to fixed width; ALIGN controls side
DATA(s) = |{ lv_carrier WIDTH = 10 ALIGN = LEFT  }|.  " 'LH        '
DATA(s) = |{ lv_price   WIDTH = 10 ALIGN = RIGHT }|.  " '       500'
DATA(s) = |{ lv_text    WIDTH = 20 ALIGN = CENTER }|.
```

`ALIGN` values: `LEFT` (default), `RIGHT`, `CENTER`.

### ALPHA — leading-zero handling for numeric strings

```abap
DATA(lv_connid) = '0400'.
|{ lv_connid ALPHA = IN  }|   " → '0400' (keep leading zeros)
|{ lv_connid ALPHA = OUT }|   " → '400 ' (strip leading zeros)
```

### CASE — change letter case

```abap
|{ lv_name CASE = UPPER }|   " → 'JOHN'
|{ lv_name CASE = LOWER }|   " → 'john'
```

### ZERO — show/hide zero values

```abap
DATA(lv_zero) = 0.
|{ lv_zero ZERO = YES }|   " → '0'
|{ lv_zero ZERO = NO  }|   " → '' (empty)
```

### NUMBER — numeric formatting

```abap
DATA(lv_amount) = CONV decfloat34( '1234567.89' ).
|{ lv_amount NUMBER = USER        }|  " locale-aware: '1.234.567,89' (DE)
|{ lv_amount NUMBER = ENVIRONMENT }|  " system locale
|{ lv_amount NUMBER = RAW         }|  " '1234567.89' (no formatting)
```

### DATE / TIME — date and time formatting

```abap
|{ sy-datum DATE = USER        }|  " locale-aware: '22.04.2026' (DE)
|{ sy-datum DATE = ISO         }|  " '2026-04-22'
|{ sy-datum DATE = ENVIRONMENT }|  " system locale
|{ sy-uzeit TIME = USER        }|  " '06:24:36'
|{ sy-uzeit TIME = ISO         }|  " '062436'
```

### Combining options

Multiple options are separated by spaces:

```abap
|{ lv_value WIDTH = 15 ALIGN = RIGHT ZERO = YES }|
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
