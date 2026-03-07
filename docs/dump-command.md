---
layout: default
title: dump - Query Short Dumps
nav_order: 8
parent: Explore Commands
---

# dump Command Requirements

## Overview

Query ABAP short dumps from transaction ST22 without opening SAP GUI. This command lists recent short dumps with optional filters (user, date, time, program, error type) and can display the full details of a specific dump including what happened, error analysis, the source code with the exact error line marked, and the call stack.

All dates and times are displayed in the user's local timezone (system default or `--timezone`). Filtering by date/time is also timezone-aware.

**Use this command to:**
- Find recent dumps caused by a specific user or program
- Investigate runtime errors without launching SAP GUI
- Quickly identify dump patterns by error type

## Command

```bash
# List recent dumps (default: last 7 days, limit 20, system timezone)
abapgit-agent dump

# Filter by user
abapgit-agent dump --user DEVELOPER

# Filter by date (in your local timezone)
abapgit-agent dump --date TODAY
abapgit-agent dump --date YESTERDAY
abapgit-agent dump --date 2024-01-15
abapgit-agent dump --date 2024-01-01..2024-01-31

# Filter by time range (within date range, in your local timezone)
abapgit-agent dump --time 08:00..18:00

# Use a specific timezone for filtering and display
abapgit-agent dump --timezone America/New_York
abapgit-agent dump --date TODAY --timezone Europe/Berlin
abapgit-agent dump --date 2024-01-15 --time 09:00..17:00 --timezone Asia/Shanghai

# Filter by program name
abapgit-agent dump --program ZMY_PROGRAM

# Filter by runtime error type
abapgit-agent dump --error MESSAGE_TYPE_X

# Control result count
abapgit-agent dump --limit 50

# Output as JSON
abapgit-agent dump --json

# View full details for row #N from the list
abapgit-agent dump --detail 1
abapgit-agent dump --user DEVELOPER --detail 1
abapgit-agent dump --date TODAY --error TIME_OUT --detail 2
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- API user must have authorization to read short dumps (`S_DEVELOP` or `S_ABAP_DUMP`)

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--user` | No | Filter by ABAP username (e.g., `DEVELOPER`) |
| `--date` | No | Date filter: `TODAY`, `YESTERDAY`, `YYYY-MM-DD`, or `YYYY-MM-DD..YYYY-MM-DD`. Defaults to last 7 days. Interpreted in `--timezone`. |
| `--time` | No | Time range filter: `HH:MM` or `HH:MM..HH:MM`. Interpreted in `--timezone`. |
| `--timezone` | No | IANA timezone name (e.g., `America/New_York`, `Europe/Berlin`, `Asia/Shanghai`). Defaults to system timezone. |
| `--program` | No | Filter by main program name (maps to `SNAP_ADT-MAINPROG`) |
| `--error` | No | Filter by runtime error name (maps to `SNAP_ADT-RUNTIME_ERROR`, e.g., `TIME_OUT`, `MESSAGE_TYPE_X`) |
| `--limit` | No | Max results to return (default: 20, max: 100) |
| `--detail` | No | Row number N from the list to show full dump details for |
| `--json` | No | Output raw JSON only (for scripting) |

---

## Tasks

### 1. Parse Arguments

- Parse all flags listed above
- Convert `--user`, `--program`, `--error` to uppercase
- Resolve timezone: `--timezone` if provided, otherwise `Intl.DateTimeFormat().resolvedOptions().timeZone`
- Parse `--date` into `{ from, to }` as `YYYYMMDD` strings, using the resolved timezone for `TODAY`/`YESTERDAY`
- Parse `--time` into `{ from, to }` as `HHMMSS` strings
- When `--date` or `--time` is provided, convert the date/time range from the user's timezone to UTC and send as `ts_from`/`ts_to` (14-char `YYYYMMDDhhmmss` strings)
- Cap limit at 100

### 2. Load Configuration

Read `.abapGitAgent` for credentials.

### 3. Fetch CSRF Token

```
GET /health  (with X-CSRF-Token: fetch)
```

### 4. If `--detail N`: Two-step flow

1. Make list request (with `limit: 100`) using same filters to get results
2. Pick the Nth item (1-based), extract its `id` field
3. Make a second request with `detail: <id>` to fetch full dump content

### 5. Make Dump Request

**Endpoint:** `POST /sap/bc/z_abapgit_agent/dump`

**List request body (no date/time filter):**
```json
{
  "limit": 20,
  "user": "DEVELOPER",
  "program": "ZMY_PROGRAM",
  "error": "MESSAGE_TYPE_X"
}
```

**List request body (with timezone-aware date/time filter):**
```json
{
  "limit": 20,
  "user": "DEVELOPER",
  "ts_from": "20240115000000",
  "ts_to":   "20240115235959"
}
```

When `ts_from`/`ts_to` are provided, ABAP filters by `SNAP_ADT-TIMESTAMP` (UTC). Otherwise it falls back to filtering by `SNAP_ADT-DATUM`/`UZEIT` (server local time) using the default 7-day window.

**Detail request body:**
```json
{
  "detail": "20240115:142305:ldcigze_GZE_00:DEVELOPER:100:42"
}
```

All filter fields are optional. Omitted fields apply no filter.

### 6. Display Results

All `utc_timestamp` values returned from ABAP are converted to the user's timezone for display.

---

## Output

### List Mode (default)

```
  Short Dumps (3 found) [America/New_York]

    #  Date        Time      User          Program                         Error
  -----------------------------------------------------------------------------------------------------------------
    1  2024-01-15  09:26:53  DEVELOPER     ZMY_PROGRAM                     MESSAGE_TYPE_X
    2  2024-01-15  07:52:10  ADMIN         SAPMV45A                        TIME_OUT
    3  2024-01-14  18:05:12  DEVELOPER     CL_MY_CLASS                     ASSERTION_FAILED

  Use --detail <number> to see full dump details
```

The timezone label (e.g. `[America/New_York]`) is shown in the header. All dates and times are in that timezone.

When results are truncated by `--limit`:
```
  Showing 20 of 47 dumps. Use --limit to see more.
  Use --detail <number> to see full dump details
```

When no dumps found:
```
  Short Dumps (0 found) [UTC]

  No short dumps found for the given filters.
```

### Detail Mode (`--detail N`)

```
  Short Dump Detail

  Error       MESSAGE_TYPE_X
  Date        2024-01-15 (America/New_York)
  Time        09:26:53
  User        DEVELOPER
  Program     ZMY_PROGRAM
  Object      ZMY_CLASS
  Package     ZMYPACKAGE
  Exception   CX_SY_NO_HANDLER

  What happened:
  -------------------------------------------------------
  The current ABAP program has encountered an unexpected situation.

  Error analysis:
  -------------------------------------------------------
  A RAISE statement in the program "ZMY_PROGRAM" raised the exception
  condition "MESSAGE_TYPE_X".

  Call stack:
  -------------------------------------------------------
    1  ZCL_MY_CLASS->DO_SOMETHING (line 42)
    2  ZCL_MY_CLASS->PROCESS (line 17)
    3  ZMY_PROGRAM START-OF-SELECTION (line 5)

  Source (ZCL_MY_CLASS=============CM003, line 42):
  -------------------------------------------------------
        METHOD do_something.
          DATA lv_val TYPE i.
          lv_val = get_value( ).
>>>>>     RAISE EXCEPTION TYPE cx_my_error.
          lv_val = lv_val + 1.
        ENDMETHOD.
```

The `>>>>>` marker indicates the exact line where the dump occurred, matching the ST22 display. The timezone is shown next to the date.

If no structured call stack is available, only the Source section is shown.

### JSON Output (`--json`)

```json
{
  "SUCCESS": true,
  "COMMAND": "DUMP",
  "MESSAGE": "3 short dump(s) found",
  "TOTAL": 3,
  "DUMPS": [
    {
      "ID": "20240115:142305:ldcigze_GZE_00:DEVELOPER:100:42",
      "UTC_TIMESTAMP": "20240115142305",
      "DATE": "2024-01-15",
      "TIME": "14:23:05",
      "USER": "DEVELOPER",
      "PROGRAM": "ZMY_PROGRAM",
      "OBJECT": "ZMY_CLASS",
      "ERROR": "MESSAGE_TYPE_X",
      "EXCEPTION": "",
      "PACKAGE": "ZMYPACKAGE",
      "HOST": "ldcigze_GZE_00"
    }
  ],
  "ERROR": ""
}
```

`DATE`/`TIME` are the server's local time (from `SNAP_ADT-DATUM`/`UZEIT`). `UTC_TIMESTAMP` is always UTC (`YYYYMMDDhhmmss`). The CLI uses `UTC_TIMESTAMP` for timezone-converted display.

In detail mode, additional fields are populated:

```json
{
  "SUCCESS": true,
  "COMMAND": "DUMP",
  "MESSAGE": "Short dump detail retrieved",
  "TOTAL": 1,
  "DUMPS": [
    {
      "ID": "20240115:142305:ldcigze_GZE_00:DEVELOPER:100:42",
      "UTC_TIMESTAMP": "20240115142305",
      "DATE": "2024-01-15",
      "TIME": "14:23:05",
      "USER": "DEVELOPER",
      "PROGRAM": "ZMY_PROGRAM",
      "OBJECT": "ZMY_CLASS",
      "ERROR": "MESSAGE_TYPE_X",
      "EXCEPTION": "CX_SY_NO_HANDLER",
      "PACKAGE": "ZMYPACKAGE",
      "HOST": "ldcigze_GZE_00",
      "WHAT_HAPPENED": "The current ABAP program has encountered...",
      "ERROR_ANALYSIS": "A RAISE statement in...",
      "SOURCE_LINE": 42,
      "SOURCE_INCLUDE": "ZCL_MY_CLASS=============CM003",
      "CALL_STACK": [
        {
          "LEVEL": 1,
          "CLASS": "ZCL_MY_CLASS",
          "METHOD": "DO_SOMETHING",
          "PROGRAM": "ZCL_MY_CLASS========CP",
          "INCLUDE": "ZCL_MY_CLASS========CM003",
          "LINE": 42
        },
        {
          "LEVEL": 0,
          "CLASS": "",
          "METHOD": "      METHOD do_something.\n>>>>>   RAISE EXCEPTION...",
          "PROGRAM": "",
          "INCLUDE": "",
          "LINE": 0
        }
      ],
      "ERROR": ""
    }
  ],
  "ERROR": ""
}
```

The `CALL_STACK` array contains two kinds of entries:
- **Structured frames** (`LEVEL > 0`): one per stack frame with class, method, program, include, and line number
- **Source block** (`LEVEL = 0`): the annotated source code with `>>>>>` on the error line, stored in the `METHOD` field

---

## Response Structure

### JSON Response Schema

```json
{
  "SUCCESS": boolean,
  "COMMAND": "DUMP",
  "MESSAGE": "string",
  "TOTAL": number,
  "DUMPS": [
    {
      "ID": "string (datum:uzeit:ahost:uname:mandt:modno)",
      "UTC_TIMESTAMP": "string (YYYYMMDDhhmmss, UTC)",
      "DATE": "string (YYYY-MM-DD, server local time)",
      "TIME": "string (HH:MM:SS, server local time)",
      "USER": "string",
      "PROGRAM": "string",
      "OBJECT": "string",
      "ERROR": "string",
      "EXCEPTION": "string",
      "PACKAGE": "string",
      "HOST": "string",
      "WHAT_HAPPENED":  "string (detail mode only)",
      "ERROR_ANALYSIS": "string (detail mode only)",
      "SOURCE_LINE":    "number (detail mode only — 1-based line in SOURCE_INCLUDE)",
      "SOURCE_INCLUDE": "string (detail mode only — include/program containing the error)",
      "CALL_STACK": [   "(detail mode only)",
        {
          "LEVEL":   "number (0 = source block, >0 = stack frame)",
          "CLASS":   "string",
          "METHOD":  "string (frame: method name; source block: annotated source text)",
          "PROGRAM": "string",
          "INCLUDE": "string",
          "LINE":    "number"
        }
      ]
    }
  ],
  "ERROR": "string"
}
```

### Dump ID Format

The `ID` field is a colon-delimited composite of the six `SNAP_KEY` fields:

```
<DATUM>:<UZEIT>:<AHOST>:<UNAME>:<MANDT>:<MODNO>
```

Example: `20240115:142305:ldcigze_GZE_00:DEVELOPER:100:42`

This ID is used in the `detail` parameter of the second request.

### Timezone Conversion

The CLI converts user date/time inputs to UTC timestamps before sending to ABAP:

```
User input (--date / --time in --timezone)
    ↓  localToUTC() — iterative DST-safe conversion
UTC ts_from / ts_to  →  sent to ABAP as "YYYYMMDDhhmmss" strings
    ↓  ABAP filters SNAP_ADT.TIMESTAMP BETWEEN ts_from AND ts_to
UTC_TIMESTAMP in response
    ↓  utcToLocal() — Intl.DateTimeFormat
Display in user's timezone
```

When no `--date`/`--time` filter is given, the request omits `ts_from`/`ts_to` and ABAP falls back to filtering `DATUM`/`UZEIT` (server local time) over the default 7-day window.

---

## Error Handling

| Error | Message |
|-------|---------|
| No authorization | `Not authorized to read short dumps` |
| Detail row out of range | `Row number N not found in results (found M dump(s))` |
| Dump not found (detail) | `Short dump not found` |

---

## Examples

```bash
# Show all dumps from today in your system timezone
abapgit-agent dump --date TODAY

# Show today's dumps in UTC
abapgit-agent dump --date TODAY --timezone UTC

# Find all TIME_OUT dumps in the last week (system timezone)
abapgit-agent dump --error TIME_OUT

# Find dumps from a specific user in January (Berlin time)
abapgit-agent dump --user DEVELOPER --date 2024-01-01..2024-01-31 --timezone Europe/Berlin

# Show only dumps during business hours in Shanghai
abapgit-agent dump --date TODAY --time 08:00..18:00 --timezone Asia/Shanghai

# Find dumps in a specific program
abapgit-agent dump --program SAPLRHWH

# View details of the first dump on the list
abapgit-agent dump --detail 1

# View details with same filters applied
abapgit-agent dump --user DEVELOPER --detail 1

# Export today's dumps to JSON
abapgit-agent dump --date TODAY --json > dumps.json
```

---

## Implementation

### ABAP Tables/APIs Used

| Table/API | Purpose |
|-----------|---------|
| `SNAP_ADT` | Pre-indexed ST22 summary — list queries. `TIMESTAMP` field (UTC) used for timezone-aware filtering; `DATUM`/`UZEIT` (server local) used as fallback |
| `CL_RUNTIME_ERROR=>create()` | Load full dump objects from `snap_keys` — returns table of `CL_RUNTIME_ERROR` references |
| `CL_RUNTIME_ERROR->get_what_happened_text()` | What happened text (detail mode) |
| `CL_RUNTIME_ERROR->get_error_analysis_text()` | Error analysis text (detail mode) |
| `CL_RUNTIME_ERROR->get_abap_callstack()` | Structured call stack as `snap_abap_stack` (detail mode) |
| `CL_RUNTIME_ERROR->get_abap_sourceinfo()` | Source lines table + exact error line number (detail mode) |
| `CL_RUNTIME_ERROR->get_section_text()` | Fallback text sections when structured data unavailable |

### List Query — Timezone-Aware Mode

Used when `ts_from` / `ts_to` are provided (UTC):

```abap
SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
       runtime_error, mainprog, object_name, exc, devclass
  FROM snap_adt
  WHERE mandt     = @sy-mandt
    AND timestamp BETWEEN @lv_ts_from AND @lv_ts_to
    AND ( @ls_params-user    IS INITIAL OR uname         = @ls_params-user    )
    AND ( @ls_params-program IS INITIAL OR mainprog      = @ls_params-program )
    AND ( @ls_params-error   IS INITIAL OR runtime_error = @ls_params-error   )
  ORDER BY timestamp DESCENDING
  INTO TABLE @DATA(lt_adt)
  UP TO @lv_limit ROWS.
```

### List Query — Server Local Time Fallback

Used when no date/time filter is given (default 7-day window):

```abap
SELECT datum, uzeit, ahost, uname, mandt, modno, timestamp,
       runtime_error, mainprog, object_name, exc, devclass
  FROM snap_adt
  WHERE mandt  = @sy-mandt
    AND datum  BETWEEN @ls_params-date_from AND @ls_params-date_to
    AND ( @ls_params-user      IS INITIAL OR uname         = @ls_params-user      )
    AND ( @ls_params-program   IS INITIAL OR mainprog      = @ls_params-program   )
    AND ( @ls_params-error     IS INITIAL OR runtime_error = @ls_params-error     )
    AND ( @ls_params-time_from IS INITIAL OR uzeit        >= @ls_params-time_from )
    AND ( @ls_params-time_to   IS INITIAL OR uzeit        <= @ls_params-time_to   )
  ORDER BY datum DESCENDING, uzeit DESCENDING
  INTO TABLE @DATA(lt_adt)
  UP TO @lv_limit ROWS.
```

Default date range when no filter: `sy-datum - 7` to `sy-datum`.

### Dump ID Encoding

```abap
rv_id = |{ iv_datum }:{ iv_uzeit }:{ iv_ahost }:{ iv_uname }:{ iv_mandt }:{ iv_modno }|.
```

### Detail Mode — Source with Error Line Marker

```abap
lo_dump->get_abap_sourceinfo(
  IMPORTING
    p_e_include    = lv_error_include
    p_e_lineno     = lv_error_lineno
    p_e_sourcetext = lt_source ).

LOOP AT lt_source INTO DATA(lv_src_line).
  IF sy-tabix = lv_error_lineno.
    lv_source_with_marker = lv_source_with_marker
      && |>>>>> { lv_src_line }| && cl_abap_char_utilities=>newline.
  ELSE.
    lv_source_with_marker = lv_source_with_marker
      && |      { lv_src_line }| && cl_abap_char_utilities=>newline.
  ENDIF.
ENDLOOP.
```

### Detail Mode — Call Stack Fallback

When `get_abap_callstack()` returns empty and `get_abap_sourceinfo()` also returns no source, the implementation falls back to section text:

1. `get_section_text(c_section_abap_eventstack)` — section 14
2. `get_section_text(c_section_abap_source)` — section 12
