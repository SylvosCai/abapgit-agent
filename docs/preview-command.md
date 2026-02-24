# preview Command Requirements

## Overview

Preview data from ABAP tables or CDS views directly from the ABAP system. This command retrieves sample data rows to help developers understand table/view contents without needing to query manually.

**This is the PRIMARY way to explore table and CDS view DATA.**

## Use Cases

- Inspect table data before writing reports
- Verify data in CDS views
- Check sample records in staging tables
- Explore unknown tables/views quickly
- Validate WHERE clause filters before using in code

## Command

```bash
# Preview table data (auto-detect type)
abapgit-agent preview --objects SFLIGHT
abapgit-agent preview --objects ZMY_TABLE

# Preview CDS view data
abapgit-agent preview --objects ZC_MY_CDS_VIEW --type DDLS

# Preview with explicit type
abapgit-agent preview --objects SFLIGHT --type TABL

# Preview with row limit
abapgit-agent preview --objects SFLIGHT --limit 20

# Preview with offset for paging
abapgit-agent preview --objects SFLIGHT --offset 10 --limit 20

# Preview with WHERE clause filter
abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA'"

# Preview specific columns only
abapgit-agent preview --objects SFLIGHT --columns CARRID,CONNID,FLDATE,PRICE

# Preview multiple tables/views
abapgit-agent preview --objects SFLIGHT,ZSCUSTOMER

# Vertical format (for wide tables)
abapgit-agent preview --objects SFLIGHT --vertical

# JSON output (for scripting/AI processing)
abapgit-agent preview --objects SFLIGHT --json
```

## Prerequisite

- `.abapGitAgent` exists with valid credentials
- Table or CDS view must exist in the ABAP system

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--objects` | Yes | Comma-separated list of table/view names |
| `--type` | No | Object type (TABL, DDLS). Auto-detected from TADIR if not specified |
| `--limit` | No | Maximum rows to return (default: 100, max: 500) |
| `--offset` | No | Number of rows to skip (for paging, default: 0) |
| `--where` | No | WHERE clause filter (e.g., `CARRID = 'AA'`) |
| `--columns` | No | Comma-separated column names to display (e.g., `CARRID,CONNID,PRICE`) |
| `--vertical` | No | Show data in vertical format (one field per line) |
| `--compact` | No | Truncate values to fit columns (useful for wide tables) |
| `--json` | No | Output raw JSON only (for scripting) |

---

## Tasks

### 1. Validate Parameters

- `--objects` must be specified
- Object names are converted to uppercase automatically
- `--limit` must be between 1 and 500 (default: 100)
- `--offset` must be >= 0 (default: 0)
- `--type` accepts TABL or DDLS (case-insensitive)

### 2. Load Configuration

Read `.abapGitAgent` for credentials

### 3. Fetch CSRF Token

```bash
GET /health (with X-CSRF-Token: fetch)
```

### 4. Make Preview Request

**Endpoint:** `POST /preview`

**Request Body:**
```json
{
  "objects": ["SFLIGHT", "ZC_CDS_VIEW"],
  "type": "TABL",
  "limit": 100,
  "offset": 0,
  "where": "CARRID = 'AA'",
  "columns": ["CARRID", "CONNID", "PRICE"]
}
```

### 5. Display Results

---

## Output

### Table Data - Default (Human-readable)

Displays all columns with row count:

```
📊 Preview: SFLIGHT (Table)

┌──────────┬────────┬──────────┬───────────┬─────────┬─────────┐
│ CARRID   │ CONNID │ FLDATE   │ PRICE     │ CURRENCY│ PLANETYPE│
├──────────┼────────┼──────────┼───────────┼─────────┼─────────┤
│ AA       │ 0017   │ 20240201 │    422.94 │ USD     │ 747-400 │
│ AA       │ 0017   │ 20240202 │    422.94 │ USD     │ 747-400 │
│ AA       │ 0017   │ 20240203 │    445.00 │ USD     │ 747-400 │
│ UA       │ 0938   │ 20240201 │    350.00 │ USD     │ 777-300 │
└──────────┴────────┴──────────┴───────────┴─────────┴─────────┘

Showing 4 of 10 rows
⚠️  Note: 3 more columns hidden (SEATSMAX, SEATSOCC, PAYMENTSUM)
   Use --columns to select specific columns
   Use --json for full data
```

### Table Data - All Columns Fit

When table has 6 or fewer columns, display all:

```
📊 Preview: TADIR (Table)

┌──────────┬──────────┬───────┬────────┬────────┐
│ PGMID    │ OBJECT   │ OBJNAM│ DEVCLASS│ CTR    │
├──────────┼──────────┼───────┼────────┼────────┤
│ R3TR     │ CLAS     │ ZCL_A │ ZABAPGIT│ 000000 │
│ R3TR     │ CLAS     │ ZCL_B │ ZABAPGIT│ 000000 │
└──────────┴──────────┴───────┴────────┴────────┘

Showing 2 of 10 rows
```

### CDS View Data (Human-readable)

```
📊 Preview: ZC_FLIGHTS (CDS View)

┌──────────┬────────┬───────────┬──────────────────────┐
│ CARRID   │ CONNID │ FLDATE    │ AIRLINENAME          │
├──────────┼────────┼───────────┼──────────────────────┤
│ AA       │ 0017   │ 20240201  │ American Airlines    │
│ UA       │ 0938   │ 20240201  │ United Airlines      │
└──────────┴────────┴───────────┴──────────────────────┘

Showing 2 of 10 rows
```

### With WHERE Filter

```
📊 Preview: SFLIGHT (filtered, 5 rows)

┌──────────┬────────┬──────────┬─────────┬─────────┐
│ CARRID   │ CONNID │ FLDATE   │ PRICE   │ CURRENCY│
├──────────┼────────┼──────────┼─────────┼─────────┤
│ AA       │ 0017   │ 20240201 │  422.94 │ USD     │
│ AA       │ 0017   │ 20240202 │  422.94 │ USD     │
└──────────┴────────┴──────────┴─────────┴─────────┘

WHERE: CARRID = 'AA'
Showing 2 of 2 rows
```

### With Pagination

```
  Previewing 1 object(s)

  Retrieved data (Showing 11-20 of 354) — Use --offset 20 to see more

  📊 Preview: SFLIGHT (Table)
  ...
```

### Column Selection

```bash
abapgit-agent preview --objects SFLIGHT --columns CARRID,CONNID,FLDATE,PRICE
```

```
📊 Preview: SFLIGHT (Table)

┌──────────┬────────┬──────────┬───────────┐
│ CARRID   │ CONNID │ FLDATE   │ PRICE     │
├──────────┼────────┼──────────┼───────────┤
│ AA       │ 0017   │ 20240201 │    422.94 │
│ AA       │ 0017   │ 20240202 │    422.94 │
└──────────┴────────┴──────────┴───────────┘

Showing 2 of 10 rows (columns: CARRID, CONNID, FLDATE, PRICE)
```

### Vertical Format

Useful for tables with many columns:

```bash
abapgit-agent preview --objects SFLIGHT --vertical
```

```
  Previewing 1 object(s)

  Retrieved data

  📊 Preview: SFLIGHT (Table)

  Row 1:
  ──────────────────────────────
  MANDT: 100
  CARRID: AA
  CONNID: 17
  FLDATE: 2024-10-24
  PRICE: 422.94
  CURRENCY: USD
  PLANETYPE: 747-400
  SEATSMAX: 385
  SEATSOCC: 372
  PAYMENTSUM: 192556.43

  Row 2:
  ──────────────────────────────
  MANDT: 100
  CARRID: AA
  CONNID: 17
  FLDATE: 2024-11-25
  PRICE: 422.94
  CURRENCY: USD
  PLANETYPE: 747-400
```

### Compact Mode

```bash
abapgit-agent preview --objects SFLIGHT --compact
```

```
  Previewing 1 object(s)

  Retrieved data

  📊 Preview: SPFLI (Table)
  ┌───────┬────────┬────────┬───────────┬──────────┬──────────┬───────────┬────────────┬────────┬────────┬──────────┬──────────┬──────────┬────────┬────────┬────────┐
  │ MANDT │ CARRID │ CONNID │ COUNTRYFR │ CITYFROM │ AIRPFROM │ COUNTRYTO │ CITYTO     │ AIRPTO │ FLTIME │ DEPTIME  │ ARRTIME  │ DISTANCE │ DISTID │ FLTYPE │ PERIOD │
  ├───────┼────────┼────────┼───────────┼──────────┼──────────┼───────────┼────────────┼────────┼────────┼──────────┼──────────┼──────────┼────────┼────────┼────────┤
  │ 100   │ AA     │ 17     │ US        │ NEW YORK │ JFK      │ US        │ SAN FRA... │ SFO    │ 361    │ 11:00:00 │ 14:01:00 │ 2572     │ MI     │        │        │
  └───────┴────────┴────────┴───────────┴──────────┴──────────┴───────────┴────────────┴────────┴────────┴──────────┴──────────┴──────────┴────────┴────────┴────────┘
```

### Multiple Objects

```
📊 Preview: 2 Objects

1️⃣  SFLIGHT (Table)
    ┌──────────┬────────┬───────────┐
    │ CARRID   │ CONNID │ FLDATE    │
    ├──────────┼────────┼───────────┤
    │ AA       │ 0017   │ 20240201  │
    └──────────┴────────┴───────────┘
    1 row

2️⃣  ZC_CUST (CDS View)
    ┌──────────┬──────────┬───────────┐
    │ KUNNR    │ NAME     │ CITY      │
    ├──────────┼──────────┼───────────┤
    │ 000001   │ Customer1│ New York  │
    └──────────┴──────────┴───────────┘
    1 row
```

### JSON Output

```json
{
  "SUCCESS": true,
  "COMMAND": "PREVIEW",
  "MESSAGE": "Retrieved 2 object(s)",
  "OBJECTS": [
    {
      "NAME": "SFLIGHT",
      "TYPE": "TABL",
      "TYPE_TEXT": "Table",
      "ROW_COUNT": 4,
      "ROWS": [
        {
          "CARRID": "AA",
          "CONNID": "0017",
          "FLDATE": "20240201",
          "PRICE": "422.94",
          "CURRENCY": "USD",
          "PLANETYPE": "747-400",
          "SEATSMAX": "400",
          "SEATSOCC": "350",
          "PAYMENTSUM": "145000"
        },
        {
          "CARRID": "AA",
          "CONNID": "0017",
          "FLDATE": "20240202",
          "PRICE": "422.94",
          "CURRENCY": "USD",
          "PLANETYPE": "747-400",
          "SEATSMAX": "400",
          "SEATSOCC": "380",
          "PAYMENTSUM": "155000"
        }
      ],
      "FIELDS": [
        { "FIELD": "CARRID", "TYPE": "CHAR", "LENGTH": 3 },
        { "FIELD": "CONNID", "TYPE": "NUMC", "LENGTH": 4 },
        { "FIELD": "FLDATE", "TYPE": "DATS", "LENGTH": 8 },
        { "FIELD": "PRICE", "TYPE": "CURR", "LENGTH": 16 },
        { "FIELD": "CURRENCY", "TYPE": "CUKY", "LENGTH": 5 },
        { "FIELD": "PLANETYPE", "TYPE": "CHAR", "LENGTH": 15 },
        { "FIELD": "SEATSMAX", "TYPE": "NUMC", "LENGTH": 4 },
        { "FIELD": "SEATSOCC", "TYPE": "NUMC", "LENGTH": 4 },
        { "FIELD": "PAYMENTSUM", "TYPE": "CURR", "LENGTH": 16 }
      ],
      "TOTAL_ROWS": 10,
      "COLUMNS_DISPLAYED": 6,
      "COLUMNS_HIDDEN": ["SEATSMAX", "SEATSOCC", "PAYMENTSUM"],
      "NOT_FOUND": false,
      "ACCESS_DENIED": false
    }
  ],
  "SUMMARY": {
    "TOTAL_OBJECTS": 1,
    "TOTAL_ROWS": 4
  },
  "PAGINATION": {
    "LIMIT": 10,
    "OFFSET": 0,
    "TOTAL": 354,
    "HAS_MORE": true,
    "NEXT_OFFSET": 10
  },
  "ERROR": ""
}
```

---

## Response Structure

### JSON Response Schema

```json
{
  "SUCCESS": boolean,
  "COMMAND": "PREVIEW",
  "MESSAGE": "string",
  "OBJECTS": [
    {
      "NAME": "string",
      "TYPE": "TABL|DDLS",
      "TYPE_TEXT": "Table|CDS View",
      "ROW_COUNT": number,
      "TOTAL_ROWS": number,
      "ROWS": [
        { "FIELD_NAME": "value", ... }
      ],
      "FIELDS": [
        {
          "FIELD": "string",
          "TYPE": "string",
          "LENGTH": number
        }
      ],
      "COLUMNS_DISPLAYED": number,
      "COLUMNS_HIDDEN": ["string"],
      "NOT_FOUND": boolean,
      "ACCESS_DENIED": boolean
    }
  ],
  "SUMMARY": {
    "TOTAL_OBJECTS": number,
    "TOTAL_ROWS": number
  },
  "PAGINATION": {
    "LIMIT": number,
    "OFFSET": number,
    "TOTAL": number,
    "HAS_MORE": boolean,
    "NEXT_OFFSET": number
  },
  "ERROR": "string"
}
```

---

## Error Handling

| Error | Message |
|-------|---------|
| Table not found | `Table not found: Z_NONEXISTENT` |
| CDS View not found | `CDS View not found: Z_NONEXISTENT` |
| Access denied | `Access denied to table: SFLIGHT` |
| Invalid WHERE clause | `Invalid WHERE clause: <reason>` |
| Invalid object type | `Unsupported object type: CLAS` |
| Invalid column name | `Invalid column: INVALID_NAME` |

### Error Output

```
❌ Table not found: Z_NONEXISTENT
```

```
❌ Access denied to table: SFLIGHT
```

---

## Object Type Detection

### Auto-Detection Rules

| Object Name Pattern | Default Type |
|---------------------|--------------|
| `ZC_*` or `zc_*` | DDLS (CDS View) |
| `Z*` | TABL (Table) - fallback |
| Other | TABL (Table) |

### Supported Object Types

| Type Code | Type Text | Description |
|-----------|-----------|-------------|
| `TABL` | Table | Database table |
| `DDLS` | CDS View | CDS View/Entity |

---

## Implementation

### ABAP Tables/Classes Used

| Table/Class | Purpose |
|-------------|---------|
| **TADIR** | Object directory (verify object exists) |
| **DD02L** | Table/structure definitions |
| **DD03L** | Table/structure fields |
| **DD02V** | Table view with delivery class |
| **CL_DD_DDL_HANDLER_FACTORY** | Read CDS view data |

### Table Data Retrieval (TABL)

```abap
" Get table fields from DD03L
SELECT fieldname, datatype, leng
  FROM dd03l
  INTO TABLE lt_fields
  WHERE tabname = iv_name
    AND as4local = 'A'
  ORDER BY position.

" Build SELECT query
" Use OPEN SQL with limited fields
SELECT UP TO lv_limit (lt_fields)
  FROM (iv_name)
  INTO TABLE lt_result
  WHERE (lv_where_clause).
```

### CDS View Data Retrieval (DDLS)

```abap
" Use CDS view directly with OPEN SQL
" First verify CDS view exists in TADIR
SELECT SINGLE obj_name FROM tadir
  INTO lv_obj_name
  WHERE obj_name = iv_name
    AND object = 'DDLS'.

" Read data from CDS view
SELECT UP TO lv_limit (lt_fields)
  FROM (iv_name)
  INTO TABLE lt_result
  WHERE (lv_where_clause).
```

### Column Width Calculation

```abap
" Calculate column width based on:
" 1. Field length from DD03L
" 2. Max value length in result set
" 3. Field description length
" Minimum width: 3 characters
" Maximum width: 20 characters (truncate with ...)
```

---

## Examples

```bash
# Preview a table with default settings
abapgit-agent preview --objects SFLIGHT

# Preview with more rows
abapgit-agent preview --objects SFLIGHT --limit 50

# Preview with paging (offset)
abapgit-agent preview --objects SFLIGHT --offset 100 --limit 50

# Preview with filter
abapgit-agent preview --objects SFLIGHT --where "CARRID = 'AA' AND CONNID = '0017'"

# Preview specific columns
abapgit-agent preview --objects SFLIGHT --columns CARRID,CONNID,FLDATE,PRICE,CURRENCY

# Preview CDS view
abapgit-agent preview --objects ZC_MY_CDS_VIEW

# Vertical format for wide tables
abapgit-agent preview --objects SFLIGHT --vertical

# Compact mode
abapgit-agent preview --objects SFLIGHT --compact

# JSON for programmatic use
abapgit-agent preview --objects SFLIGHT --json

# Preview multiple objects
abapgit-agent preview --objects SFLIGHT,ZSCUSTOMER,ZCF_MY_VIEW
```

---

## Security Considerations

- **Read-only access**: Preview only reads data, no modifications
- **WHERE clause validation**: Sanitize input to prevent SQL injection
  - Allow only: `=`, `<>`, `>`, `<`, `>=`, `<=`, `LIKE`, `IN`, `AND`, `OR`, `(`, `)`
  - Block: `DELETE`, `UPDATE`, `INSERT`, `DROP`, `TABLE`
- **Column name validation**: Validate column names against DD03L
- **Row limit**: Enforce maximum of 500 rows to prevent large result sets
