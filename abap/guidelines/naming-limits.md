---
layout: default
title: Naming Length Limits
nav_order: 6
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP Naming Length Limits

**Searchable keywords**: name length, character limit, 30 characters, 16 characters, 40 characters, too long, truncate, field name, method name, class name, table name, CDS name

> **Rule of thumb**: Most ABAP object names → 30 chars. Table/structure **field names** → 16 chars. CDS/DDLS names → 40 chars. Method names (including test methods) → 30 chars.

---

## Quick Reference

| Object / Element | Max Length | Example (at limit) |
|---|---|---|
| Class name (CLAS) | **30** | `ZCL_PURCHASE_ORDER_VALIDATOR` |
| Interface name (INTF) | **30** | `ZIF_PURCHASE_ORDER_VALIDATOR` |
| Program name (PROG) | **30** | `ZFICO_PAYMENT_PROPOSAL_REPORT` |
| Function Group name (FUGR) | **26** | `ZFICO_PAYMENT_UTILITIES` |
| Function Module name | **30** | `ZFICO_CALCULATE_OPEN_ITEMS` |
| Table name (TABL) | **30** | `ZFICO_PAYMENT_PROPOSAL_HEADER` |
| **Table field name** | **16** | `PAYMENT_METHOD` |
| Structure name (STRU) | **30** | `ZFICO_PAYMENT_PROPOSAL_RESULT` |
| **Structure field name** | **16** | `PAYMENT_METHOD` |
| Data Element name (DTEL) | **30** | `ZFICO_PAYMENT_PROPOSAL_STATUS` |
| Domain name (DOMA) | **30** | `ZFICO_PAYMENT_PROPOSAL_STATUS` |
| Table Type name (TTYP) | **30** | `ZFICO_PAYMENT_PROPOSAL_T_TYPE` |
| Package name | **30** | `ZFICO_PAYMENT_PROPOSAL_COMMON` |
| **CDS View Entity name (DDLS)** | **40** | `ZC_FICO_PAYMENT_PROPOSAL_ITEM_DETAILS` |
| CDS field alias | **30** | `PaymentProposalItemCategory` |
| Message Class name (MSAG) | **20** | `ZFICO_PAYMENT_MSGS` |
| Class method name | **30** | `calculate_payment_proposal` |
| **Test method name** | **30** | `test_calculate_open_items` |
| Interface method name | **30** | `calculate_payment_proposal` |
| Class attribute name | **30** | `mv_payment_proposal_processor` |
| Local variable name | **30** | `lv_payment_proposal_status` |
| Local type/class name | **30** | `lty_payment_proposal_items` |
| Test class name (local) | **30** | `ltcl_payment_proposal_calc` |

---

## Critical Differences — Don't Confuse These

### Table/Structure Field Names: 16 Characters MAX

This is the **most common mistake**. Field names in TABL and STRU are limited to **16 characters**, not 30.

```xml
<!-- WRONG — 17 characters -->
<FIELDNAME>LAST_MODIFIED_AT</FIELDNAME>

<!-- CORRECT — 16 characters or fewer -->
<FIELDNAME>LAST_MODIFIED</FIELDNAME>
<FIELDNAME>SYS_CHANGED_AT</FIELDNAME>  <!-- 14 chars ✓ -->
<FIELDNAME>LAST_PULLED_AT</FIELDNAME>  <!-- 14 chars ✓ -->
```

When naming table fields, keep names short and descriptive:
- `CARRID` not `CARRIER_ID_FIELD`
- `CONNID` not `CONNECTION_IDENTIFIER`
- `STATUS` not `CURRENT_STATUS_FLAG`
- `CREATED_AT` not `CREATION_TIMESTAMP`

### CDS View Names: 40 Characters MAX

CDS View Entity (DDLS) names allow up to **40 characters** — more room than regular ABAP objects.

```
ZC_MY_FLIGHT_BOOKING_REVENUE_SUMMARY     ← 40 chars (at limit)
ZC_FLIGHT_REVENUE                        ← 17 chars (fine)
```

However, CDS **field aliases** inside the view are still limited to **30 characters** (ABAP identifier rules).

### Function Group Names: 26 Characters MAX

Function groups (`FUGR`) have a **26-character limit** because ABAP appends a 4-character suffix internally (e.g. `SAPLZMY_FG` prefix + module name). The safe usable name length is 26 characters.

### Test Method Names: 30 Characters MAX — Causes Syntax Error

Test methods (`FOR TESTING`) hit the 30-char limit frequently because the `test_` prefix takes 5 chars before the meaningful content starts.

```abap
" WRONG — 34 characters → syntax error at activation
METHODS test_execute_with_minimal_params FOR TESTING.

" CORRECT — abbreviate to stay within 30 chars
METHODS test_exec_minimal FOR TESTING.     " 18 chars ✓
METHODS test_exec_with_files FOR TESTING.  " 24 chars ✓
```

**Counting test method length**: include the full method name — `test_exec_minimal` is 18 characters.

---

## Counting Characters Before You Name Things

Use this mental check before naming any ABAP element:

```
# Object name: type prefix + your name ≤ limit
ZCL_             (4 chars) + name ≤ 30  →  name ≤ 26 chars
ZIF_             (4 chars) + name ≤ 30  →  name ≤ 26 chars
ZC_              (3 chars) + name ≤ 40  →  name ≤ 37 chars  (CDS)
Z                (1 char)  + name ≤ 30  →  name ≤ 29 chars  (table/program)

# Namespace prefix eats more of the budget — plan ahead
ZCL_FICO_        (9 chars) + name ≤ 30  →  name ≤ 21 chars
ZCL_FICO_PAYMENT_PROPOSAL   = 26 chars  ✓
ZCL_FICO_PAYMENT_PROPOSAL_V = 27 chars  ✓  (but getting tight)

# Field name in TABL/STRU: no prefix, just ≤ 16 total
PAYMENT_METHOD               = 14 chars  ✓
PAYMENT_METHOD_CD            = 17 chars  ✗  → shorten to PAYMENT_METH_CD

# Method name: no prefix, just ≤ 30 total
test_exec_with_files                    →  24 chars ✓
test_execute_with_minimal_params        →  34 chars ✗
```

---

## Common Length Violations and Fixes

| Too Long (violates limit) | Fixed Version | Limit |
|---|---|---|
| `ZCL_COMMAND_PULL_WITH_RETRY` (30+ chars) | `ZCL_COMMAND_PULL_RETRY` | 30 |
| `LAST_SUCCESSFULLY_PULLED_AT` (table field, 28 chars) | `LAST_PULLED_AT` | 16 |
| `test_execute_command_with_files` (test method, 32 chars) | `test_exec_with_files` | 30 |
| `ZC_MY_VERY_LONG_CDS_VIEW_NAME_EXCEEDS_40_CHARS` (47 chars) | `ZC_MY_LONG_CDS_VIEW_NAME_TRIMMED` | 40 |
| `ZBIZ_OBJECT_CREATION_SERVICE_MESSAGE_CLASS` (MSAG, 43 chars) | `ZBIZ_CREATE_MSGS` | 20 |

---

## SAP Technical Basis for These Limits

These limits come from the ABAP Dictionary (DDIC) and ABAP kernel:

| Limit Source | Explanation |
|---|---|
| 30 chars (most objects) | ABAP uses `RSYN` program name space; objects stored in `TADIR` with `SOBJ_NAME CHAR(40)` but compiler enforces 30 for classes/interfaces/programs |
| 16 chars (DDIC fields) | Stored in `DD03L.FIELDNAME CHAR(16)` — this is a hard database column width |
| 40 chars (CDS names) | CDS objects stored in `DD02L.TABNAME CHAR(40)` — intentionally larger for CDS |
| 20 chars (MSAG) | Message class name stored in `T100A.ARBGB CHAR(20)` |
| 26 chars (FUGR) | Function group internally prefixed with `SAPL` (4 chars) for the main include |

---

## See Also

- **Naming Conventions** (objects.md) — prefixes per object type
- **Object Creation** (object-creation.md) — which files to create
- **Testing** (testing.md) — test method naming (30-char limit detail)
