---
layout: default
title: SQL Best Practices
nav_order: 2
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP SQL Best Practices

**Searchable keywords**: SELECT, FROM, WHERE, ABAP SQL, Open SQL, host variable, @ prefix, range table, INTO, UP TO, OFFSET, GROUP BY, JOIN

When writing ABAP SQL (Open SQL) queries, follow these rules:

## TOPICS IN THIS FILE
1. Host Variables (@ prefix) - line 5
2. Range Tables (IN clause) - line 17
3. SELECT Clause Order - line 35
4. Fixed Point Arithmetic - line 52
5. Field Separation - line 62

## 1. Host Variables - Use @ Prefix

Use `@` prefix for host variables in ABAP SQL:

```abap
" Correct
SELECT * FROM tadir WHERE devclass = @lv_package.

" Wrong - no @ prefix
SELECT * FROM tadir WHERE devclass = lv_package.
```

## 2. Range Tables for IN Clauses

When filtering with `IN`, use a range table with `@` prefix:

```abap
" Define range table
DATA lt_type_range TYPE RANGE OF tadir-object.
ls_type-sign = 'I'.
ls_type-option = 'EQ'.
ls_type-low = 'CLAS'.
APPEND ls_type TO lt_type_range.

" Use with @ prefix
SELECT object, obj_name FROM tadir
  WHERE object IN @lt_type_range
  INTO TABLE @lt_objects.
```

## 3. SELECT Statement Clause Order

The correct sequence is:
```
SELECT → FROM → WHERE → ORDER BY → INTO → UP TO → OFFSET
```

```abap
SELECT object, obj_name FROM tadir
  WHERE devclass = @lv_package
    AND object IN @lt_type_range
  ORDER BY object, obj_name
  INTO TABLE @lt_objects
  UP TO @lv_limit ROWS
  OFFSET @lv_offset.
```

## 4. Fixed Point Arithmetic (FIXPT)

For numeric operations in ABAP SQL (especially with UP TO/OFFSET), enable FIXPT in the class XML:

```xml
<VSEOCLASS>
  <FIXPT>X</FIXPT>
</VSEOCLASS>
```

## 5. Field Separation

Always separate fields with commas in SELECT:

```abap
" Correct
SELECT object, obj_name FROM tadir ...

" Wrong - missing comma
SELECT object obj_name FROM tadir ...
```

---

## 6. Modern ABAP SQL (with FIXPT)

When `<FIXPT>X</FIXPT>` is in class XML (default for modern ABAP):

```abap
" ✅ Required syntax
SELECT carrid, connid, fldate      " Commas required
  FROM sflight
  INTO TABLE @lt_result             " @ for host variables
  WHERE carrid = @lv_carrier.       " @ for parameters
```

**Common errors**:
- Missing commas → Add between all SELECT fields
- Missing @ → Add to all ABAP variables (not DB columns)

**Why FIXPT**: Without it, decimals treated as integers in calculations.

---

## See Also
- **Constructor Expressions** (classes.md) - for VALUE #(), FILTER, FOR loops
- **Internal Tables** - for filtering and iteration patterns
- **abapGit** (abapgit.md) - for XML metadata templates
