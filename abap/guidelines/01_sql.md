# ABAP SQL Best Practices

When writing ABAP SQL (Open SQL) queries, follow these rules:

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
