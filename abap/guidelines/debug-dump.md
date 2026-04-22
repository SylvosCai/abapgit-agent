---
layout: default
title: Dump Analysis Guide
nav_order: 19
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Dump Analysis Guide

#### When Something Goes Wrong — Start with `dump`

**First reflex** for any HTTP 500, runtime error, or user-reported crash:

```bash
abapgit-agent dump --date TODAY           # list today's dumps
abapgit-agent dump --date TODAY --detail 1  # full detail: call stack + source
```

The `--detail` output shows the exact failing line (`>>>>>` marker), call stack,
and SAP's error analysis. Use it before asking the user to open ST22.

> **`???` in error analysis fields** (table name, access method, key values) is normal when the
> ABAP compiler inlined the table access (e.g. `itab[ 1 ]` style expressions). Focus on the
> `>>>>>` source line and the exception class — these are always reliable.

Common filters:
```bash
abapgit-agent dump --user DEVELOPER --date TODAY        # specific user
abapgit-agent dump --error TIME_OUT                     # specific error type
abapgit-agent dump --program ZMY_PROGRAM --detail 1     # specific program, full detail
```

After identifying the failing class/method, use `view` for broader context:
```bash
abapgit-agent view --objects ZCL_MY_CLASS
```
