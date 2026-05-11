---
layout: default
title: PINF Headless Pull Support
nav_order: 6
parent: pull - Pull & Activate
grand_parent: Development Commands
---

# PINF Headless Pull Support

## Background

When abapGit deserializes a brand-new PINF (Package Interface) object, it calls
`cl_package_interface=>create_new_package_interface()` to create the interface in
the ABAP system. Without the `i_suppress_dialog = abap_true` parameter,
`RS_ACCESS_PERMISSION` inside `s_lock_db` opens the **CTS transport dialog
(SAPLSTRD 0353)** — a GUI screen that requires user interaction.

In headless/REST mode (which is how `abapgit-agent pull` works) there is no GUI
session. The dialog throws `CX_SY_DYN_CALL_ILLEGAL_TYPE`, which is a subclass of
`cx_root` but **not** `zcx_abapgit_exception`. It escaped abapGit's
`DESERIALIZE_STEP` catch boundary and was silently swallowed by the outer
`CATCH cx_root` handler — the object was neither created nor reported as an error,
leaving `ACTIVATED_COUNT=0` with no log entry.

## Fix

The abapGit fork used by this project adds `i_suppress_dialog = abap_true` to
`create_new_package_interface()` in `zcl_abapgit_object_pinf~create_or_load`, and
to `lif_package_interface_facade~save`. It also calls
`set_default_package(iv_package)` and `set_default_transport(iv_transport)` before
`create_or_load` so that `RS_CORR_INSERT` inside `s_lock_db` picks up the supplied
transport request without showing a dialog.

- **Commit**: [`051908c9`](https://github.com/SylvosCai/abapGit/commit/051908c9) — *fix(PINF): suppress transport dialog in background mode*
- **Branch**: [`fix/pinf-suppress-dialog`](https://github.com/SylvosCai/abapGit/tree/fix/pinf-suppress-dialog)

## Scope

This fix only affects **brand-new PINF objects** being pulled for the first time.
Updating an existing PINF (already present in the ABAP system) already worked before
this fix.

The fix works for both local packages (`$*`) and transportable packages. For
transportable packages the transport request supplied during pull is reused via ABAP
memory — no dialog shown.

## Workflow

No changes to the pull command syntax — PINF objects are pulled the same way as any
other XML-only object:

```bash
git add src/zmy_pinf.pinf.xml
git commit -m "feat: add package interface"
git push
abapgit-agent pull --files src/zmy_pinf.pinf.xml --sync-xml
```

If the pull still fails for a new PINF, verify your ABAP system has the fix applied:

```bash
abapgit-agent health
```

→ For PINF file format and XML template: `abapgit-agent ref --topic abapgit-xml-only`
