---
layout: default
title: ENHO Headless Pull Support
nav_order: 4
parent: pull - Pull & Activate
grand_parent: Development Commands
---

# ENHO Headless Pull Support

## Background

When abapGit deserializes a brand-new ENHO Hook object, it calls
`cl_enh_factory=>create_enhancement()` to create the enhancement in the ABAP system.
Without the `dark = abap_true` parameter, the Enhancement Framework opens the
**Modification Assistant dialog (SAPLSTRD 0353)** — a GUI screen that requires
user interaction.

In headless/REST mode (which is how `abapgit-agent pull` works) there is no GUI
session. The dialog throws `cx_root`, which escaped the existing `CATCH cx_enh_root`
handler and caused the pull to **silently fail** for brand-new ENHO objects — the
object was neither created nor reported as an error.

## Fix

The abapGit fork used by this project adds `dark = abap_true` to both
`cl_enh_factory=>create_enhancement()` calls in
`zcl_abapgit_object_enho_hook~deserialize`. This suppresses all dialog screens and
allows brand-new ENHO Hook objects to be created reliably via REST pull.

- **Commit**: [`5dcdc84c`](https://github.com/SylvosCai/abapGit/commit/5dcdc84c) — *fix: pass dark=abap_true to create_enhancement in ENHO hook deserializer*
- **Branch**: [`fix/enho-dark-mode`](https://github.com/SylvosCai/abapGit/tree/fix/enho-dark-mode)

## Scope

This fix only affects **brand-new ENHO objects** being pulled for the first time.
Updating an existing ENHO (already present in the ABAP system) already worked before
this fix — abapGit's full-replace deserialization (delete + recreate) reuses the
existing enhancement context which doesn't trigger the Modification Assistant dialog.

## Workflow

No changes to the pull command syntax — ENHO objects are pulled the same way as any
other object:

```bash
git add src/zfoo_enh.enho.xml src/zfoo_enh.enho.d639f45c.abap
git commit -m "feat: add enhancement"
git push
abapgit-agent pull --files src/zfoo_enh.enho.xml --sync-xml
```

If the pull still fails for a new ENHO, verify your ABAP system has the fix applied:

```bash
abapgit-agent health
```

→ For ENHO file format, hash algorithm, and creation guide: `abapgit-agent ref --topic enho`
