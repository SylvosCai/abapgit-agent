---
layout: default
title: drop - Delete Single ABAP Object
nav_order: 4
parent: Development Commands
---

# drop Command

Physically delete a single ABAP object from the ABAP system and optionally re-activate it from git.

## Command

```bash
abapgit-agent drop --files <file>
abapgit-agent drop --files <file> --pull
abapgit-agent drop --files <file> --transport <TRANSPORT>
```

## When to Use

Use `drop` when an ABAP object is in a broken or inconsistent state that prevents activation:

- Object has a corrupt inactive version that blocks re-activation
- You want to force a clean re-installation of one specific object
- The object's metadata has drifted and a fresh activation is needed

## Parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `--files <file>` | ✅ Yes | Path to the ABAP source file or XML metadata file. The object type and name are derived from the filename. The file must exist on disk. |
| `--transport <TRANSPORT>` | No | Transport request (e.g. `DEVK900001`). Required if the system enforces transports. |
| `--pull` | No | Re-activate the object via `pull` immediately after deletion. |

## Accepted File Formats

| File | Object Type Derived | Supported |
|------|---------------------|-----------|
| `abap/zcl_foo.clas.abap` | CLAS ZCL_FOO | ✅ |
| `abap/zif_bar.intf.abap` | INTF ZIF_BAR | ✅ |
| `abap/zmy_prog.prog.abap` | PROG ZMY_PROG | ✅ |
| `abap/zmy_table.tabl.xml` | TABL ZMY_TABLE | ✅ |
| `abap/zmy_type.ttyp.xml` | TTYP ZMY_TYPE | ✅ |
| `abap/zmy_elem.dtel.xml` | DTEL ZMY_ELEM | ❌ Not supported |

The file must exist locally — this ensures the object can be re-pulled after deletion.

> **DTEL not supported:** After deleting a data element with `RS_DD_DELETE_OBJ`, SAP's CBDA
> in-memory activation engine cannot re-activate it (`act_rc=8`). To fix a data element,
> edit the XML file and run `pull` instead.

## Output

### Success (drop only)

```
🗑️  Dropping CLAS ZCL_FOO from ABAP system...
✅ Object deleted successfully.
   Object: ZCL_FOO (CLAS)
```

### Success (drop + pull)

```
🗑️  Dropping CLAS ZCL_FOO from ABAP system...
✅ Object deleted successfully.
   Object: ZCL_FOO (CLAS)

↩️  Re-pulling from git...

🚀 Starting pull for: https://github.com/org/repo.git
   Branch: main
   Files: abap/zcl_foo.clas.abap

✅ Pull completed successfully!
```

### Error — Object Not Found in ABAP

```
❌ Failed to delete object
   Error: Object ZCL_FOO (CLAS) not found in TADIR
```

### Error — Unsupported Type (DTEL)

```
❌ drop does not support DTEL objects.
   Data elements cannot be re-activated after deletion due to SAP CBDA
   activation engine limitations. Edit the XML file and run pull instead.
```

### Error — File Not Found on Disk

```
❌ Error: file not found: abap/zcl_foo.clas.abap
```

## Important Notes

- **Only one file per call** — `drop` takes a single `--files` argument (unlike `pull` which accepts comma-separated files).
- **File must exist on disk** — ensures the object can be re-activated after deletion.
- **Uses abapGit's own serializer** — calls `ZCL_ABAPGIT_OBJECTS=>GET_OBJECT` and `delete`, so deletion respects the same logic abapGit uses for object removal.
- **Does not affect git** — the file in your repository is not touched.
- **Transport required on locked systems** — if the ABAP system requires a transport for changes, pass `--transport`.

## Typical Workflow

```bash
# 1. Object is broken — pull fails with activation error
abapgit-agent pull --files abap/zcl_foo.clas.abap

# 2. Drop the broken object and immediately re-pull it clean
abapgit-agent drop --files abap/zcl_foo.clas.abap --pull

# 3. Done — object is freshly activated
```

## Related Commands

- [`pull`](pull-command.md) — Activate objects from git
- [`inspect`](inspect-command.md) — Run Code Inspector checks on activated objects
- [`delete`](delete-command.md) — Remove the entire abapGit repository link (not a single object)
