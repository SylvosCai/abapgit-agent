---
layout: default
title: Naming Conventions
nav_order: 5
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP Object Naming Conventions

**Searchable keywords**: naming convention, prefix, namespace, object type, CLAS, INTF, PROG, TABL, DDLS, SAP namespace, customer namespace, PoC, probe

## SAP Namespace vs Customer Namespace

| | SAP Namespace | Customer Namespace |
|---|---|---|
| **Object prefix** | `CL_*`, `IF_*`, `/NAME/CL_*`, `/NAME/IF_*`, etc. | `Z*`, `Y*` |
| **Package prefix** | SAP-delivered (e.g. `SFIN`, `CA_*`, `/NAME/*`) | `Z*`, `Y*`, `$*` (local/non-transportable) |
| **Ownership** | Delivered and maintained by SAP | Owned by the customer |
| **In git repo** | Objects from an SAP-delivered package | Custom development objects |

> **Rule**: Never add customer-created objects (including PoC/probe classes) into SAP namespace
> packages. PoC objects always use `Z*`/`Y*` prefix and always go in a `Z*`, `Y*`, or `$*` package
> (use `$*` only for throwaway probes — non-transportable)
> — even on a project where production objects use SAP namespace.

## How to Determine This Project's Naming Convention

```
1. Check guidelines/objects.local.md  ← this project's actual conventions (always check first)
2. No objects.local.md exists?        ← assume customer namespace project, use Z/Y defaults below
```

`objects.local.md` is never overwritten by `abapgit-agent init --update`. It specifies the
naming pattern for production objects in this project — which could be customer namespace
(`ZCL_*`, `YCL_*`) or SAP namespace (`CL_*`, `/NAMESPACE/CL_*`).

## Default Naming Conventions (Z/Y customer namespace)

Applied when no `objects.local.md` exists:

| Object Type | Default Prefix | Default Example |
|-------------|---------------|-----------------|
| Class | `ZCL_` | `ZCL_MY_CLASS` |
| Interface | `ZIF_` | `ZIF_MY_INTERFACE` |
| Program | `Z` | `ZMY_PROGRAM` |
| Package | `Z` or `Y` | `ZMYPROJECT` |
> **Note**: `$` packages (e.g. `$TMP`) are local/non-transportable — use only for throwaway probe objects, never for real development.
| Table | `Z` | `ZMY_TABLE` |
| CDS View Entity | `ZC_` | `ZC_MY_VIEW` |
| Data Element | `Z` | `ZMY_ELEMENT` |
| Structure | `Z` | `ZMY_STRUCTURE` |
| Table Type | `Z` | `ZMY_TABLE_TYPE` |

## Project-Specific Conventions (`objects.local.md`)

`objects.local.md` should define both the naming prefix **and the correct package(s)** for
new objects. When package rules are present, Claude uses them directly without asking. When
absent, Claude must ask the user which package to use.

Examples of what `objects.local.md` might contain:

```
# Customer namespace — Y prefix
Class prefix:     YCL_    e.g. YCL_MY_CLASS
Interface prefix: YIF_    e.g. YIF_MY_INTERFACE
Default package:  YMYPROJECT

# SAP namespace project — single package
Class prefix:     CL_     e.g. CL_MY_CLASS
Interface prefix: IF_     e.g. IF_MY_INTERFACE
Default package:  MY_PACKAGE

# SAP namespace project — multiple packages
Class prefix:     CL_     e.g. CL_MY_CLASS
Interface prefix: IF_     e.g. IF_MY_INTERFACE
Packages:
  - Feature A objects → MY_PACKAGE_A
  - Feature B objects → MY_PACKAGE_B

# SAP registered namespace
Class prefix:     /MYNAMESPACE/CL_
Default package:  /MYNAMESPACE/MAIN
```

→ For file structure (what files to create): `abapgit-agent ref --topic object-creation`
→ For XML templates: `abapgit-agent ref --topic abapgit`
→ For name **length limits** (30/16/40 char rules): `abapgit-agent ref --topic naming-limits`
