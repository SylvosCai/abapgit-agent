---
layout: default
title: Naming Conventions
nav_order: 7
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP Object Naming Conventions

> **Project-specific overrides**: Add your naming conventions to `guidelines/objects.local.md`.
> That file is never overwritten by `abapgit-agent init --update` and is searched by the `ref` command.

**Searchable keywords**: naming convention, Z prefix, namespace, object type, CLAS, INTF, PROG, TABL, DDLS

## TOPICS IN THIS FILE
1. Naming Conventions
2. ABAP Object Types
3. XML Metadata (see guidelines/abapgit.md)

## Naming Conventions

Use `Z_` or `Y_` prefix for custom objects:

| Object Type | Prefix | Example |
|-------------|--------|---------|
| Class | ZCL_ | ZCL_MY_CLASS |
| Interface | ZIF_ | ZIF_MY_INTERFACE |
| Program | Z | ZMY_PROGRAM |
| Package | $ | $MY_PACKAGE |
| Table | Z | ZMY_TABLE |
| CDS View | ZC | ZC_MY_VIEW |
| CDS Entity | ZE | ZE_MY_ENTITY |
| Data Element | Z | ZMY_ELEMENT |
| Structure | Z | ZMY_STRUCTURE |
| Table Type | Z | ZMY_TABLE_TYPE |

## ABAP Object Types

Common object types in this project:

| Type | Description | File Suffix |
|------|-------------|-------------|
| CLAS | Classes | .clas.abap |
| PROG | Programs | .prog.abap |
| FUGR | Function Groups | .fugr.abap |
| INTF | Interfaces | .intf.abap |
| TABL | Tables | .tabl.abap |
| STRU | Structures | .stru.abap |
| DTEL | Data Elements | .dtel.abap |
| TTYP | Table Types | .ttyp.abap |
| DDLS | CDS Views | .ddls.asddls |
| DDLX | CDS Entities | .ddlx.asddlx |

## XML Metadata

**See guidelines/abapgit.md for XML templates.**

Each ABAP object requires an XML metadata file for abapGit. Quick reference:

```
Class:         zcl_*.clas.xml
Interface:     zif_*.intf.xml
Table:         z*.tabl.xml
CDS View:      zc_*.ddls.xml
```

**Important**: Always push to git BEFORE running pull:
```bash
git add .
git commit -m "Changes"
git push           # CRITICAL: Push FIRST
abapgit-agent pull --files abap/zcl_my_class.clas.abap
```
