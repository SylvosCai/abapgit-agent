---
layout: default
title: Overview
nav_order: 1
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# ABAP Coding Guidelines Index

This folder contains detailed ABAP coding guidelines that can be searched using the `ref` command.

## Guidelines Available

| File | Topic |
|------|-------|
| `sql.md` | ABAP SQL Best Practices |
| `exceptions.md` | Exception Handling |
| `testing.md` | Unit Testing (including CDS) |
| `cds.md` | CDS Views |
| `classes.md` | ABAP Classes and Objects |
| `objects.md` | Object Naming Conventions |
| `naming-limits.md` | Naming Length Limits (30/16/40 char rules per type) |
| `json.md` | JSON Handling |
| `string-template.md` | String Templates — syntax, escaping `\{` `\}`, JSON payloads |
| `abapgit.md` | abapGit XML Metadata — CLAS, INTF, PROG, CDS/DDLS, DCLS, FUGR |
| `abapgit-xml-only.md` | abapGit XML Metadata — XML-only objects (TABL, STRU, DTEL, TTYP, DOMA, MSAG) |
| `abapgit-fugr.md` | abapGit XML Metadata — Function Group (FUGR) details |
| `enho.md` | Enhancement Objects (ENHO) — workflow, hash algorithm, creation guide |
| `unit-testable-code.md` | Unit Testable Code Guidelines (Dependency Injection) |
| `common-errors.md` | Common ABAP Errors - Quick Fixes |
| `debug-session.md` | Debug Session Guide |
| `debug-dump.md` | Dump Analysis Guide |
| `branch-workflow.md` | Branch Workflow |
| `workflow-detailed.md` | Development Workflow (Detailed) |
| `object-creation.md` | Object Creation (XML metadata, local classes) |
| `cds-testing.md` | CDS Testing (Test Double Framework) |
| `abaplint.md` | abaplint Rule Guidelines (prefer_inline trap, safe patterns) |
| `comments.md` | Documentation Comments (ABAP DOC, shorttext, @parameter, CDS //) |
| `run-probe-classes.md` | run Command — AI Guidelines (probe classes, scratchWorkspace) |
| `probe-poc.md` | Probe and PoC — Full Decision Flow |

## Usage

These guidelines are automatically searched by the `ref` command:

```bash
# Search across all guidelines
abapgit-agent ref "CORRESPONDING"

# List all topics
abapgit-agent ref --list-topics
```

## Adding Custom Guidelines

To add your own guidelines:
1. Create a new `.md` file in this folder
2. Export to reference folder: `abapgit-agent ref export`
