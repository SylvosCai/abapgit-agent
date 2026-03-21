---
layout: default
title: Object Creation Workflow
nav_order: 3
parent: ABAP Development
---

# Object Creation Workflow

This diagram shows the full decision flow when creating a new ABAP object — from determining the project namespace to choosing the right package, confirming with the user, and deploying.

![Object Creation Workflow](../img/object-creation-flow.svg)

## Key Decision Points

### 1. Customer vs SAP Namespace

Check `guidelines/objects.local.md`:
- **`Z*`/`Y*` prefix** (or no file) → customer namespace project
- **`CL_*`, `IF_*`, `/NAMESPACE/*` prefix** → SAP namespace project

### 2. Package Assignment

1. Check `objects.local.md` for package rules — use them directly if present
2. No package rules? Read `.abapGitAgent` → get `package` → run `abapgit-agent tree --package <root>`
   - One package found → use directly
   - Multiple packages → present options, ask user
3. No package in `.abapGitAgent`? → ask user

### 3. Confirmation Rule

| Object + Package | Action |
|---|---|
| `Z*`/`Y*` object in `Z*`/`Y*`/`$*` package | Write files directly — no confirmation needed |
| SAP namespace object or SAP-delivered package | Show creation summary and wait for confirmation |

### 4. SAP Namespace — Production vs PoC vs Probe

When `objects.local.md` shows a SAP namespace prefix, always ask:

> "Is this a production object, a PoC (will persist, needs its own package/repo), or a probe (throwaway, run once)?"

- **Production** → follow the package assignment + confirmation flow
- **PoC** → requires `pocWorkspace.path` configured in `.abapGitAgent`; Claude confirms repo and package before writing anything
- **Probe** → requires `scratchWorkspace` configured in `.abapGitAgent`; auto-named `ZCL_{USER}_<PURPOSE>`

→ For the complete probe/PoC decision flow: `abapgit-agent ref --topic probe-poc`

## Related Guidelines

| Guide | Access |
|---|---|
| Naming Conventions | `abapgit-agent ref --topic objects` |
| Object Creation (XML templates) | `abapgit-agent ref --topic object-creation` |
| Probe and PoC Decision Flow | `abapgit-agent ref --topic probe-poc` |
| abapGit XML Metadata | `abapgit-agent ref --topic abapgit` |
