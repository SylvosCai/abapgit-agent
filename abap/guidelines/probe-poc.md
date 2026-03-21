---
layout: default
title: Probe and PoC Guide
nav_order: 19
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Probe and PoC — Full Decision Flow

**Searchable keywords**: probe, PoC, proof of concept, scratchWorkspace, pocWorkspace,
disableProbeClasses, SAP namespace, customer namespace, throwaway class

## Probe vs PoC — Key Difference

| | Probe | PoC |
|---|---|---|
| Intent | Throwaway — run once to test something | Validate an idea — may persist, needs proper home |
| Naming | Auto-derived (`ZCL_{USER}_*`) | Developer chooses a meaningful name |
| Location | `scratchWorkspace` (separate repo) | `pocWorkspace` (separate repo, manually set up) |
| Config | `scratchWorkspace` in `.abapGitAgent` | `pocWorkspace` in `.abapGitAgent` |

Both always use `Z*`/`Y*` prefix and a customer namespace package (`Z*`, `Y*`, `$*`).
Never use SAP namespace prefix or SAP-delivered package for probe/PoC objects.

---

## How to Determine the Project Namespace

Before applying the decision flow, determine the project namespace from `objects.local.md`:

```
objects.local.md shows Z*/Y* prefix  OR  no objects.local.md exists
  → Customer namespace project

objects.local.md shows CL_*, IF_*, /NAMESPACE/* prefix
  → SAP namespace project
```

---

## Full Decision Flow

```
User asks to create an object
│
├── SAP namespace project AND user provides a Z*/Y* name
│   └── Ask: "Production objects here use CL_* prefix. Did you mean CL_MY_HELPER?
│           Or is this a PoC/probe object?"
│       ├── "PoC/probe"   →  continue as PoC or probe flow below
│       └── "Production"  →  correct the name to match project prefix, then proceed
│
├── User confirms PROBE
│   ├── Customer namespace project
│   │   ├── disableProbeClasses = false / not set
│   │   │   └── Create in current project (Z*/Y* prefix, Z*/Y*/$* package)
│   │   └── disableProbeClasses = true
│   │       ├── scratchWorkspace configured  →  create there (see workflow below)
│   │       └── scratchWorkspace NOT configured
│   │           └── STOP — tell user to configure scratchWorkspace in .abapGitAgent
│   │
│   └── SAP namespace project
│       ├── scratchWorkspace configured  →  create there (see workflow below)
│       └── scratchWorkspace NOT configured
│           └── STOP — tell user to configure scratchWorkspace in .abapGitAgent
│           (disableProbeClasses is irrelevant for SAP namespace projects —
│            they have no customer namespace packages to host probes)
│
└── User confirms POC
    ├── pocWorkspace.path configured in .abapGitAgent
    │   └── Read {pocWorkspace.path}/.abapGitAgent → get package property
    │       Show confirmation:
    │         "I'll create this PoC object in:
    │            Repo:    {pocWorkspace.path}
    │            Package: {package from that repo's .abapGitAgent}
    │          Is this correct, or do you want a different PoC repo?"
    │       ├── Confirmed        →  work in that repo using its naming/package rules
    │       └── Different repo   →  ask user for the path, then confirm again
    └── pocWorkspace.path NOT configured
        └── STOP — tell user to set up a PoC repo first (see setup below)
```

---

## Probe Workflow (scratchWorkspace)

**Naming** — derive from `scratchWorkspace` config in `.abapGitAgent`:
- `classPrefix` (default: `ZCL_{USER}_`) + `<PURPOSE>`, max 30 chars
- Example: user=`JOHN`, purpose=`OPEN_TRANSPORTS` → `ZCL_JOHN_OPEN_TRANSPORTS`
- If name already exists in `{path}/src/`, append `_2`, `_3`, etc.

**Workflow:**
1. Read `{scratchWorkspace.path}/.abapGitAgent` to confirm `folder` property (e.g. `/src/`)
2. Write class files in `{path}/src/`
3. Commit and push from `{path}`:
   ```bash
   cd {path} && git add . && git commit -m "probe: <description>" && git push
   ```
4. Activate:
   ```bash
   cd {path} && abapgit-agent pull --files src/<classname>.clas.abap
   ```
5. Tell user (do NOT auto-run):
   ```
   Class activated. Run with: abapgit-agent run --class <CLASSNAME>
   ```
   Run the command from the original project directory, not `{path}`.

---

## PoC Workflow (pocWorkspace)

Once the user confirms the repo and package:

1. Read `{pocWorkspace.path}/.abapGitAgent` → get `folder` and `package`
2. Read `{pocWorkspace.path}/guidelines/objects.local.md` if it exists → use its naming rules
3. Write object files in `{pocWorkspace.path}/{folder}/`
4. Commit and push from `{pocWorkspace.path}`:
   ```bash
   cd {pocWorkspace.path} && git add . && git commit -m "poc: <description>" && git push
   ```
5. Activate:
   ```bash
   cd {pocWorkspace.path} && abapgit-agent pull --files {folder}/<name>.<ext> --sync-xml
   ```

---

## PoC Repo Setup (one-time, manual)

When `pocWorkspace.path` is not configured, tell the user to:

```
1. Create a git repo for the PoC:
     mkdir my-poc && cd my-poc && git init && git remote add origin <url>

2. Link it to an ABAP package:
     abapgit-agent init --package <POC_PACKAGE>

3. Add pocWorkspace to .abapGitAgent in the main project:
     "pocWorkspace": { "path": "/absolute/path/to/my-poc" }
```

**Switching PoCs**: update `pocWorkspace.path` to point to the new PoC repo.
Claude always shows the current repo and package before creating anything —
a stale path is caught before any files are written.
