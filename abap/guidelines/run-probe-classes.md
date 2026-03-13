---
layout: default
title: run Command Guide
nav_order: 18
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# run Command — AI Guidelines

## Never Run Proactively

`abapgit-agent run` executes live ABAP code. **Never call it unless the user explicitly asks.**

A class implementing `IF_OO_ADT_CLASSRUN` can do anything — modify database records, send emails, trigger RFCs. The interface signature gives no indication of side effects.

```
User: "Write a class that reads flight data and prints it"
→ ✓ Create the class, pull it, STOP. Do NOT run it.
→ ✓ Tell the user: "Class is activated. Run with: abapgit-agent run --class ZCL_MY_CLASS"

User: "Now run it"
→ ✓ Run it
```

---

## Probe Classes and `scratchWorkspace`

### Decision flow

```
User asks to create a probe class
├── disableProbeClasses = false / not set  →  create in current project (default)
└── disableProbeClasses = true
    ├── scratchWorkspace configured  →  create there (see workflow below)
    └── scratchWorkspace not configured  →  refuse, guide user to set it up
```

### When `disableProbeClasses = true` and `scratchWorkspace` is configured

**Naming** — derive from `scratchWorkspace` config in `.abapGitAgent`:
- `classPrefix` (default: `ZCL_{USER}_`) + `<PURPOSE>`, max 30 chars
- Example: user=`JOHN`, purpose=`OPEN_TRANSPORTS` → `ZCL_JOHN_OPEN_TRANSPORTS`
- If name already exists in `{path}/src/`, append `_2`, `_3`, etc.

**Workflow:**
1. Read `{path}/.abapGitAgent` to confirm `folder` property (e.g. `/src/`)
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

### When `disableProbeClasses = true` and `scratchWorkspace` is NOT configured

Refuse and tell the user to configure `scratchWorkspace` in `.abapGitAgent`.
→ See `docs/run-command.md` (Scratch Workspace section) for setup instructions.
