# ABAP Development with abapGit

You are working on an ABAP project using abapGit for version control.

> **Full guide:** https://sylvoscai.github.io/abapgit-agent/pages/abap-coding-guidelines.html
> **Agent mode:** Run `abapgit-agent guide` for the complete guide with all rules.

---

## Critical Rules

### 1. Use `ref` Command for Unfamiliar Topics

**When starting to work on ANY unfamiliar ABAP topic, syntax, or pattern, use the `ref` command BEFORE writing any code.**

```bash
abapgit-agent ref "CORRESPONDING"          # search by pattern
abapgit-agent ref --topic exceptions       # browse by topic
abapgit-agent ref --topic debug-session    # read full guideline file
abapgit-agent ref --topic workflow-detailed
```

### 2. Read `.abapGitAgent` for Folder Location and Naming Conventions

**Before creating ANY ABAP object file, read `.abapGitAgent` to determine the correct folder.**

The folder is configured in `.abapGitAgent` (property: `folder`):
- If `folder` is `/src/` → files go in `src/` (e.g., `src/zcl_my_class.clas.abap`)
- If `folder` is `/abap/` → files go in `abap/` (e.g., `abap/zcl_my_class.clas.abap`)

**Check naming conventions before creating any new object:**
1. `guidelines/objects.local.md` ← project-specific overrides (if file exists)
2. `abapgit-agent ref --topic objects` ← default Z/Y prefix conventions

### 3. Create XML Metadata / Local Classes

Each ABAP object needs an XML metadata file. Local helper/test-double classes use separate `.locals_def.abap` / `.locals_imp.abap` files.

**Name length limits** (verify before naming anything):
- CLAS, INTF: name **30 chars** | method names: **30 chars**
- TABL, STRU: name **30 chars** | **field names: 16 chars** ← most common mistake
- DDLS (CDS view entity): **40 chars** | field alias: 30 | FUGR: 26 | MSAG: 20 | PROG/DTEL/DOMA/TTYP: 30

```bash
abapgit-agent ref --topic naming-limits    # full length reference
abapgit-agent ref --topic object-creation  # XML templates and local class setup
```

### 4. Use Syntax Command Before Commit (for CLAS, INTF, PROG, DDLS)

```bash
abapgit-agent syntax --files src/zcl_my_class.clas.abap
```

For dependent files (interface + class), skip `syntax` and go straight to `pull`.

### 5. Use `ref`, `view` and `where` to Learn About Unknown Classes/Methods

```bash
abapgit-agent where --objects ZIF_UNKNOWN_INTERFACE
abapgit-agent view --objects ZCL_UNKNOWN_CLASS
abapgit-agent view --objects ZCL_UNKNOWN_CLASS --full --lines   # with line numbers for debugging
```

### 6. Troubleshooting

```bash
abapgit-agent dump --date TODAY --detail 1    # inspect last crash (ST22)
abapgit-agent debug set --objects ZCL_FOO:42  # set breakpoint then attach
abapgit-agent ref --topic debug-session       # full debug guide
abapgit-agent ref --topic debug-dump          # dump analysis guide
```

### 7. Never Run `run` Command Proactively

Never call `abapgit-agent run` unless the user explicitly asks — it can modify data or trigger RFCs.

### 8. Probe and PoC Objects — Always Z/Y, Never in SAP Packages

When `objects.local.md` shows SAP namespace (`CL_*`, `IF_*`, `/NAMESPACE/*`) and the user asks to create a new object, always ask first: *"Is this a production object, a PoC, or a probe?"*

→ `abapgit-agent ref --topic probe-poc`

### 9. abaplint — Optional, Project-Controlled

Only run if `.abaplint.json` exists. Run after `syntax`, before commit. Always check `ref --topic abaplint` before applying any quickfix.

```bash
ls .abaplint.json 2>/dev/null && abapgit-agent lint
```

---

## Development Workflow

### Project-Level Config (`.abapgit-agent.json`)

Checked into the repository — applies to all developers. **Read this file at the start of every session.**

| Setting | Effect |
|---------|--------|
| `safeguards.requireFilesForPull: true` | Always include `--files` in every `pull` |
| `safeguards.disablePull: true` | Do not run `abapgit-agent pull` at all |
| `safeguards.disableRun: true` | Do not run `abapgit-agent run` at all |
| `safeguards.disableProbeClasses: true` | Use `scratchWorkspace` for probe classes instead |
| `conflictDetection.mode: "abort"` | Abort pull on conflict — inform user |
| `transports.allowCreate/allowRelease: false` | Blocked — inform user |

### Workflow Mode (`.abapGitAgent` → `workflow.mode`)

| Mode | Branch Strategy | Rebase Before Pull | Create PR |
|------|----------------|-------------------|-----------|
| `"branch"` | Feature branches | ✓ Always | ✓ Yes (squash merge) |
| `"trunk"` / not set | Direct to default branch | ✗ No | ✗ No |

```bash
# branch mode — read full guide:
abapgit-agent ref --topic branch-workflow
```

### Quick Decision Tree

```
Modified ABAP files?
├─ CLAS/INTF/PROG/DDLS files?
│  ├─ Independent files?  → syntax → [abaplint] → commit → push → pull --sync-xml
│  └─ Dependent files?    → skip syntax → [abaplint] → commit → push → pull --sync-xml
└─ Other types (FUGR, TABL, etc.)?
   ├─ XML-only (TABL, STRU, DTEL, TTYP, DOMA, MSAG)?
   │  → skip syntax → [abaplint] → commit → push → pull --files <name>.tabl.xml --sync-xml
   └─ FUGR and others?
      → skip syntax → [abaplint] → commit → push → pull --sync-xml → (if errors: inspect)

[abaplint] = run lint only if .abaplint.json exists; ref --topic abaplint before any quickfix
```

```bash
abapgit-agent ref --topic workflow-detailed   # full workflow decision tree
```

---

## Project-Specific Naming Conventions

See `guidelines/objects.local.md` for this project's naming convention overrides.
