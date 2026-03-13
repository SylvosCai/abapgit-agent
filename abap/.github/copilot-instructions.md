# ABAP Development with abapGit

You are working on an ABAP project using abapGit for version control.

---

## Critical Rules

### 1. Use `ref` Command for Unfamiliar Topics

**When starting to work on ANY unfamiliar ABAP topic, syntax, or pattern, you MUST use the `ref` command BEFORE writing any code.**

```
❌ WRONG: Start writing code immediately based on assumptions
✅ CORRECT: Run ref command first to look up the correct pattern
```

**Why**: ABAP syntax is strict. Guessing leads to activation errors that waste time.

| Scenario | Example |
|----------|---------|
| Implementing new ABAP feature | "How do I use FILTER operator?" |
| Unfamiliar pattern | "What's the correct VALUE #() syntax?" |
| SQL operations | "How to write a proper SELECT with JOIN?" |
| CDS views | "How to define CDS view with associations?" |
| Getting syntax errors | Check reference before trying approaches |

```bash
# Search for a pattern
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"

# Browse by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql

# List all topics
abapgit-agent ref --list-topics
```

### 2. Read `.abapGitAgent` for Folder Location and Naming Conventions

**Before creating ANY ABAP object file, you MUST read `.abapGitAgent` to determine the correct folder.**

```
❌ WRONG: Assume files go in "abap/" folder
✅ CORRECT: Read .abapGitAgent to get the "folder" property value
```

The folder is configured in `.abapGitAgent` (property: `folder`):
- If `folder` is `/src/` → files go in `src/` (e.g., `src/zcl_my_class.clas.abap`)
- If `folder` is `/abap/` → files go in `abap/` (e.g., `abap/zcl_my_class.clas.abap`)

**Also check naming conventions before creating any new object:**

```
1. Check guidelines/objects.local.md  ← project-specific overrides (if file exists)
2. Fall back to guidelines/objects.md ← default Z/Y prefix conventions
```

`objects.local.md` is created by `abapgit-agent init` and is never overwritten by updates — it holds project-specific prefixes (e.g. `YCL_` instead of `ZCL_`).

### 3. Create XML Metadata / Local Classes

Each ABAP object needs an XML metadata file. Local helper/test-double classes use separate `.locals_def.abap` / `.locals_imp.abap` files.
→ See `guidelines/object-creation.md` for XML templates and local class setup

### 4. Use Syntax Command Before Commit (for CLAS, INTF, PROG, DDLS)

```
❌ WRONG: Make changes → Commit → Push → Pull → Find errors → Fix → Repeat
✅ CORRECT: Make changes → Run syntax → Fix locally → Commit → Push → Pull → Done
```

**For CLAS, INTF, PROG, DDLS files**: Run `syntax` command BEFORE commit to catch errors early.

```bash
# Check syntax of local code (no commit/push needed)
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Check multiple INDEPENDENT files
abapgit-agent syntax --files src/zcl_utils.clas.abap,src/zcl_logger.clas.abap
```

**For dependent files, skip `syntax` and use `pull` instead:**
```bash
# ❌ BAD - Interface and implementing class (may show false errors)
abapgit-agent syntax --files src/zif_my_intf.intf.abap,src/zcl_my_class.clas.abap

# ✅ GOOD - Use pull instead for dependent files
git add . && git commit && git push
abapgit-agent pull --files src/zif_my_intf.intf.abap,src/zcl_my_class.clas.abap
```

### 5. Local Helper / Test-Double Classes

→ See `guidelines/object-creation.md` for local class setup (`locals_def.abap` / `locals_imp.abap`)

### 6. Use `ref`, `view` and `where` Commands to Learn About Unknown Classes/Methods

**When working with unfamiliar ABAP classes or methods, follow this priority:**

```
1. First: Check local git repo for usage examples
2. Second: Check ABAP reference/cheat sheets
3. Third: Use view/where commands to query ABAP system (if needed)
```

```bash
# Find where a class/interface is USED
abapgit-agent where --objects ZIF_UNKNOWN_INTERFACE

# View CLASS DEFINITION
abapgit-agent view --objects ZCL_UNKNOWN_CLASS
```

### 7. CDS Unit Tests

Use `CL_CDS_TEST_ENVIRONMENT` for unit tests that read CDS views.
→ See `guidelines/cds-testing.md` for code examples

### 8. Use `unit` Command for Unit Tests

**Use `abapgit-agent unit` to run ABAP unit tests (AUnit).**

```
❌ WRONG: Try to use SE24, SE37, or other transaction codes
✅ CORRECT: Use abapgit-agent unit --files src/zcl_test.clas.testclasses.abap
```

```bash
# Run unit tests (after pulling to ABAP)
abapgit-agent unit --files src/zcl_test.clas.testclasses.abap

# Multiple test classes
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap
```

### 9. Troubleshooting ABAP Issues

| Symptom | Tool | When |
|---|---|---|
| HTTP 500 / runtime crash (ST22) | `dump` | Error already occurred |
| Wrong output, no crash | `debug` | Need to trace logic |

Quick start:
```bash
abapgit-agent dump --date TODAY --detail 1    # inspect last crash
abapgit-agent debug set --objects ZCL_FOO:42  # set breakpoint then attach
```
→ See `guidelines/debug-dump.md` for dump analysis workflow
→ See `guidelines/debug-session.md` for full debug session guide, breakpoint tips, and pull flow architecture

---

## Development Workflow

This project's workflow mode is configured in `.abapGitAgent` under `workflow.mode`.

### Project-Level Config (`.abapgit-agent.json`)

Checked into the repository — applies to all developers. **Read this file at the start of every session.**

| Setting | Values | Default | Effect |
|---------|--------|---------|--------|
| `safeguards.requireFilesForPull` | `true`/`false` | `false` | Requires `--files` on every pull |
| `safeguards.disablePull` | `true`/`false` | `false` | Disables pull entirely (CI/CD-only projects) |
| `conflictDetection.mode` | `"abort"`/`"ignore"` | `"abort"` | Whether to abort pull on conflict |
| `transports.allowCreate` | `true`/`false` | `true` | When `false`, `transport create` is blocked |
| `transports.allowRelease` | `true`/`false` | `true` | When `false`, `transport release` is blocked |

### Workflow Modes

| Mode | Branch Strategy | Rebase Before Pull | Create PR |
|------|----------------|-------------------|-----------|
| `"branch"` | Feature branches | ✓ Always | ✓ Yes (squash merge) |
| `"trunk"` | Direct to default branch | ✗ No | ✗ No |
| (not set) | Direct to default branch | ✗ No | ✗ No |

### Trunk Workflow (`"mode": "trunk"`)

```bash
git checkout main
git pull origin main
edit src/zcl_auth_handler.clas.abap
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap
git add . && git commit -m "feat: add authentication handler"
git push origin main
abapgit-agent pull --files src/zcl_auth_handler.clas.abap
```

### Branch Workflow (`"mode": "branch"`)

→ See `guidelines/branch-workflow.md` for step-by-step commands and examples

### AI Tool Guidelines

**When `workflow.mode = "branch"`:**
1. ✓ Create feature branches (naming: `feature/description`)
2. ✓ Always `git fetch origin <default> && git rebase origin/<default>` before `pull` command
3. ✓ Use `--force-with-lease` after rebase (never `--force`)
4. ✓ Create PR with squash merge when feature complete
5. ✗ Never commit directly to default branch

**When `workflow.mode = "trunk"` or not set:**
1. ✓ Commit directly to default branch
2. ✓ `git pull origin <default>` before push
3. ✗ Don't create feature branches

**When `safeguards.requireFilesForPull = true`:** always include `--files` in every `pull` command

**When `safeguards.disablePull = true`:** do not run `abapgit-agent pull` at all

**When `conflictDetection.mode = "abort"`:** if pull aborts with conflict error, inform user and suggest `--conflict-mode ignore` to override for that run

### Quick Decision Tree

```
Modified ABAP files?
├─ CLAS/INTF/PROG/DDLS files?
│  ├─ Independent files (no cross-dependencies)?
│  │  └─ ✅ Use: syntax → commit → push → pull
│  └─ Dependent files (interface + class, class uses class)?
│     └─ ✅ Use: skip syntax → commit → push → pull
└─ Other types (FUGR, TABL, etc.)?
   └─ ✅ Use: skip syntax → commit → push → pull → (if errors: inspect)
```

→ See `guidelines/workflow-detailed.md` for full workflow decision tree, error indicators, and complete command reference

---

## Guidelines Index

Detailed guidelines are available in the `guidelines/` folder:

| File | Topic |
|------|-------|
| `guidelines/sql.md` | ABAP SQL Best Practices |
| `guidelines/exceptions.md` | Exception Handling |
| `guidelines/testing.md` | Unit Testing (including CDS) |
| `guidelines/cds.md` | CDS Views |
| `guidelines/classes.md` | ABAP Classes and Objects |
| `guidelines/objects.md` | Object Naming Conventions (defaults) |
| `guidelines/objects.local.md` | **Project** Naming Conventions — overrides `objects.md` (never overwritten) |
| `guidelines/json.md` | JSON Handling |
| `guidelines/abapgit.md` | abapGit XML Metadata Templates |
| `guidelines/unit-testable-code.md` | Unit Testable Code Guidelines (Dependency Injection) |
| `guidelines/common-errors.md` | Common ABAP Errors - Quick Fixes |
| `guidelines/debug-session.md` | Debug Session Guide |
| `guidelines/debug-dump.md` | Dump Analysis Guide |
| `guidelines/branch-workflow.md` | Branch Workflow |
| `guidelines/workflow-detailed.md` | Development Workflow (Detailed) |
| `guidelines/object-creation.md` | Object Creation (XML metadata, local classes) |
| `guidelines/cds-testing.md` | CDS Testing (Test Double Framework) |

These guidelines are automatically searched by the `ref` command.

---

## For More Information

- [SAP ABAP Cheat Sheets](https://github.com/SAP-samples/abap-cheat-sheets)
- [ABAP Keyword Documentation](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm)
