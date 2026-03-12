---
layout: default
title: Development Workflow (Detailed)
nav_order: 15
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Development Workflow (Detailed)

```
1. Read .abapGitAgent → get folder value AND workflow.mode
       │
       ▼
2. Research → use ref command for unfamiliar topics
       │
       ▼
3. Write code → place in correct folder (e.g., src/zcl_*.clas.abap)
       │
       ▼
4. Syntax check (for CLAS, INTF, PROG, DDLS only)
       │
       ├─► CLAS/INTF/PROG/DDLS → abapgit-agent syntax --files <file>
       │       │
       │       ├─► Errors? → Fix locally (no commit needed), re-run syntax
       │       │
       │       └─► Clean ✅ → Proceed to commit
       │
       └─► Other types (FUGR, TABL, etc.) → Skip syntax, go to commit
               │
               ▼
5. Commit and push → git add . && git commit && git push
       │
       ▼
6. Activate → abapgit-agent pull --files src/file.clas.abap
       │         (behaviour depends on .abapgit-agent.json — see AI Tool Guidelines)
       │
       ▼
7. Verify → Check pull output
   - **"Error updating where-used list"** → SYNTAX ERROR (use inspect for details)
   - Objects in "Failed Objects Log" → Syntax error (use inspect)
   - Objects NOT appearing at all → XML metadata issue (check abapgit.md)
       │
       ▼
8. (Optional) Run unit tests → abapgit-agent unit --files <testclass> (AFTER successful pull)
```

**Syntax Command - Supported Object Types:**

| Object Type | Syntax Command | What to Do |
|-------------|----------------|------------|
| CLAS (classes) | ✅ Supported | Run `syntax` before commit |
| CLAS (test classes: .testclasses.abap) | ✅ Supported | Run `syntax` before commit |
| INTF (interfaces) | ✅ Supported | Run `syntax` before commit |
| PROG (programs) | ✅ Supported | Run `syntax` before commit |
| DDLS (CDS views) | ✅ Supported | Run `syntax` before commit (requires annotations) |
| FUGR (function groups) | ❌ Not supported | Skip syntax, use `pull` then `inspect` |
| TABL/DTEL/DOMA/MSAG/SHLP | ❌ Not supported | Skip syntax, just `pull` |
| All other types | ❌ Not supported | Skip syntax, just `pull` |

**IMPORTANT**:
- **Use `syntax` BEFORE commit** for CLAS/INTF/PROG/DDLS - catches errors early, no git pollution
- **Syntax checks files INDEPENDENTLY** - syntax checker doesn't have access to uncommitted files
- **For dependent files** (interface + class): Create/activate underlying object FIRST, then dependent object (see workflow below)
- **DDLS requires proper annotations** - CDS views need `@AbapCatalog.sqlViewName`, view entities don't
- **ALWAYS push to git BEFORE running pull** - abapGit reads from git
- **Use `inspect` AFTER pull** for unsupported types or if pull fails

**Working with dependent objects (RECOMMENDED APPROACH):**

When creating objects with dependencies (e.g., interface → class), create and activate the underlying object FIRST:

```bash
# Step 1: Create interface, syntax check, commit, activate
vim src/zif_my_interface.intf.abap
abapgit-agent syntax --files src/zif_my_interface.intf.abap  # ✅ Works (no dependencies)
git add src/zif_my_interface.intf.abap src/zif_my_interface.intf.xml
git commit -m "feat: add interface"
git push
abapgit-agent pull --files src/zif_my_interface.intf.abap   # Interface now activated

# Step 2: Create class, syntax check, commit, activate
vim src/zcl_my_class.clas.abap
abapgit-agent syntax --files src/zcl_my_class.clas.abap     # ✅ Works (interface already activated)
git add src/zcl_my_class.clas.abap src/zcl_my_class.clas.xml
git commit -m "feat: add class implementing interface"
git push
abapgit-agent pull --files src/zcl_my_class.clas.abap
```

**Benefits:**
- ✅ Syntax checking works for both objects
- ✅ Each step is validated independently
- ✅ Easier to debug if something fails
- ✅ Cleaner workflow

**Alternative approach (when interface design is uncertain):**

If the interface might need changes while implementing the class, commit both together:

```bash
# Create both files
vim src/zif_my_interface.intf.abap
vim src/zcl_my_class.clas.abap

# Skip syntax (files depend on each other), commit together
git add src/zif_my_interface.intf.abap src/zif_my_interface.intf.xml
git add src/zcl_my_class.clas.abap src/zcl_my_class.clas.xml
git commit -m "feat: add interface and implementing class"
git push

# Pull both together
abapgit-agent pull --files src/zif_my_interface.intf.abap,src/zcl_my_class.clas.abap

# Use inspect if errors occur
abapgit-agent inspect --files src/zcl_my_class.clas.abap
```

**Use this approach when:**
- ❌ Interface design is still evolving
- ❌ Multiple iterations expected

**Working with mixed file types:**
When modifying multiple files of different types (e.g., 1 class + 1 CDS view):
1. Run `syntax` on independent supported files (CLAS, INTF, PROG, DDLS)
2. Commit ALL files together (including unsupported types)
3. Push and pull ALL files together

Example:
```bash
# Check syntax on independent files only
abapgit-agent syntax --files src/zcl_my_class.clas.abap,src/zc_my_view.ddls.asddls

# Commit and push all files
git add src/zcl_my_class.clas.abap src/zc_my_view.ddls.asddls
git commit -m "feat: add class and CDS view"
git push

# Pull all files together
abapgit-agent pull --files src/zcl_my_class.clas.abap,src/zc_my_view.ddls.asddls
```

**When to use syntax vs inspect vs view**:
- **syntax**: Check LOCAL code BEFORE commit (CLAS, INTF, PROG, DDLS)
- **inspect**: Check ACTIVATED code AFTER pull (all types, runs Code Inspector)
- **view**: Understand object STRUCTURE (not for debugging errors)

### Quick Decision Tree for AI

**When user asks to modify/create ABAP code:**

```
1. Identify file extension(s) AND dependencies
   ├─ .clas.abap or .clas.testclasses.abap → CLAS ✅ syntax supported
   ├─ .intf.abap → INTF ✅ syntax supported
   ├─ .prog.abap → PROG ✅ syntax supported
   ├─ .ddls.asddls → DDLS ✅ syntax supported (requires proper annotations)
   └─ All other extensions → ❌ syntax not supported

2. Check for dependencies:
   ├─ Interface + implementing class? → DEPENDENT (interface is underlying)
   ├─ Class A uses class B? → DEPENDENT (class B is underlying)
   ├─ CDS view uses table? → INDEPENDENT (table already exists)
   └─ Unrelated bug fixes across files? → INDEPENDENT

3. For SUPPORTED types (CLAS/INTF/PROG/DDLS):
   ├─ INDEPENDENT files → Run syntax → Fix errors → Commit → Push → Pull
   │
   └─ DEPENDENT files (NEW objects):
       ├─ RECOMMENDED: Create underlying object first (interface, base class, etc.)
       │   1. Create underlying object → Syntax → Commit → Push → Pull
       │   2. Create dependent object → Syntax (works!) → Commit → Push → Pull
       │   ✅ Benefits: Both syntax checks work, cleaner workflow
       │
       └─ ALTERNATIVE: If interface design uncertain, commit both together
           → Skip syntax → Commit both → Push → Pull → (if errors: inspect)

4. For UNSUPPORTED types (FUGR, TABL, etc.):
   Write code → Skip syntax → Commit → Push → Pull → (if errors: inspect)

5. For MIXED types (some supported + some unsupported):
   Write all code → Run syntax on independent supported files ONLY → Commit ALL → Push → Pull ALL
```

**Example workflows:**

**Scenario 1: Interface + Class (RECOMMENDED)**
```bash
# Step 1: Interface first
vim src/zif_calculator.intf.abap
abapgit-agent syntax --files src/zif_calculator.intf.abap  # ✅ Works
git commit -am "feat: add calculator interface" && git push
abapgit-agent pull --files src/zif_calculator.intf.abap    # Interface activated

# Step 2: Class next
vim src/zcl_calculator.clas.abap
abapgit-agent syntax --files src/zcl_calculator.clas.abap  # ✅ Works (interface exists!)
git commit -am "feat: implement calculator" && git push
abapgit-agent pull --files src/zcl_calculator.clas.abap
```

**Scenario 2: Multiple independent classes**
```bash
# All syntax checks work (no dependencies)
vim src/zcl_class1.clas.abap src/zcl_class2.clas.abap
abapgit-agent syntax --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap
git commit -am "feat: add utility classes" && git push
abapgit-agent pull --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap
```

**Error indicators after pull:**
- ❌ **"Error updating where-used list"** → SYNTAX ERROR - run `inspect` for details
- ❌ **Objects in "Failed Objects Log"** → SYNTAX ERROR - run `inspect`
- ❌ **Objects NOT appearing at all** → XML metadata issue (check `ref --topic abapgit`)
- ⚠️ **"Activated with warnings"** → Code Inspector warnings - run `inspect` to see details

### Commands

```bash
# 1. Syntax check LOCAL code BEFORE commit (CLAS, INTF, PROG, DDLS)
abapgit-agent syntax --files src/zcl_my_class.clas.abap
abapgit-agent syntax --files src/zc_my_view.ddls.asddls
abapgit-agent syntax --files src/zcl_class1.clas.abap,src/zif_intf1.intf.abap,src/zc_view.ddls.asddls

# 2. Pull/activate AFTER pushing to git
abapgit-agent pull --files src/zcl_class1.clas.abap,src/zcl_class2.clas.abap

# Override conflict detection for a single pull (e.g. deliberate branch switch)
abapgit-agent pull --files src/zcl_class1.clas.abap --conflict-mode ignore

# 3. Inspect AFTER pull (for errors or unsupported types)
abapgit-agent inspect --files src/zcl_class1.clas.abap

# Run unit tests (after successful pull)
abapgit-agent unit --files src/zcl_test1.clas.testclasses.abap,src/zcl_test2.clas.testclasses.abap

# View object definitions (multiple objects)
abapgit-agent view --objects ZCL_CLASS1,ZCL_CLASS2,ZIF_INTERFACE

# Preview table data (multiple tables/views)
abapgit-agent preview --objects ZTABLE1,ZTABLE2

# Explore table structures
abapgit-agent view --objects ZTABLE --type TABL

# Display package tree
abapgit-agent tree --package \$MY_PACKAGE

# Investigate runtime errors (ST22 short dumps)
abapgit-agent dump                                          # Last 7 days
abapgit-agent dump --user DEVELOPER --date TODAY           # Today's dumps for a user
abapgit-agent dump --program ZMY_PROGRAM                   # Dumps from a specific program
abapgit-agent dump --error TIME_OUT                        # Dumps by error type
abapgit-agent dump --user DEVELOPER --detail 1             # Full detail of first result
```
