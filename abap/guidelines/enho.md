---
layout: default
title: Enhancement Objects (ENHO)
nav_order: 15
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Enhancement Objects (ENHO) — Workflow Guide

ENHO (Hook Enhancements) let you inject ABAP code into specific spots of existing methods without modifying the original class. Use them sparingly — only when you cannot subclass or use dependency injection.

**Searchable keywords**: enho, enhancement, hook, hook implementation, enhancements, hook_impl

---

## Creating Brand-New ENHOs

**abapGit pull can create brand-new ENHOs headlessly** — write the XML and ABAP files, push, and pull as normal.

This works because abapGit's ENHO hook deserializer passes `dark = abap_true` to `cl_enh_factory=>create_enhancement`, which suppresses the Modification Assistant dialog (`SAPLSTRD 0353`) that would otherwise require a GUI session. The `save()` call also uses `run_dark = abap_true`.

### Workflow for new ENHOs

1. Write `<name>.enho.xml` (see XML Template section below)
2. For each hook: compute `hash = SHA1(full_name)[0:8]` → write `<name>.enho.<hash>.abap`
3. Commit, push, then pull:
   ```bash
   abapgit-agent pull --files src/<name>.enho.xml --sync-xml
   ```

The `--sync-xml` flag is important for new objects — abapGit's serializer may rewrite the XML during the first pull.

Once the ENHO exists in the system, abapGit's full-replace deserialization (delete + recreate) works correctly via REST.

---

## File Naming Pattern

An ENHO object produces **multiple** files:

```
<name>.enho.xml                 ← object metadata (describes all hooks)
<name>.enho.<hash>.abap         ← hook source code (one file per hook)
<name>.enho.<hash2>.abap        ← second hook source (if multiple hooks)
```

### Hash Computation

The `<hash>` is the first 8 hex characters of SHA1(`full_name`):

```js
// Node.js
const crypto = require('crypto');
const fullName = '\\TY:ZCL_TARGET_CLASS\\ME:GET_VALUE\\SE:BEGIN\\EI';
const hash = crypto.createHash('sha1').update(fullName).digest('hex').substring(0, 8);
// → 'd639f45c'
```

### `full_name` Format

```
\TY:<CLASS>\ME:<METHOD>\SE:<SECTION>\EI
```

| Part | Value | Meaning |
|------|-------|---------|
| `\TY:` | class name (uppercase) | Target class |
| `\IN:` | interface name (uppercase) | Interface (only when method is from an interface) |
| `\ME:` | method name (uppercase) | Target method |
| `\SE:` | `BEGIN` or `END` | Hook point |
| `\EI` | *(fixed suffix)* | End of identifier |

Examples:
```
\TY:ZCL_TARGET\ME:GET_VALUE\SE:BEGIN\EI
\TY:CL_MPC_EXT\IN:/IWBEP/IF_MGW_APPL_SRV_RUNTIME\ME:GET_ENTITYSET\SE:BEGIN\EI
```

The `full_name` and its hash are stored in the XML `<FILES>/<item>/<FILE>` element — read them directly from the `.enho.xml` file when the ENHO already exists in git.

---

## Pull Behavior

ENHO deserialization is **full replace**: abapGit deletes the entire ENHO object and recreates all its hooks from git. This is the same as for classes and interfaces.

- Passing the `.enho.xml` file targets the whole ENHO (preferred)
- Passing any `.enho.<hash>.abap` file also targets the whole ENHO (resolves to same object)
- **Other ENHO objects are NOT affected** — the object filter is at the ENHO name level

```bash
# Both commands activate the whole ZFOO_ENH (all hooks)
abapgit-agent pull --files src/zfoo_enh.enho.xml
abapgit-agent pull --files src/zfoo_enh.enho.d639f45c.abap
```

---

## Workflow — Editing an Existing ENHO

When an ENHO already exists in both git and the ABAP system:

```bash
# Edit the hook source
vi src/zfoo_enh.enho.d639f45c.abap

# Skip syntax (not supported for ENHO), commit, push, pull
git add src/zfoo_enh.enho.d639f45c.abap
git commit -m "fix: update enhancement hook"
git push
abapgit-agent pull --files src/zfoo_enh.enho.xml --sync-xml
```

---

## Note on Syntax Command

The `syntax` command supports ENHO `.enho.<hash>.abap` files. It extracts the code between `ENHANCEMENT`/`ENDENHANCEMENT`, wraps it in a minimal program skeleton, and runs `SYNTAX-CHECK` with the target class pool's uccheck/unicode settings.

```bash
abapgit-agent syntax --files src/zfoo_enh.enho.d639f45c.abap
```

**Limitation**: Because the code is checked outside the real class pool context, instance variables and class-specific types from the target class are not visible. The check catches basic syntax errors (undefined keywords, missing periods, etc.) but not semantic errors like unknown field names from the enhanced class.

---

## XML Template

This is the format abapGit actually serializes. Use a real abapGit export as the source of truth — copy from an existing ENHO in your project.

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_ENHO" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <TOOL>HOOK_IMPL</TOOL>
   <SHORTTEXT>My enhancement description</SHORTTEXT>
   <ORIGINAL_OBJECT>
    <PGMID>R3TR</PGMID>
    <ORG_OBJ_TYPE>CLAS</ORG_OBJ_TYPE>
    <ORG_OBJ_NAME>ZCL_TARGET_CLASS</ORG_OBJ_NAME>
    <ORG_MAIN_TYPE>CLAS</ORG_MAIN_TYPE>
    <ORG_MAIN_NAME>ZCL_TARGET_CLASS</ORG_MAIN_NAME>
    <PROGRAMNAME>ZCL_TARGET_CLASS================CP</PROGRAMNAME>
   </ORIGINAL_OBJECT>
   <ENHANCEMENTS>
    <ENH_HOOK_IMPL>
     <PROGRAMNAME>ZCL_TARGET_CLASS================CP</PROGRAMNAME>
     <ENHMODE>D</ENHMODE>
     <FULL_NAME>\TY:ZCL_TARGET_CLASS\ME:GET_VALUE\SE:BEGIN\EI</FULL_NAME>
    </ENH_HOOK_IMPL>
   </ENHANCEMENTS>
   <FILES>
    <item>
     <NAME>\TY:ZCL_TARGET_CLASS\ME:GET_VALUE\SE:BEGIN\EI</NAME>
     <FILE>d639f45c</FILE>
    </item>
   </FILES>
  </asx:values>
 </asx:abap>
</abapGit>
```

Key elements:
- `<TOOL>HOOK_IMPL</TOOL>` — always `HOOK_IMPL` for hook enhancements
- `<SHORTTEXT>` — short description text
- `<ORIGINAL_OBJECT>` — the class being enhanced
- `<PROGRAMNAME>` — class pool name: classname padded to 30 chars with `=`, then `CP` (total 32 chars)
  - Example: `ZCL_MY_CLASS` (12 chars) → `ZCL_MY_CLASS==================CP`
- `<ENHANCEMENTS>/<ENH_HOOK_IMPL>/<FULL_NAME>` — the full_name identifier
- `<ENHMODE>D` — Draft mode (standard value)
- `<FILES>/<item>/<NAME>` — same full_name as in ENHANCEMENTS
- `<FILES>/<item>/<FILE>` — first 8 hex chars of SHA1(full_name)

For multiple hooks: add additional `<ENH_HOOK_IMPL>` blocks in `<ENHANCEMENTS>` and additional `<item>` blocks in `<FILES>`.

### Hash file format (`.enho.<hash>.abap`)

```abap
"Name: \TY:ZCL_TARGET_CLASS\ME:GET_VALUE\SE:BEGIN\EI
ENHANCEMENT 0 ZFOO_ENH.
  " Your ABAP code goes here
  DATA lv_result TYPE string.
  lv_result = 'modified'.
ENDENHANCEMENT.
```

The first line is a comment with the `full_name`. `ENHANCEMENT 0 <ENHNAME>.` and `ENDENHANCEMENT.` are the wrapper statements — the actual code goes between them.
