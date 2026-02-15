# ABAP Development with abapGit

You are working on an ABAP project using abapGit for version control.

## Commands

| Command | Description |
|---------|-------------|
| `abapgit-agent pull` | Pull and activate all ABAP objects |
| `abapgit-agent pull --files <file>` | Pull and activate specific file only |
| `abapgit-agent inspect --files <file>` | Syntax check ABAP source |
| `abapgit-agent unit --files <file>` | Run AUnit tests |
| `abapgit-agent tree --package <package>` | Display package hierarchy |
| `abapgit-agent view --objects <name>` | View object definitions |

## Explore Unknown Objects

**Before working with unfamiliar objects, use `view` command:**

```bash
# Check table structure
abapgit-agent view --objects ZMY_TABLE --type TABL

# Check class definition
abapgit-agent view --objects ZCL_UNKNOWN_CLASS

# Check interface
abapgit-agent view --objects ZIF_UNKNOWN_INTERFACE

# Check data element
abapgit-agent view --objects ZMY_DTEL --type DTEL
```

## When to Use View Command

AI assistant SHOULD call `view` command when:
- User asks to "check", "look up", or "explore" an unfamiliar object
- Working with a table/structure and you don't know the fields
- Calling a class/interface method and you don't know the parameters
- You need to verify an object exists before using it

## Key ABAP Rules

1. **Global classes MUST use `PUBLIC`**:
   ```abap
   CLASS zcl_my_class DEFINITION PUBLIC.  " <- REQUIRED
   ```

2. **Use `/ui2/cl_json` for JSON**:
   ```abap
   DATA ls_data TYPE ty_request.
   ls_data = /ui2/cl_json=>deserialize( json = lv_json ).
   lv_json = /ui2/cl_json=>serialize( data = ls_response ).
   ```

3. **Test class name max 30 chars**: `ltcl_util` (not `ltcl_abgagt_util_test`)

4. **Interface method implementation**: Use prefix `zif_interface~method_name`

5. **abapGit files need XML metadata**: `.clas.xml`, `.intf.xml` alongside `.clas.abap`, `.intf.abap`

## Workflow

1. Edit ABAP file in `/abap/`
2. `git add <file> && git commit -m "..." && git push`
3. `abapgit-agent pull --files <file>` (seconds, not minutes)

## Error Handling

- Activation fails with "Error updating where-used list" = **syntax error**
- Use `abapgit-agent inspect --files <file>` for detailed error messages

## Object Naming

| Pattern | Type |
|---------|------|
| `ZCL_*` | Class |
| `ZIF_*` | Interface |
| `Z*` | Other objects |
