# ABAP Objects

## Creating New ABAP Objects - XML Metadata Required

**CRITICAL CHECKLIST - Never Forget!**

When creating ANY new ABAP object file, you MUST also create its XML metadata file:

| ABAP File | Required XML File |
|-----------|------------------|
| `zcl_my_class.clas.abap` | `zcl_my_class.clas.xml` |
| `zif_my_intf.intf.abap` | `zif_my_intf.intf.xml` |

**Without XML files**, abapGit will NOT recognize the objects during pull, and they won't be activated.

### XML Metadata Format for CLAS

For `zcl_abgagt_util.clas.abap`, create `zcl_abgagt_util.clas.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_ABGAGT_UTIL</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

### XML Metadata Format for INTF

For `zif_abgagt_util.intf.abap`, create `zif_abgagt_util.intf.xml`:

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOINTERF>
    <CLSNAME>ZIF_ABGAGT_UTIL</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOINTERF>
  </asx:values>
 </asx:abap>
</abapGit>
```

### Important Notes

1. **CRITICAL: Push to git BEFORE pulling into ABAP**
   - Always commit and push ABAP files to git first
   - Then run `abapgit-agent pull` to activate in ABAP
   - Never run `abapgit-agent pull` without pushing changes first

2. **Only pull ABAP files** - The XML metadata stays in git:
   ```bash
   abapgit-agent pull --files zcl_my_class.clas.abap
   ```
3. abapGit reads the XML from git to deserialize the ABAP code
4. XML files are NOT activated in ABAP - they are only for abapGit

```bash
# After making changes to ABAP files
git add .
git commit -m "Describe changes"
git push    # CRITICAL: Push BEFORE pulling

# Then validate in ABAP system (single file - fast)
abapgit-agent pull --files abap/zcl_my_class.clas.abap

# Or validate all files
abapgit-agent pull
```

## Naming Conventions

- Use `Z_` or `Y_` prefix for custom objects
- Class names: `ZCL_<NAME>`
- Interface names: `ZIF_<NAME>`
- Programs: `Z<NAME>`
- Package: `$<PROJECT_NAME>`

## ABAP Object Types

Common object types in this project:
- `CLAS` - Classes
- `PROG` - Programs
- `FUGR` - Function Groups
- `INTF` - Interfaces
- `TABL` - Tables
- `DDLS` - Data Definitions
