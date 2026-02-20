# abapGit Object XML Metadata

Each ABAP object requires an XML metadata file for abapGit to understand how to serialize/deserialize it. This guide provides templates for common object types.

## Why XML Metadata?

abapGit needs XML files to:
- Know the object type and serializer to use
- Store object attributes (description, exposure, state, etc.)
- Handle object-specific configurations

## Object Types and XML Templates

### Class (CLAS)

**Filename**: `src/zcl_my_class.clas.xml`

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_CLAS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOCLASS>
    <CLSNAME>ZCL_MY_CLASS</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description of the class</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOCLASS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `CLSNAME`: Class name (must match filename)
- `DESCRIPT`: Class description
- `EXPOSURE`: Exposure (2 = Public, 3 = Protected, 4 = Private)
- `STATE`: State (1 = Active)
- `UNICODE`: Unicode encoding (X = Yes)

---

### Interface (INTF)

**Filename**: `src/zif_my_interface.intf.xml`

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <VSEOINTERF>
    <CLSNAME>ZIF_MY_INTERFACE</CLSNAME>
    <LANGU>E</LANGU>
    <DESCRIPT>Description of the interface</DESCRIPT>
    <EXPOSURE>2</EXPOSURE>
    <STATE>1</STATE>
    <UNICODE>X</UNICODE>
   </VSEOINTERF>
  </asx:values>
 </asx:abap>
</abapGit>
```

---

### Program (PROG)

**Filename**: `src/zmy_program.prog.xml`

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>ZMY_PROGRAM</NAME>
    <SUBC>I</SUBC>
    <RLOAD>E</RLOAD>
    <FIXPT>X</FIXPT>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `NAME`: Program name
- `SUBC`: Subc (I = Include, 1 = Executable, F = Function Group, M = Module Pool, S = Subroutine Pool)
- `RLOAD`: Rload (E = External, I = Internal)

---

### Table (TABL)

**Filename**: `src/zmy_table.tabl.xml`

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZMY_TABLE</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>Description of the table</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
   </DD02V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `TABNAME`: Table name
- `DDTEXT`: Description (NOT DESCRIPT)
- `TABCLASS`: Table class (TRANSP = Transparent)
- `CONTFLAG`: Delivery class (A = Application table)

---

### CDS View (DDLS)

**Filename**: `src/zc_my_view.ddls.xml`

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DDLS" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DDLS>
    <DDLNAME>ZC_MY_VIEW</DDLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>My CDS View</DDTEXT>
    <SOURCE_TYPE>V</SOURCE_TYPE>
   </DDLS>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Points for CDS Views**:
1. **ABAP file extension**: Use `.ddls.asddls` (NOT `.ddls.abap`)
2. **XML file**: Use `.ddls.xml`
3. **DDLNAME**: Must match the CDS view name in the source
4. **SOURCE_TYPE**: V = View, C = Consumption
5. **Serializer**: Use `LCL_OBJECT_DDLS`

---

### Data Element (DTEL)

**Filename**: `src/zmy_dtel.dtel.xml`

```xml
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V>
    <ROLLNAME>ZMY_DTEL</ROLLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DDTEXT>Description of data element</DDTEXT>
    <REPTEXT>Description</REPTEXT>
    <SCRTEXT_S>Short</SCRTEXT_S>
    <SCRTEXT_M>Medium</SCRTEXT_M>
    <SCRTEXT_L>Long Description</SCRTEXT_L>
    <DTELMASTER>E</DTELMASTER>
    <DATATYPE>CHAR</DATATYPE>
    <LENG>000010</LENG>
   </DD04V>
  </asx:values>
 </asx:abap>
</abapGit>
```

**Key Fields**:
- `ROLLNAME`: Data element name
- `DDTEXT`: Description (NOT DESCRIPT)
- `DATATYPE`: Data type (CHAR, NUMC, etc.)
- `LENG`: Length (e.g., 000010 for 10 characters)

---

## Important Notes

1. **ALWAYS push to git BEFORE running pull** - abapGit reads from git
2. **Check pull output** to verify objects were recognized by abapGit
3. **Use inspect AFTER pull** to check syntax on objects in ABAP
