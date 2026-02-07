# ABAP Project Guidelines

## ABAP Syntax Validation

This is an ABAP project. **Do not attempt local syntax validation** - ABAP code can only be validated in an SAP system.

**To validate ABAP code:**

1. After generating code, push changes to git
2. Run `abapgit-agent pull` to pull and activate in ABAP system
3. Review activation results for errors
4. If errors exist, fix them and repeat
5. Use `abapgit-agent syntax-check <type> <name>` for detailed syntax errors

```bash
# After making changes to ABAP files
git add .
git commit -m "Describe changes"
git push

# Validate in ABAP system
abapgit-agent pull

# Check specific object for syntax errors
abapgit-agent syntax-check CLAS ZCL_MY_CLASS

# Run unit tests (planned)
abapgit-agent unit --package ZMY_PACKAGE
```

## ABAP Reference

For ABAP syntax, objects, and abapGit conventions, refer to:

- **abapGit**: https://github.com/abapGit/abapGit
- abapGit documentation explains object types, serialization format, and best practices

## ABAP Object Types

Common object types in this project:
- `CLAS` - Classes
- `PROG` - Programs
- `FUGR` - Function Groups
- `INTF` - Interfaces
- `TABL` - Tables
- `DDLS` - Data Definitions

## Naming Conventions

- Use `Z_` or `Y_` prefix for custom objects
- Class names: `ZCL_<NAME>`
- Interface names: `ZIF_<NAME>`
- Programs: `Z<NAME>`
- Package: `$<PROJECT_NAME>`
