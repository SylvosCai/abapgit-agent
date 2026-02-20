# ABAP Coding Guidelines Index

This folder contains detailed ABAP coding guidelines that can be searched using the `ref` command.

## Guidelines Available

| File | Topic |
|------|-------|
| `01_sql.md` | ABAP SQL Best Practices |
| `02_exceptions.md` | Exception Handling |
| `03_testing.md` | Unit Testing (including CDS) |
| `04_cds.md` | CDS Views |
| `05_classes.md` | ABAP Classes and Objects |
| `06_objects.md` | XML Metadata, Naming Conventions |
| `07_json.md` | JSON Handling |

## Usage

These guidelines are automatically searched by the `ref` command:

```bash
# Search across all guidelines
abapgit-agent ref "CORRESPONDING"

# List all topics
abapgit-agent ref --list-topics
```

## Adding Custom Guidelines

To add your own guidelines:
1. Create a new `.md` file in this folder
2. Follow the naming convention: `XX_name.md`
3. Export to reference folder: `abapgit-agent ref export`
