# ABAP Development

This project uses [abapgit-agent](https://github.com/SylvosCai/abapgit-agent) for ABAP development.

## IMPORTANT: Before Writing Any ABAP Code

Read the condensed AI instructions by running:

```bash
abapgit-agent guide --ai
```

**Never pipe `abapgit-agent guide --ai` or `abapgit-agent ref` through `head`, `tail`, or any other truncation command. Always read the full output.**

This covers: all AI tool rules, the workflow decision tree, and the full guidelines index.
For the complete human-readable guide (workflow examples, explanations): `abapgit-agent guide`

> **Humans:** Full guide online at https://sylvoscai.github.io/abapgit-agent/pages/abap-coding-guidelines.html

## Accessing Guidelines

Search by keyword to find which file covers a topic, then read the full file with `--topic`:

```bash
# Find files matching a keyword
abapgit-agent ref "debug session"

# Read a full guideline file by filename stem
abapgit-agent ref --topic debug-session       # guidelines/debug-session.md
abapgit-agent ref --topic workflow-detailed   # guidelines/workflow-detailed.md
abapgit-agent ref --topic object-creation     # guidelines/object-creation.md
abapgit-agent ref --topic branch-workflow     # guidelines/branch-workflow.md
abapgit-agent ref --topic debug-dump          # guidelines/debug-dump.md
abapgit-agent ref --topic run-probe-classes   # guidelines/run-probe-classes.md
abapgit-agent ref --topic cds-testing         # guidelines/cds-testing.md

# Also available: sql, cds, exceptions, json, testing, objects, abapgit, classes, ...
abapgit-agent ref --list-topics
```

The `ref` command uses bundled guidelines automatically — no local `guidelines/` folder needed.

## Project-Specific Naming Conventions

See `guidelines/objects.local.md` for this project's naming convention overrides.

## CLI Commands

Run `abapgit-agent --help` to see all available commands (pull, customize, transport, preview, and more).
