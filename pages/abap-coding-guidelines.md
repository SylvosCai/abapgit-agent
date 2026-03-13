---
layout: default
title: ABAP Coding Guidelines
nav_order: 2
parent: ABAP Development
has_children: true
---

# ABAP Coding Guidelines

Detailed ABAP coding guidelines for development in this project.

## Coding Standards

| Guide | Description |
|-------|-------------|
| [SQL Best Practices](../abap/guidelines/sql.md) | Modern ABAP SQL syntax, joins, aggregations, host variables |
| [Exception Handling](../abap/guidelines/exceptions.md) | Class-based exceptions, TRY/CATCH patterns |
| [Unit Testing](../abap/guidelines/testing.md) | AUnit test structure, test doubles, assertions |
| [CDS Views](../abap/guidelines/cds.md) | CDS view entities, associations, annotations |
| [Classes & Objects](../abap/guidelines/classes.md) | OO patterns, instantiation, interfaces |
| [Naming Conventions](../abap/guidelines/objects.md) | Z/Y prefix rules, naming by object type |
| [JSON Handling](../abap/guidelines/json.md) | `/ui2/cl_json` serialization and deserialization |
| [abapGit XML Metadata](../abap/guidelines/abapgit.md) | XML templates for classes, interfaces, tables, CDS views |
| [Unit Testable Code](../abap/guidelines/unit-testable-code.md) | Dependency injection patterns for testable ABAP |
| [Common ABAP Errors](../abap/guidelines/common-errors.md) | Quick fixes for frequent activation and syntax errors |

## Debugging & Troubleshooting

| Guide | Description |
|-------|-------------|
| [Debug Session Guide](../abap/guidelines/debug-session.md) | Step-by-step debug workflow: set breakpoints, attach, inspect variables, pull flow architecture |
| [Dump Analysis Guide](../abap/guidelines/debug-dump.md) | Analyse ST22 short dumps using the `dump` command |
| [run Command Guide](../abap/guidelines/run-probe-classes.md) | Never-run-proactively rule, probe class workflow, scratchWorkspace setup |

## Workflow & Project Setup

| Guide | Description |
|-------|-------------|
| [Branch Workflow](../abap/guidelines/branch-workflow.md) | Feature branch workflow with rebase-before-pull and PR creation |
| [Development Workflow (Detailed)](../abap/guidelines/workflow-detailed.md) | Full workflow decision tree, error indicators, command reference |
| [Object Creation](../abap/guidelines/object-creation.md) | XML metadata templates and local class setup |
| [CDS Testing](../abap/guidelines/cds-testing.md) | CDS test double framework (`CL_CDS_TEST_ENVIRONMENT`) |
