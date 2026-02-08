# claude-mem for ABAP Knowledge Management

claude-mem is a Claude Code plugin that stores memories across sessions. Use it to retain ABAP knowledge learned from the reference repositories.

## Setup

1. Install the plugin:
   ```bash
   /plugin marketplace add thedotmack/claude-mem
   /plugin install claude-mem
   ```

## Quick Start: Explore All Reference Docs at Once

Use `claude-mem:make-plan` to bulk explore the abap-reference folder and save all knowledge:

1. In Claude Code, invoke the skill:
   ```
   /claude-mem:make-plan
   ```

2. Provide instructions to explore all reference repos:
   ```
   Explore the abap-reference folder at /Users/i045696/Documents/code/abap-reference

   It contains:
   - abapGit: ABAP file format patterns for abapGit deployment
   - styleguides: Clean ABAP coding standards
   - abap-cheat-sheets: Code snippets and examples

   Save all ABAP patterns and conventions to claude-mem memory:
   - ABAP class file format (ZCL_*.clas.abap, XML metadata)
   - Clean ABAP naming conventions
   - Authorization check patterns
   - Dynamic method calls
   - Syntax check via Code Inspector

   Focus on copy-ready patterns and code examples.
   ```

3. The subagent will:
   - Explore each repository
   - Extract patterns and examples
   - **Automatically save memories** to claude-mem
   - Create a plan document for reference

## Manual Knowledge Saving

When you discover specific ABAP patterns, save them:

```
/claude-mem:do
```

This launches a subagent to implement your task and **automatically save memories**.

## Retrieving Knowledge

When you need to recall previously learned ABAP patterns:

```
/claude-mem:mem-search
```

Then search for topics like:
- "ABAP class structure"
- "Clean ABAP naming"
- "Authorization check"
- "Dynamic method call"
- "abapGit syntax check"

## Key Memory Topics

| Topic | What to Save |
|-------|-------------|
| abapGit format | `ZCL_*.clas.abap`, XML metadata pattern |
| Clean ABAP | Naming conventions, patterns, anti-patterns |
| Authorization | `AUTHORITY-CHECK` syntax, activity values |
| Dynamic calls | `CALL METHOD (class)=>(method)` |
| Syntax check | `zcl_abapgit_code_inspector=>run()` |

## Workflow

1. **First time**: Use `claude-mem:make-plan` to explore and save all reference docs
2. **During work**: Use `claude-mem:do` to save new discoveries
3. **When needed**: Use `claude-mem:mem-search` to recall patterns

claude-mem persists across sessions - your learned knowledge is saved.
