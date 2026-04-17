---
nav_exclude: true
---

# abapGit Agent — CLI Tool Development

This is the **abapgit-agent** Node.js CLI project. It has two development surfaces:

- **Node.js CLI** (`src/`, `tests/`) — commands, utils, tests
- **ABAP backend** (`abap/`) — REST handlers, command classes, viewers

## ABAP Development

Run the full ABAP development guide before writing any ABAP code:

```bash
abapgit-agent guide
```

**Never pipe `abapgit-agent guide` or `abapgit-agent ref` through `head`, `tail`, or any truncation command. Always read the full output.**

## Node.js Development Workflow

```bash
npm test                          # Unit tests (Jest, no ABAP needed)
node bin/abapgit-agent <command>  # Manual test
npm run test:integration          # Integration tests (requires .abapGitAgent)
```

See [docs/integration-tests.md](docs/integration-tests.md) for the full integration test guide.

## Adding a New Command

When creating a new CLI command, complete **all** of the following:

### 1. Implement `src/commands/<name>.js`

- Export `{ name, description, requiresAbapConfig, execute(args, context) }`
- First thing in `execute`: check `--help` / `-h` **before any validation**

```js
if (args.includes('--help') || args.includes('-h')) {
  console.log(`
Usage:
  abapgit-agent <name> [options]
...
`);
  return;
}
```

### 2. Register in `src/commands/index.js`

Add an entry to the command map.

### 3. Write a unit test `tests/unit/<name>-command.test.js`

One test file per command. See existing tests for the pattern.

### 4. Create the command spec `docs/<category>/<name>-command.md`

Use `docs/_template-command.md` as the starting point.

Categories and their Jekyll `parent:` value:

| Category | Folder | `parent:` |
|---|---|---|
| Development | `docs/development/` | `Development Commands` |
| Explore | `docs/explore/` | `Explore Commands` |
| Setup | `docs/setup/` | `Setup Commands` |
| Utility | `docs/utility/` | `Utility Commands` |

### 5. Add to `docs/commands.md`

Add a row to the commands overview table.

### 6. Add to `README.md`

Add a line under the relevant CLI section.

### 7. Add to ABAP backend (if needed)

If the command calls the ABAP system, add:
- `abap/zcl_abgagt_resource_<name>.clas.abap` + `.clas.xml`
- `abap/zcl_abgagt_command_<name>.clas.abap` + `.clas.xml`
- Register in `abap/zcl_abgagt_cmd_factory.clas.abap`
- Register in `abap/zcl_abgagt_rest_handler.clas.abap`

See [docs/architecture/](docs/architecture/) for the layered architecture overview.
