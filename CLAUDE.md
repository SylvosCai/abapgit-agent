# ABAP Git Agent - CLI Tool Development

This is the **abapgit-agent** CLI tool project - a Node.js application for pulling and activating ABAP code from git repositories.

## Quick Reference

```bash
# Quick commands
abapgit-agent pull                              # Pull and activate all
abapgit-agent pull --files src/zcl_my_class.clas.abap      # Pull specific file(s)
abapgit-agent inspect --files src/zcl_my_class.clas.abap    # Syntax check
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap  # Run tests
abapgit-agent preview --objects TABLE           # Preview table data
abapgit-agent view --objects OBJ               # View object definition
abapgit-agent tree --package '$PACKAGE'          # Show package hierarchy
abapgit-agent ref "PATTERN"                    # Search ABAP reference (cheat sheets + guidelines)
```

## When Working on Unfamiliar ABAP Topics

**IMPORTANT**: When working on unfamiliar ABAP syntax, patterns, or APIs, ALWAYS use the `ref` command first:

```bash
# Search for a specific pattern (searches SAP cheat sheets + custom guidelines)
abapgit-agent ref "CORRESPONDING"
abapgit-agent ref "FILTER #"
abapgit-agent ref "VALUE #("

# Browse by topic
abapgit-agent ref --topic exceptions
abapgit-agent ref --topic sql
abapgit-agent ref --topic internal-tables

# List all available topics
abapgit-agent ref --list-topics
```

This ensures you use correct ABAP syntax rather than guessing.

## Common Tasks

**Making code changes:**
```bash
1. Edit ABAP file(s)
2. git add && git commit && git push
3. abapgit-agent pull --files src/zcl_my_class.clas.abap
4. Check for errors in output
5. If "Error updating where-used list" → use inspect for details
```

**Exploring unknown tables:**
```bash
abapgit-agent preview --objects SFLIGHT              # See sample data
abapgit-agent view --objects SFLIGHT --type TABL    # See structure
```

## IMPORTANT: When Working on ABAP Files

When making changes to files in the `abap/` folder, **always read `abap/CLAUDE.md` first**.

This file contains critical ABAP development rules including:
- Never run inspect before commit/push/pull
- Always push to git BEFORE running pull
- Use inspect AFTER pull to check syntax
- ABAP naming conventions

## Project Structure

```
abapgit-agent/
├── bin/
│   └── abapgit-agent        # CLI entry point
├── src/
│   ├── agent.js             # Main agent class
│   ├── abap-client.js       # REST client for ABAP communication
│   ├── config.js            # Configuration management
│   ├── server.js            # HTTP server
│   └── logger.js            # Logging utilities
├── abap/                    # ABAP backend components
│   ├── zcl_abapgit_agent*.clas.abap    # Main agent class
│   ├── zif_abapgit_agent.intf.abap     # Agent interface
│   ├── zcl_abgagt_cmd_factory.clas.abap # Command factory
│   ├── zcl_abgagt_command_*.clas.abap   # Command implementations
│   ├── zif_abgagt_command.intf.abap     # Command interface
│   ├── zcl_abgagt_resource_*.clas.abap  # REST resource handlers
│   └── CLAUDE.md            # ABAP project guidelines
├── docs/                    # Command documentation
│   ├── commands.md          # All commands overview
│   ├── pull-command.md      # Pull command detailed spec
│   ├── syntax-command.md    # Syntax command detailed spec
│   └── ...                  # Other command specs
└── tests/
```

## ABAP Architecture

### Call Stack
```
CLI (bin/abapgit-agent)
    ↓
REST Client (src/abap-client.js)
    ↓
ABAP REST Handler (ZCL_ABGAGT_REST_HANDLER)
    ↓
Resource: ZCL_ABGAGT_RESOURCE_PULL → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_PULL → ZCL_ABGAGT_AGENT
Resource: ZCL_ABGAGT_RESOURCE_INSPECT → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_INSPECT → ZCL_ABGAGT_AGENT
Resource: ZCL_ABGAGT_RESOURCE_UNIT → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_UNIT → ZCL_ABGAGT_AGENT
Resource: ZCL_ABGAGT_RESOURCE_TREE → ZCL_ABGAGT_CMD_FACTORY → ZCL_ABGAGT_COMMAND_TREE → ZCL_ABGAGT_AGENT
```

### ABAP Objects

| Object | Description |
|--------|-------------|
| `ZCL_ABGAGT_AGENT` | Main agent - handles pull, inspect, unit operations |
| `ZCL_ABGAGT_CMD_FACTORY` | Command factory - creates command instances dynamically |
| `ZCL_ABGAGT_COMMAND_PULL` | Pull command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_INSPECT` | Inspect command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_SYNTAX` | Syntax command - check source without activation |
| `ZCL_ABGAGT_COMMAND_UNIT` | Unit command - implements ZIF_ABGAGT_COMMAND |
| `ZCL_ABGAGT_COMMAND_TREE` | Tree command - displays package hierarchy |
| `ZIF_ABGAGT_COMMAND` | Command interface with constants |
| `ZIF_ABGAGT_CMD_FACTORY` | Factory interface |

### Syntax Checker Architecture

The syntax checker uses object-type based implementations with dynamic instantiation:

| Object | Description |
|--------|-------------|
| `ZIF_ABGAGT_SYNTAX_CHECKER` | Interface for syntax checkers |
| `ZCL_ABGAGT_SYNTAX_CHK_CLAS` | Class checker (supports local classes) |
| `ZCL_ABGAGT_SYNTAX_CHK_INTF` | Interface checker |
| `ZCL_ABGAGT_SYNTAX_CHK_PROG` | Program checker |
| `ZCL_ABGAGT_SYNTAX_CHK_FACTORY` | Factory - creates checkers by TADIR type |

**Naming convention:** `ZCL_ABGAGT_SYNTAX_CHK_{TADIR_TYPE}` enables dynamic instantiation.

## CLI Commands Overview

> **📖 For detailed command specifications, see [docs/commands.md](docs/commands.md)**

### Command Categories

| Category | Commands | Purpose |
|----------|----------|---------|
| **Development** | `syntax`, `pull`, `inspect`, `unit` | Core development workflow |
| **Exploration** | `view`, `preview`, `tree`, `list`, `where` | Explore ABAP system |
| **Setup** | `init`, `create`, `import`, `delete` | Repository setup |
| **Utility** | `ref`, `status`, `health` | Reference & diagnostics |

### When to Use Which Command

| Scenario | Command | Documentation |
|----------|---------|---------------|
| Check LOCAL syntax BEFORE commit (CLAS/INTF/PROG only) | `syntax` | [docs/syntax-command.md](docs/syntax-command.md) |
| Activate code AFTER push to git | `pull` | [docs/pull-command.md](docs/pull-command.md) |
| Check activated code with Code Inspector | `inspect` | [docs/inspect-command.md](docs/inspect-command.md) |
| Run unit tests | `unit` | [docs/unit-command.md](docs/unit-command.md) |
| Explore package structure | `tree` | [docs/tree-command.md](docs/tree-command.md) |
| Understand table/class/interface | `view` | [docs/view-command.md](docs/view-command.md) |
| See table data | `preview` | [docs/preview-command.md](docs/preview-command.md) |
| Find where object is used | `where` | [docs/where-command.md](docs/where-command.md) |
| Search ABAP reference docs | `ref` | [docs/ref-command.md](docs/ref-command.md) |

### Key Command Characteristics

#### syntax Command
- ✅ Checks LOCAL source files (no commit/push needed)
- ✅ Works for NEW objects that don't exist in ABAP system yet
- ⚠️ **Files checked INDEPENDENTLY** - no cross-file dependencies
- ✅ Supported: CLAS, INTF, PROG
- ❌ Not supported: DDLS, FUGR, TABL, etc.
- **Use for**: Independent files only (not interface + implementing class)
- **See**: [docs/syntax-command.md](docs/syntax-command.md)

#### pull Command
- ✅ Activates code from git in ABAP system
- ✅ Handles dependencies correctly (activates in correct order)
- ⚠️ **CRITICAL**: Always pull ALL changed files together (don't pull one by one)
- ⚠️ **CRITICAL**: Always push to git BEFORE running pull
- **See**: [docs/pull-command.md](docs/pull-command.md)

#### inspect Command
- ✅ Runs Code Inspector on ACTIVATED code
- ✅ Works AFTER pull
- ✅ Supports all object types (CLAS, INTF, DDLS, etc.)
- ✅ Shows warnings and info messages
- **See**: [docs/inspect-command.md](docs/inspect-command.md)

#### view Command
- ✅ **PRIMARY way to explore unfamiliar objects**
- ✅ View table structure, class methods, interface definitions
- ✅ Works without git (reads from ABAP system)
- **See**: [docs/view-command.md](docs/view-command.md)

#### preview Command
- ✅ **PRIMARY way to explore table/CDS view DATA**
- ✅ See sample rows, filter, paginate
- **See**: [docs/preview-command.md](docs/preview-command.md)

## Development Workflow

### For CLI Tool Development (Node.js)

1. Make changes to CLI code (JavaScript in `src/`)
2. Test locally:
   ```bash
   node bin/abapgit-agent --help
   node bin/abapgit-agent <command> --help
   ```
3. Test against real ABAP system (requires configured `.abapGitAgent`)
4. Commit and push

### For ABAP Backend Development

**See [abap/CLAUDE.md](abap/CLAUDE.md) for complete workflow.**

Key workflow:
```
1. Read .abapGitAgent → get folder value
2. Research → use ref command for unfamiliar topics
3. Write code → place in correct folder
4. Syntax check (CLAS/INTF/PROG only):
   - Independent files → use syntax command
   - Dependent files → skip syntax
5. Commit and push
6. Activate → pull command
7. Verify → check output
8. (Optional) Run unit tests
```

### Decision Tree: When to Use syntax vs pull

```
Modified ABAP files?
├─ CLAS/INTF/PROG files?
│  ├─ Independent files (no cross-dependencies)?
│  │  └─ ✅ Use: syntax → commit → push → pull
│  └─ Dependent files (interface + class, class uses class)?
│     └─ ✅ Use: skip syntax → commit → push → pull
└─ Other types (DDLS, FUGR, TABL, etc.)?
   └─ ✅ Use: skip syntax → commit → push → pull → (if errors: inspect)
```

## Configuration

### File-based (.abapGitAgent)
Create `.abapGitAgent` in repository root:
```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "git-username",
  "gitPassword": "git-token",
  "transport": "DEVK900001"
}
```

### Environment Variables
```bash
export ABAP_HOST="your-sap-system.com"
export ABAP_PORT=443
export ABAP_CLIENT="100"
export ABAP_USER="TECH_USER"
export ABAP_PASSWORD="your-password"
export ABAP_LANGUAGE="EN"
export GIT_USERNAME="git-username"
export GIT_PASSWORD="git-token"
export ABAP_TRANSPORT="DEVK900001"
```

### Transport Request Precedence

When running `pull` command, the transport request is determined in this order:

| Priority | Source | Example |
|----------|--------|---------|
| 1 | CLI `--transport` argument | `--transport DEVK900001` |
| 2 | Config file `transport` | `"transport": "DEVK900001"` |
| 3 | Environment variable `ABAP_TRANSPORT` | `export ABAP_TRANSPORT="DEVK900001"` |
| 4 (default) | Not set | abapGit creates/uses default |

## Troubleshooting

**Pull fails with "Error updating where-used list":**
- This means SYNTAX ERROR in the object
- Run `abapgit-agent inspect --files <file>` for detailed errors

**Pull shows "Failed Objects" > 0:**
- Objects failed to activate - check the error messages
- Fix syntax errors and pull again

**Preview/View command fails:**
- Check table/view exists in the ABAP system
- Verify credentials in `.abapGitAgent`

**"Table or view not found":**
- Table may not exist or may have different name
- Use `view` command to check available tables

## Command Documentation

All command specifications are maintained in the `docs/` folder:

| Command | Documentation |
|---------|---------------|
| Overview | [docs/commands.md](docs/commands.md) |
| pull | [docs/pull-command.md](docs/pull-command.md) |
| syntax | [docs/syntax-command.md](docs/syntax-command.md) |
| inspect | [docs/inspect-command.md](docs/inspect-command.md) |
| unit | [docs/unit-command.md](docs/unit-command.md) |
| tree | [docs/tree-command.md](docs/tree-command.md) |
| view | [docs/view-command.md](docs/view-command.md) |
| preview | [docs/preview-command.md](docs/preview-command.md) |
| where | [docs/where-command.md](docs/where-command.md) |
| ref | [docs/ref-command.md](docs/ref-command.md) |
| init | [docs/init-command.md](docs/init-command.md) |
| create | [docs/create-command.md](docs/create-command.md) |
| import | [docs/import-command.md](docs/import-command.md) |
| delete | [docs/delete-command.md](docs/delete-command.md) |
| list | [docs/list-command.md](docs/list-command.md) |
| status | [docs/status-command.md](docs/status-command.md) |
| health | [docs/health-command.md](docs/health-command.md) |

## For ABAP Code Generation

**NOTE**: This file is for developing the CLI tool itself. For guidelines on **generating ABAP code** for abapGit repositories, see [abap/CLAUDE.md](abap/CLAUDE.md).
