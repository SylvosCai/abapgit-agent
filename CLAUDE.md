---
nav_exclude: true
---

# ABAP Git Agent - CLI Tool Development

This is the **abapgit-agent** CLI tool project - a Node.js application for pulling and activating ABAP code from git repositories.

## Quick Reference

```bash
# Quick commands
abapgit-agent syntax --files src/zcl_my_class.clas.abap    # Check syntax BEFORE commit (local files)
abapgit-agent pull --files src/zcl_my_class.clas.abap      # Pull and activate (AFTER push)
abapgit-agent inspect --files src/zcl_my_class.clas.abap   # Code Inspector check (AFTER pull)
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap  # Run unit tests
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

### For ABAP Development Workflow

> **📖 For complete ABAP development workflow, see [abap/CLAUDE.md](abap/CLAUDE.md)**

**High-level workflow:**
```
1. Edit ABAP file(s)
2. syntax (for CLAS/INTF/PROG only - if files are independent)
3. git add && git commit && git push
4. pull --files <files>
5. Verify output (if errors: use inspect)
6. (Optional) unit tests
```

**Quick examples:**
```bash
# Check syntax before commit (CLAS/INTF/PROG only)
abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Pull and activate after push
abapgit-agent pull --files src/zcl_my_class.clas.abap

# If errors, use inspect for details
abapgit-agent inspect --files src/zcl_my_class.clas.abap
```

### For Exploring ABAP System

**Exploring unknown tables/structures:**
```bash
abapgit-agent view --objects SFLIGHT --type TABL    # See table structure
abapgit-agent preview --objects SFLIGHT              # See sample data
```

**Exploring unknown classes/interfaces:**
```bash
abapgit-agent view --objects ZCL_MY_CLASS            # See class definition
abapgit-agent where --objects ZCL_MY_CLASS           # See where it's used
```

## Project Structure

```
abapgit-agent/
├── bin/
│   ├── abapgit-agent        # CLI entry point
│   └── abgagt               # Alias for abapgit-agent
├── src/
│   ├── commands/            # Command implementations (CLI layer)
│   ├── utils/               # Utility modules
│   ├── agent.js             # Main agent class
│   ├── config.js            # Configuration management
│   ├── server.js            # HTTP server
│   ├── logger.js            # Logging utilities
│   └── index.js             # Main entry point
├── abap/                    # ABAP backend components
│   ├── guidelines/          # ABAP development guidelines
│   ├── zcl_abgagt_agent.clas.abap           # Main agent class
│   ├── zif_abgagt_agent.intf.abap           # Agent interface
│   ├── zcl_abgagt_cmd_factory.clas.abap     # Command factory
│   ├── zcl_abgagt_command_*.clas.abap       # Command implementations
│   ├── zif_abgagt_command.intf.abap         # Command interface
│   ├── zcl_abgagt_resource_*.clas.abap      # REST resource handlers
│   ├── zcl_abgagt_rest_handler.clas.abap    # REST handler
│   ├── zcl_abgagt_syntax_chk_*.clas.abap    # Syntax checkers
│   ├── zif_abgagt_syntax_checker.intf.abap  # Syntax checker interface
│   ├── zcl_abgagt_viewer_*.clas.abap        # Object viewers
│   ├── zif_abgagt_viewer.intf.abap          # Viewer interface
│   └── CLAUDE.md                            # ABAP project guidelines
├── docs/                    # Command documentation (user docs)
│   ├── commands.md          # All commands overview
│   ├── pull-command.md      # Pull command detailed spec
│   ├── syntax-command.md    # Syntax command detailed spec
│   └── ...                  # Other command specs
├── pages/                   # Website pages (Jekyll)
│   ├── getting-started.md
│   ├── development-commands.md
│   └── ...
├── tests/
│   ├── integration/         # Integration tests
│   ├── unit/                # Unit tests
│   ├── fixtures/            # Test fixtures
│   └── helpers/             # Test helpers
├── scripts/                 # Build and release scripts
├── CLAUDE.md                # Project guidelines (this file)
└── package.json             # Node.js project configuration
```

## ABAP Architecture

### Layered Architecture Pattern

```
CLI (bin/abapgit-agent)
    ↓
REST Client (src/agent.js + src/config.js)
    ↓
HTTP → ABAP REST Handler (ZCL_ABGAGT_REST_HANDLER)
    ↓
Resource Layer (ZCL_ABGAGT_RESOURCE_*)
    ↓
Command Factory (ZCL_ABGAGT_CMD_FACTORY)
    ↓
Command Layer (ZCL_ABGAGT_COMMAND_*)
    ↓
Core Agent (ZCL_ABGAGT_AGENT)
```

**Example flow:**
```
abapgit-agent pull --files ...
    ↓
ZCL_ABGAGT_RESOURCE_PULL
    ↓
ZCL_ABGAGT_CMD_FACTORY (creates COMMAND_PULL instance)
    ↓
ZCL_ABGAGT_COMMAND_PULL
    ↓
ZCL_ABGAGT_AGENT (executes pull logic)
```

### Components Overview

| Layer | Pattern | Count | Purpose |
|-------|---------|-------|---------|
| **REST Handler** | `ZCL_ABGAGT_REST_HANDLER` | 1 | Routes HTTP requests to resources |
| **Resources** | `ZCL_ABGAGT_RESOURCE_*` | 15 | REST endpoints, one per command |
| **Factory** | `ZCL_ABGAGT_CMD_FACTORY` | 1 | Creates command instances dynamically |
| **Commands** | `ZCL_ABGAGT_COMMAND_*` | 13 | Business logic (pull, syntax, inspect, unit, etc.) |
| **Core Agent** | `ZCL_ABGAGT_AGENT` | 1 | Core operations (pull, activate, etc.) |
| **Utilities** | `ZCL_ABGAGT_UTIL` | 1 | Helper methods |

All commands implement `ZIF_ABGAGT_COMMAND` interface.

### Specialized Sub-Architectures

**Syntax Checkers** (type-specific with dynamic instantiation):
- Pattern: `ZCL_ABGAGT_SYNTAX_CHK_{TYPE}` (CLAS, INTF, PROG)
- Factory: `ZCL_ABGAGT_SYNTAX_CHK_FACTORY`
- Interface: `ZIF_ABGAGT_SYNTAX_CHECKER`

**Object Viewers** (type-specific with dynamic instantiation):
- Pattern: `ZCL_ABGAGT_VIEWER_{TYPE}` (CLAS, INTF, PROG, TABL, STRU, DTEL, TTYP, DDLS)
- Factory: `ZCL_ABGAGT_VIEWER_FACTORY`
- Interface: `ZIF_ABGAGT_VIEWER`

**Key Design Principle:** Naming conventions enable dynamic instantiation via factory pattern.

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

> **For ABAP development workflow details, see [abap/CLAUDE.md](abap/CLAUDE.md)**

## Development Workflow

### For CLI Tool Development (Node.js)

**Workflow:**
```
1. Make changes to CLI code (JavaScript in src/ or tests/)
2. Run unit tests: npm test (or npm run test:unit)
3. Test locally: node bin/abapgit-agent <command>
4. (Optional) Run integration tests against ABAP system
5. Commit and push
```

**Test structure:**
```bash
# Unit tests (fast, no ABAP system needed)
npm test                    # Run all unit tests (Jest)
npm run test:unit          # Same as npm test

# Integration tests (require configured .abapGitAgent)
npm run test:integration   # Test against real ABAP system
npm run test:cmd           # Command integration tests (all commands)
npm run test:aunit         # AUnit integration tests
npm run test:lifecycle     # Full lifecycle tests
npm run test:all           # All tests (unit + integration)

# Test specific commands only (NEW - fast feedback!)
npm run test:cmd:syntax    # Test only syntax command
npm run test:cmd:pull      # Test only pull command
npm run test:cmd:inspect   # Test only inspect command
npm run test:cmd:unit      # Test only unit command
npm run test:cmd:view      # Test only view command
npm run test:cmd:preview   # Test only preview command
npm run test:cmd:tree      # Test only tree command

# Or use the --command flag directly
npm run test:cmd -- --command=syntax
npm run test:cmd -- --command=syntax --demo  # Demo mode for specific command
```

**Unit tests location:**
- `tests/unit/` - One test file per command/module
- Example: `tests/unit/syntax-command.test.js` tests `src/commands/syntax.js`
- Run fast, no external dependencies

**Integration tests location:**
- `tests/integration/` - Tests against real ABAP system
- Requires `.abapGitAgent` configuration
- Tests actual command execution end-to-end

**Manual testing:**
```bash
node bin/abapgit-agent --help
node bin/abapgit-agent syntax --files src/example.clas.abap
```

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
  "transport": "DEVK900001",

  "workflow": {
    "mode": "branch"  // "branch" or "trunk" (default: trunk)
  }
}
```

#### Workflow Configuration

The `workflow` section controls git workflow behavior for AI coding tools:

| Field | Values | Default | Description |
|-------|--------|---------|-------------|
| `mode` | `"branch"` or `"trunk"` | `"trunk"` | Workflow mode (see below) |
| `defaultBranch` | String (e.g., `"main"`) | Auto-detected | Override auto-detected default branch |

**Workflow Modes:**

- **`"branch"`**: Feature branch workflow with rebase
  - Create feature branches for new features
  - Rebase to default branch before pull
  - Create PRs with squash merge
  - Prevents polluting main branch with WIP commits

- **`"trunk"`**: Trunk-based development (default)
  - Commit directly to default branch
  - Simpler workflow for small teams
  - No rebase or PRs required

**Default Branch Detection:**

The default branch (main/master/develop) is **auto-detected** using:
1. Check `git symbolic-ref refs/remotes/origin/HEAD`
2. Check for common branch names (main → master → develop)
3. Fallback to `"main"`

You only need to set `defaultBranch` if using a non-standard branch name.

> **📖 For complete workflow details, see [abap/CLAUDE.md](abap/CLAUDE.md)**

### Project-Level Configuration (.abapgit-agent.json)

**Checked into repository** - applies to ALL developers

Create `.abapgit-agent.json` in repository root for team-wide policies:

```json
{
  "project": {
    "name": "Project Name",
    "description": "Optional project description"
  },

  "safeguards": {
    "requireFilesForPull": true,
    "disablePull": false,
    "reason": "Large project with 500+ objects. Selective activation required."
  },

  "conflictDetection": {
    "mode": "abort",
    "reason": "Multi-developer project — prevent accidental overwrites"
  },

  "transports": {
    "allowCreate": false,
    "allowRelease": false,
    "reason": "Transport requests are managed by the release manager."
  }
}
```

#### Safeguard Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `requireFilesForPull` | boolean | `false` | Requires `--files` parameter for pull command |
| `disablePull` | boolean | `false` | Completely disables pull command |
| `reason` | string | `null` | Optional explanation shown in error messages |

#### Conflict Detection Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `mode` | string | `"abort"` | `"abort"` — abort on conflict; `"ignore"` — disable detection |
| `reason` | string | `null` | Optional explanation |

CLI `--conflict-mode` flag always takes precedence over project config.

#### Transport Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `allowCreate` | boolean | `true` | When `false`, `transport create` is blocked |
| `allowRelease` | boolean | `true` | When `false`, `transport release` is blocked |
| `reason` | string | `null` | Optional explanation shown in error messages |

#### Use Cases

**Large Projects (100+ objects):**
```json
{
  "safeguards": {
    "requireFilesForPull": true,
    "reason": "Large project. Use --files to avoid timeout."
  }
}
```

**CI/CD Only Activation:**
```json
{
  "safeguards": {
    "disablePull": true,
    "reason": "All activations must go through CI/CD pipeline."
  }
}
```

**Solo Developer (disable conflict detection):**
```json
{
  "conflictDetection": {
    "mode": "ignore"
  }
}
```

#### Behavior

**When `requireFilesForPull: true`:**
- `abapgit-agent pull` → ❌ Error: --files is required
- `abapgit-agent pull --files ...` → ✅ Works

**When `disablePull: true`:**
- All `pull` commands are disabled
- Error message directs users to project maintainer

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
