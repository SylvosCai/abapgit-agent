# abapGit Agent

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using REST API.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

![Using abapgit-agent in Claude Code](https://raw.githubusercontent.com/SylvosCai/abapgit-agent/master/img/claude.png)

## Quick Start

### For Existing Repositories

```bash
# 1. Install CLI
npm install -g abapgit-agent

# 2. Clone repo
git clone https://github.com/user/abap-project.git
cd abap-project

# 3. Initialize configuration
abapgit-agent init --folder /abap/ --package ZMY_PACKAGE

# 4. Edit .abapGitAgent with credentials

# 5. Create online repository in ABAP
abapgit-agent create

# 6. Import objects from ABAP to git
abapgit-agent import

# 7. Pull to activate in ABAP
abapgit-agent pull
```

See [Creating New ABAP Projects](INSTALL.md#creating-new-abap-projects) to set up a new ABAP repository with Claude Code integration.

## CLI Commands

### Setup Commands

```bash
# Initialize local configuration for existing git repo
abapgit-agent init --folder /abap/ --package ZMY_PACKAGE

# Create online repository in ABAP
abapgit-agent create

# Import objects from ABAP package to git
abapgit-agent import
```

### Development Commands

```bash
# Pull and activate (auto-detects git remote and branch)
abapgit-agent pull

# Pull with specific branch
abapgit-agent pull --branch develop

# Pull specific files only (fast iteration)
abapgit-agent pull --files zcl_my_class.clas.abap,zif_my_intf.intf.abap

# Override git URL if needed
abapgit-agent pull --url https://github.com/user/repo --branch main

# Pull with a specific transport request
abapgit-agent pull --transport DEVK900001

# Import objects from ABAP package to git
abapgit-agent import

# Import with custom commit message
abapgit-agent import --message "feat: add new feature"
```

### Validation Commands

```bash
# Inspect source file for issues
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Run AUnit tests for test classes
abapgit-agent unit --files abap/zcl_my_test.clas.testclasses.abap

# Run tests for multiple test classes
abapgit-agent unit --files abap/zcl_test1.clas.testclasses.abap,abap/zcl_test2.clas.testclasses.abap
```

### Utility Commands

```bash
# Health check
abapgit-agent health

# Check integration status
abapgit-agent status
```

## Local Development

```bash
# Install dependencies
cd abapgit-agent
npm install

# Run from package directory (auto-detects from git)
node bin/abapgit-agent pull

# Or use npm script
npm run pull -- --url <git-url> --branch main
```

## Documentation

| Topic | File |
|-------|------|
| Installation & Setup | [INSTALL.md](INSTALL.md) |
| init Command | [docs/init-command.md](docs/init-command.md) |
| create Command | [docs/create-command.md](docs/create-command.md) |
| import Command | [docs/import-command.md](docs/import-command.md) |
| pull Command | [docs/pull-command.md](docs/pull-command.md) |
| inspect Command | [docs/inspect-command.md](docs/inspect-command.md) |
| unit Command | [docs/unit-command.md](docs/unit-command.md) |
| REST API Reference | [API.md](API.md) |
| Error Handling | [ERROR_HANDLING.md](ERROR_HANDLING.md) |
| ABAP Coding Guidelines | [abap/CLAUDE.md](abap/CLAUDE.md) |

## Dependent Projects

This tool depends on and is designed to work with:

- [abapGit](https://github.com/abapGit/abapGit) - The foundation for managing ABAP code in git

## License

MIT License
