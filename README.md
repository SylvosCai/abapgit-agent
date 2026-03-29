---
title: abapGit Agent
nav_order: 1
permalink: /
---

# abapGit Agent

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using REST API.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

[![Using abapgit-agent in Claude Code](https://img.youtube.com/vi/OkGlOsnXB4o/maxresdefault.jpg?v=2)](https://www.youtube.com/watch?v=OkGlOsnXB4o)

## Quick Start

### For Existing Repositories

```bash
# 1. Install CLI
npm install -g abapgit-agent

# 2. Clone repo
git clone https://github.com/user/abap-project.git
cd abap-project

# 3. Initialize configuration
abapgit-agent init --package ZMY_PACKAGE

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

### Setup

```bash
abapgit-agent init --package ZMY_PACKAGE   # Initialize local config
abapgit-agent create                        # Create online repo in ABAP
abapgit-agent import                        # Import objects from ABAP to git
abapgit-agent delete                        # Delete repo from ABAP
```

### Development

```bash
abapgit-agent pull                                         # Pull and activate
abapgit-agent pull --files src/zcl_my_class.clas.abap     # Pull specific files
abapgit-agent syntax --files src/zcl_my_class.clas.abap   # Check syntax before commit
abapgit-agent inspect --files src/zcl_my_class.clas.abap  # Code Inspector after pull
abapgit-agent unit --files src/zcl_my_test.clas.testclasses.abap  # Run AUnit tests
abapgit-agent run --class ZCL_MY_RUNNER                   # Execute class headlessly
```

### Explore

```bash
abapgit-agent tree --package '$MY_PACKAGE'   # Package hierarchy
abapgit-agent list --package '$MY_PACKAGE'   # List objects
abapgit-agent view --objects ZCL_MY_CLASS    # View object definition
abapgit-agent preview --objects SFLIGHT      # Preview table data
abapgit-agent where --objects ZCL_MY_CLASS   # Where-used list
abapgit-agent dump --date TODAY              # Query short dumps (ST22)
abapgit-agent debug set --files abap/zcl_my_class.clas.abap:42  # Set breakpoint
```

### Utility

```bash
abapgit-agent guide                   # Read full ABAP development guide
abapgit-agent ref "CORRESPONDING"     # Search ABAP reference
abapgit-agent ref --topic sql         # Browse by topic
abapgit-agent transport list          # List transport requests
abapgit-agent upgrade                 # Upgrade CLI and ABAP backend
abapgit-agent status                  # Check configuration
abapgit-agent health                  # Verify ABAP connection
```

## Local Development

```bash
# Install dependencies
npm install

# Run unit tests (no ABAP system needed)
npm test

# Test a command manually
node bin/abapgit-agent --help
node bin/abapgit-agent syntax --files src/zcl_my_class.clas.abap

# Run integration tests against a real ABAP system (requires .abapGitAgent)
npm run test:integration
```

## Documentation

| Topic | File |
|-------|------|
| Full Documentation | [https://sylvoscai.github.io/abapgit-agent/](https://sylvoscai.github.io/abapgit-agent/) |
| Installation & Setup | [INSTALL.md](INSTALL.md) |
| All Commands Overview | [docs/commands.md](docs/commands.md) |
| REST API Reference | [API.md](API.md) |

## Dependent Projects

This tool depends on and is designed to work with:

- [abapGit](https://github.com/abapGit/abapGit) - The foundation for managing ABAP code in git

## License

MIT License
