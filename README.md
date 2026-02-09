# abapGit Agent

A local agent that enables AI coding tools (Claude, Copilot, etc.) to automatically pull and activate ABAP code from git repositories using REST API.

## Overview

This project provides a bridge between AI coding tools and your ABAP system:

1. **Claude generates ABAP code** → Push to git
2. **Local agent pulls from git** → Activates in ABAP system
3. **Returns activation results** → Claude fixes errors if any

## Quick Start

1. **Install CLI**: From this repo, run `sudo npm link`
2. **Configure repo**: Add `.abapGitAgent` with SAP credentials
3. **Validate code**: Run `abapgit-agent pull` after pushing

See [Creating New ABAP Projects](INSTALL.md#creating-new-abap-projects) to set up a new ABAP repository with Claude Code integration.

## CLI Commands

```bash
# Pull and activate (auto-detects git remote and branch)
abapgit-agent pull

# Pull with specific branch
abapgit-agent pull --branch develop

# Pull specific files only (fast iteration)
abapgit-agent pull --files zcl_my_class.clas.abap,zif_my_intf.intf.abap

# Override git URL if needed
abapgit-agent pull --url https://github.com/user/repo --branch main

# Inspect source file for issues
abapgit-agent inspect --files abap/zcl_my_class.clas.abap

# Health check
abapgit-agent health

# Check integration status
abapgit-agent status

# Run unit tests (TODO: In Progress)
abapgit-agent unit --files abap/zcl_my_test.clas.abap
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
| REST API Reference | [API.md](API.md) |
| Error Handling | [ERROR_HANDLING.md](ERROR_HANDLING.md) |
| ABAP Coding Guidelines | [abap/CLAUDE.md](abap/CLAUDE.md) |
| claude-mem Knowledge Management | [CLAUDE_MEM.md](CLAUDE_MEM.md) |

## License

MIT License
