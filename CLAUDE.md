# ABAP AI Bridge - CLI Tool Development

This is the **abapgit-agent** CLI tool project - a Node.js application for pulling and activating ABAP code from git repositories.

## Project Structure

```
abap-ai-bridge/
├── bin/
│   └── abapgit-agent        # CLI entry point
├── src/
│   ├── agent.js             # Main agent class
│   ├── abap-client.js       # REST client for ABAP communication
│   ├── config.js            # Configuration management
│   ├── server.js            # HTTP server
│   └── logger.js             # Logging utilities
├── abap/                    # ABAP backend components
│   ├── zcl_abapgit_agent*.clas.abap
│   ├── zif_abapgit_agent.intf.abap
│   └── CLAUDE.md            # ABAP project guidelines (copy to your ABAP repos)
└── tests/
```

## CLI Commands

```bash
# Pull and activate from current git repo
abapgit-agent pull

# Syntax check for specific object
abapgit-agent syntax-check <object_type> <object_name>

# Health check
abapgit-agent health

# Check configuration
abapgit-agent status
```

## For ABAP Code Generation

**NOTE**: This file is for developing the CLI tool itself. For guidelines on **generating ABAP code** for abapGit repositories, see `/abap/CLAUDE.md`. Copy that file to your ABAP repository root when setting up new projects.

## Development Workflow

1. Make changes to CLI code (JavaScript) or ABAP backend (abap/ folder)
2. Test locally: `node bin/abapgit-agent pull`
3. Test against real ABAP system
4. Commit and push
5. Deploy ABAP changes via abapGit to your SAP system
