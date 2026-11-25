---
nav_exclude: true
search_exclude: true
---

# Central configuration example pair

These two files show the two locations used by central configuration:

- `examples/configuration/central/repository.abapGitAgent.json` is the repository-local binding. Copy its shape into the ignored `.abapGitAgent` in the repository root, then keep project settings there as needed.
- `examples/configuration/central/systems.json` is the personal catalog shape. Merge the required entries into `~/.config/abapgit-agent/systems.json`; do not overwrite an existing catalog.

The JSON source examples are repository/package files, not Jekyll downloads; the documentation site deliberately excludes JSON files.

The values are placeholders. The setup commands create metadata and empty file-provider drafts; they do not accept secrets. Add file-provider passwords or tokens only to the private personal catalog, keep its directory and file permissions private, and never commit the populated catalog.

Do not mix a central `system` reference with equivalent flat SAP endpoint/account fields, or a central `gitProfile` reference with flat Git account fields. See [Installation & Setup](../../../docs/install.md#choose-a-connection-format) for both supported formats.
