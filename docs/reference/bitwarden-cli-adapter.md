---
layout: default
title: Bitwarden CLI Adapter Example
parent: Reference
nav_order: 5
permalink: /docs/reference/bitwarden-cli-adapter/
---

# User-created Bitwarden CLI adapter

This standalone example implements the custom `cli` credential contract. This branch provides the central dispatcher and `credential register/create` commands on POSIX systems. The helper remains independently maintained and must be registered explicitly; copying it alone does not bind a repository to central credentials. These examples are source-repository files, not included in the npm package.

Choose this instead of the built-in Bitwarden local-serve provider when you do not want a vault HTTP server. There is no `bw serve`, automatic login/unlock/sync, vault search or write. Only an exact **Login item UUID** is accepted; Secure Notes and custom-field PATs are not supported by this example.

## 1. Install and authenticate yourself

Use Node.js 18+ and the official Bitwarden CLI. Copy [bitwarden-cli.cjs](../../examples/credentials/bitwarden-cli.cjs) into a trusted user-owned directory, for example `/Users/me/bin/bitwarden-cli.cjs`. Use absolute paths to your trusted Node executable, helper and `bw` executable; replace the illustrative paths below with your installations.

**Create a dedicated restricted Bitwarden account containing only the credentials required by abapgit-agent/Pi. Do not use your personal vault or an administrator account.** If credentials are shared through an organization, grant access only to the required collections, with permission to read the credentials and no unnecessary administrative or group grants. Check that the account cannot access unrelated credentials.

Use a separate CLI profile by setting `BITWARDENCLI_APPDATA_DIR` to a dedicated directory before logging in. Start Pi from the terminal that holds this restricted account's `BW_SESSION`; an already-running Pi does not inherit a session exported in another terminal.

Log in and unlock using the official CLI yourself. Export the resulting `BW_SESSION` to the consuming terminal using Bitwarden's documented session setup. This example requires that existing session; it never prompts for or handles a master password. Do not put session keys/passwords in the catalog, lookup arguments, source files or shell history. The CLI account/server selection and vault synchronization are also user-managed.

## 2. Register the helper

```bash
abapgit-agent credential register bitwarden-cli \
  --executable /opt/homebrew/bin/node \
  --arg /Users/me/bin/bitwarden-cli.cjs \
  --arg /opt/homebrew/bin/bw
```

The equivalent trusted personal catalog fragment is:

```json
{
  "cliAdapters": {
    "bitwarden-cli": {
      "executable": "/opt/homebrew/bin/node",
      "args": ["/Users/me/bin/bitwarden-cli.cjs", "/opt/homebrew/bin/bw"]
    }
  }
}
```

`bitwarden-cli` is a custom name. The provider names `file`, `cli` and `bitwarden` are reserved adapter names. The Node executable launches the script, so no shebang or executable bit is required on the helper itself. Keep all three paths outside untrusted repository-controlled code.

## 3. Reference a Login item

For a Git profile whose Login item contains the username and token/password:

```bash
abapgit-agent credential create work-git --origin https://github.tools.sap \
  --provider cli --adapter bitwarden-cli \
  --arg 11111111-1111-4111-8111-111111111111
```

For a client on an existing central SAP system:

```bash
abapgit-agent logon create 100 --system DEV \
  --provider cli --adapter bitwarden-cli \
  --arg 22222222-2222-4222-8222-222222222222
```

Both store this credential shape (with their respective item UUID):

```json
{
  "provider": "cli",
  "adapter": "bitwarden-cli",
  "args": ["11111111-1111-4111-8111-111111111111"]
}
```

Registration prefix arguments plus credential arguments produce one invocation:

```text
/opt/homebrew/bin/node /Users/me/bin/bitwarden-cli.cjs /opt/homebrew/bin/bw <Login UUID>
```

The helper runs `bw --nointeraction --raw status`, requires `unlocked`, then runs `bw --nointeraction --raw get item <UUID>`. It checks the returned ID, Login type and nonempty username/password before emitting exactly one `{user,password}` JSON object. Password bytes are preserved. Stdin is closed; the session is inherited through the environment, never command arguments.

**Do not run that invocation directly into your terminal:** its stdout is the credential. The dispatcher captures it privately; metadata listing/creation never executes it. Never use shell tracing or log the helper result.

## Safety and checks

The helper uses no shell and enforces a shared 12-second deadline and 64-KiB captured-output budget across both CLI calls. It kills the direct `bw` process on timeout and returns a generic error with no credential output on failure. The outer dispatcher still owns its 15-second bound, strict result validation and POSIX foreground-process-group cleanup (detached/daemonizing credential children are unsupported); this example does not implement that dispatcher or a sandbox.

An unlocked CLI session grants access to everything available to that restricted account, not just the item UUID passed to the helper. Avoiding a listening HTTP server reduces exposure, but is not an item-scoped permission. Trusted helpers and other processes with access to your user environment can access the session. Clear the session and lock the vault when finished using Bitwarden's documented procedure.

Run the isolated tests from the repository root:

```bash
npm test -- --runInBand tests/unit/bitwarden-cli-example.test.js
```

Tests mock the CLI and use synthetic credentials; they do not log in, unlock or contact a real vault. CLI flags were checked against installed Bitwarden CLI 2026.8.0 source; actual authenticated compatibility has not been verified.

See the [credential command spec](../utility/credential-command.md) and [provider architecture](../architecture/central-systems-client-logons.md) for the full contract.
