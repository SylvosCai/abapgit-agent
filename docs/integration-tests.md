---
layout: default
title: Integration Tests
parent: Reference
nav_order: 4
---

# Integration Test Guide

Integration tests run the abapgit-agent CLI end-to-end against a real ABAP system.
The test suite is **self-organizing**: on the first run it automatically clones all
required test repos and activates the necessary ABAP objects. Subsequent runs skip
any setup that is already in place.

## Prerequisites

| Requirement | Notes |
|---|---|
| Node.js ≥ 16 | `node --version` |
| Configured ABAP system | BAS/on-premise with abapgit-agent ABAP backend installed |
| `.abapGitAgent` in project root | Created by `abapgit-agent init`; must contain valid credentials |
| Write access to test repos | Your own forks of the `github.com/SylvosCai/abgagt-*` reference repos — the public repos are read-only for external users. See [Test Repo Access Requirements](#test-repo-access-requirements). |

`.abapGitAgent` minimum fields needed for integration tests:

```json
{
  "host": "your-sap-system.com",
  "sapport": 443,
  "protocol": "https",
  "client": "100",
  "user": "TECH_USER",
  "password": "your-password",
  "language": "EN",
  "gitUsername": "git-user",
  "gitPassword": "git-token"
}
```

## Quick Start

```bash
# First run — setup + all tests (takes ~5 min):
npm run test:all

# Setup only (verify prerequisites are in place, then stop):
npm run test:setup

# Unit tests only (no ABAP system needed):
npm test
```

On **first run** `npm run test:all` prints a setup phase that:
1. Clones each test repo to `/tmp/abgagt-*-test/`
2. Registers it in the ABAP system (`create`)
3. Activates all test objects (`pull --conflict-mode ignore`)

On **subsequent runs** setup detects that the ABAP objects are already active and
skips all registration/activation steps — adding only a few seconds overhead.

## Test Suites

| Suite | Command | What it tests | ABAP package |
|---|---|---|---|
| Unit (Jest) | `npm test` | JavaScript code, no ABAP needed | — |
| AUnit | `npm run test:aunit` | ABAP test classes | project package |
| Command | `npm run test:cmd` | All CLI commands end-to-end | project package |
| Pull workflow | `npm run test:pull` | Git ref switching (tags/branches) | `$ABGAGT_PULL_TEST` |
| Full pull | `npm run test:full-pull` | Pull without `--files` | `$ABGAGT_PULL_TEST` |
| Conflict detection | `npm run test:conflict` | Conflict abort / ignore modes | `$ABGAGT_PULL_TEST` |
| Sync-XML | `npm run test:sync-xml` | `--sync-xml` flag end-to-end | `$ABGAGT_PULL_TEST` |
| XML-only | `npm run test:xml-only` | TABL/DTEL/TTYP via `.xml` files | `$ABGAGT_PULL_TEST` |
| Drop | `npm run test:drop` | `drop` command per object type | `$ABGAGT_DROP_TEST` |
| Customize | `npm run test:customize` | `customize` command (C-class tables) | `$ABGAGT_CUS_TST` |
| Lifecycle | `npm run test:lifecycle` | `init → create → import → delete` | `$ABGAGT_LIFECYCLE_TEST` |
| JUnit output | `npm run test:junit` | `--junit-output` flag | project package |
| Debug | `npm run test:debug` | Breakpoint management (ADT) | project package |

### Running a single command suite

```bash
npm run test:cmd -- --command=syntax   # syntax command only
npm run test:cmd -- --command=pull     # pull command only
npm run test:cmd -- --command=view
npm run test:cmd -- --command=debug
# etc.
```

## Test Repo Access Requirements

The integration tests **write** to the test repos during execution (branch creation,
pushes, abapGit registrations). This means:

- **Read access is not enough** — `gitUsername`/`gitPassword` must have **push access** to
  the configured test repo URLs.
- The `github.com/SylvosCai/abgagt-*` repos are **public reference implementations**.
  They are not writable by external contributors, so you cannot run the full integration
  suite against them directly.
- **You must fork the repos** (or host your own copies) and configure your forks via
  `testRepos` in `.abapGitAgent`.

### Forking the reference repos

1. Fork each repo on GitHub into your own account or organisation:
   - `github.com/SylvosCai/abgagt-pull-test`
   - `github.com/SylvosCai/abgagt-drop-test`
   - `github.com/SylvosCai/abgagt-customize-test`
   - `github.com/SylvosCai/abgagt-lifecycle-test`
   - `github.com/SylvosCai/abgagt-debug-test`

2. Add a `testRepos` section to `.abapGitAgent` pointing to your forks
   (this file is gitignored and never committed):

```json
{
  "testRepos": {
    "pull":      "https://github.com/your-org/abgagt-pull-test.git",
    "drop":      "https://github.com/your-org/abgagt-drop-test.git",
    "customize": "https://github.com/your-org/abgagt-customize-test.git",
    "lifecycle": "https://github.com/your-org/abgagt-lifecycle-test.git",
    "debug":     "https://github.com/your-org/abgagt-debug-test.git"
  }
}
```

Any key that is absent falls back to the public default URL, so you only need to
specify repos you are overriding.

3. Ensure `gitUsername`/`gitPassword` in `.abapGitAgent` have **push access** to all
   configured repos.

## Skipping Auto-Setup

If you manage test prerequisites manually (e.g. in CI where setup already ran
in a prior stage), pass `--no-setup` to skip the setup phase:

```bash
npm run test:all -- --no-setup
```

## CI Environment Variables

In CI pipelines you can override git credentials for test repos that live under
a different account than the main project:

| Variable | Purpose |
|---|---|
| `TEST_GIT_USR` | Git username injected into test repo `.abapGitAgent` |
| `TEST_GIT_PSW` | Git password/token injected into test repo `.abapGitAgent` |

## Troubleshooting

### Setup fails with "cannot clone repo"

- Check that the repo URL is reachable and your `gitUsername`/`gitPassword` have read access.
- If using `testRepos` overrides, verify the URL is correct.
- Stale clones at `/tmp/abgagt-*-test/` with a broken `.git/` are auto-detected and re-cloned.

### Setup fails with "create failed"

- The ABAP system may not have the required package (e.g. `$ABGAGT_DROP_TEST`).
  The `$` prefix means it is a local (non-transportable) package — create it manually
  in SE80 or via `abapgit-agent tree --package '$'`.
- Check that the ABAP backend version supports all commands (`abapgit-agent health`).

### Pull / drop / customize tests fail after setup succeeded

Run setup standalone to verify the key object is active:

```bash
npm run test:setup
```

If it reports `already active` but tests still fail, the object may exist but be
in an error state. Use `abapgit-agent inspect` or SE80 to investigate.

### Lifecycle test: "create failed"

The lifecycle test creates and deletes an abapGit repo registration on each run.
If a previous run crashed mid-test the registration may already exist. The
`lifecycle-runner.js` detects this and calls `delete` automatically before
re-running `create`.

### Debug tests: breakpoint line numbers wrong

Debug tests use hardcoded ADT-verified line numbers. If the `abgagt-debug-test`
objects were modified after the test was written, Group A tests will fail.
Re-run setup (`npm run test:setup`) to ensure the latest object versions are active,
then check the failing test's expected line against `view --full --lines` output.
