---
layout: default
title: Branch Workflow
nav_order: 20
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# Branch Workflow

#### Starting a New Feature

```bash
# 1. Create and switch to feature branch from default branch
git checkout main  # or master/develop (auto-detected)
git pull origin main
git checkout -b feature/user-authentication

# 2. Make your changes
edit src/zcl_auth_handler.clas.abap

# 3. Check syntax (CLAS/INTF/PROG/DDLS only, if independent)
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap
ls .abaplint.json 2>/dev/null && abapgit-agent lint   # abaplint (if configured)

# 4. Commit
git add src/zcl_auth_handler.clas.abap
git commit -m "feat: add authentication handler"

# 5. Push feature branch
git push origin feature/user-authentication

# 6. **CRITICAL**: Rebase before pull
git fetch origin main
git rebase origin/main
git push origin feature/user-authentication --force-with-lease

# 7. Pull to ABAP system
abapgit-agent pull --files src/zcl_auth_handler.clas.abap
```

#### During Development: Always Rebase Before Pull

**CRITICAL**: Before every `pull` command, rebase to default branch to avoid activating outdated code.

```bash
# Before EVERY pull, always do this:
git fetch origin main  # main/master/develop (auto-detected)
git rebase origin/main

# If no conflicts:
git push origin feature/user-authentication --force-with-lease
abapgit-agent pull --files src/zcl_auth_handler.clas.abap

# If conflicts:
# 1. Fix conflicts in files
# 2. git add <resolved-files>
# 3. git rebase --continue
# 4. git push origin feature/user-authentication --force-with-lease
# 5. abapgit-agent pull --files ...
```

#### Completing the Feature

```bash
# 1. Final rebase and push
git fetch origin main
git rebase origin/main
git push origin feature/user-authentication --force-with-lease

# 2. Final activation and test
abapgit-agent pull --files src/zcl_auth_handler.clas.abap
abapgit-agent unit --files src/zcl_auth_handler.clas.testclasses.abap

# 3. Create PR (squash merge enabled on GitHub/GitLab)
# See "Creating a PR" section below for transport handling
```

#### Creating a PR

When creating a PR with `gh pr create`:

**Check `.abapgit-agent.json` for `transports.hook`.**

```
transports.hook present?
├─ NO  → create PR normally, no transport line needed. STOP here.
└─ YES → continue below
```

**If hook is configured — resolve which transport to include:**

```bash
abapgit-agent transport list --scope task --json
```

- **One result** → use it directly, no need to ask the user
- **Multiple results** → show the list to the user and ask which one to use for this PR
- **No results** → ask the user to provide a transport number manually, or confirm to skip

**Append `transport: <number>` as the last line of the PR body:**

```
## Summary
- Implemented user authentication handler

## Changed Objects
- ZCL_AUTH_HANDLER (CLAS)

transport: DEVK900001
```

**Why:** The CI pipeline reads this line from the PR description and uses it for the
Activate stage, overriding the automatic hook selection. This lets the user control
which transport request receives the activated changes.

#### Why Rebase Before Pull?

ABAP is a **centralized system**. Multiple developers may modify the same files:

| Without Rebase | With Rebase |
|----------------|-------------|
| ✗ Your branch is based on old main | ✓ Your branch includes latest changes |
| ✗ Activate outdated code in ABAP | ✓ Activate current code |
| ✗ May overwrite others' work | ✓ Conflicts caught before activation |
| ✗ Hard to debug issues | ✓ Clear what changed |

**Example Scenario:**

```
Situation:
- You: working on feature/auth (based on main commit A)
- Colleague: pushed to main (now at commit B)
- Both modified: src/zcl_auth_handler.clas.abap

Without rebase:
  feature/auth pull → activates version from commit A ✗

With rebase:
  git rebase origin/main → either:
    - No conflict: includes colleague's changes ✓
    - Conflict: you see it and resolve ✓
```

#### Complete Example Workflow (Branch Mode)

```bash
# Day 1: Start feature
git checkout main && git pull origin main
git checkout -b feature/user-authentication
edit src/zcl_auth_handler.clas.abap
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap
ls .abaplint.json 2>/dev/null && abapgit-agent lint   # abaplint (if configured)
git add . && git commit -m "wip: add basic auth logic"
git push origin feature/user-authentication
git fetch origin main && git rebase origin/main
git push origin feature/user-authentication --force-with-lease
abapgit-agent pull --files src/zcl_auth_handler.clas.abap

# Day 2: Continue (colleague pushed to main overnight)
git fetch origin main && git rebase origin/main
# If conflicts: resolve, git add, git rebase --continue
git push origin feature/user-authentication --force-with-lease
edit src/zcl_auth_handler.clas.abap
abapgit-agent syntax --files src/zcl_auth_handler.clas.abap
ls .abaplint.json 2>/dev/null && abapgit-agent lint   # abaplint (if configured)
git add . && git commit -m "feat: complete auth logic"
git push origin feature/user-authentication
git fetch origin main && git rebase origin/main
git push origin feature/user-authentication --force-with-lease
abapgit-agent pull --files src/zcl_auth_handler.clas.abap

# Day 3: Finish feature
abapgit-agent unit --files src/zcl_auth_handler.clas.testclasses.abap
git fetch origin main && git rebase origin/main
git push origin feature/user-authentication --force-with-lease
# Create PR: check transport hook, resolve transport, include in PR body
# abapgit-agent transport list --scope task --json  → pick transport → append to PR body
```
