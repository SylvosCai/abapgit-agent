---
layout: default
title: Remote Cache & Stale Data Risk
nav_order: 3
parent: pull - Pull & Activate
grand_parent: Development Commands
---

# pull Command - Remote Cache & Stale Data Risk

## Background

abapGit caches the list of remote git files (`mt_remote`) as an instance variable on the
online repo object (`zcl_abapgit_repo_online`). The cache is populated on the first
`get_files_remote()` call and reused for all subsequent calls within the same ABAP work
process (WP).

The cache is cleared (reset) by calling `reset_remote()`, which sets the internal flag
`mv_request_remote_refresh = abap_true`, forcing a fresh GitHub download on the next
`get_files_remote()` call.

---

## The Stale Cache Risk

SAP ABAP work processes are **reused across HTTP requests**. The `zcl_abapgit_repo_srv`
singleton and its repo instances live in WP static memory and survive between requests.

If two consecutive `pull` calls hit the **same WP**, the `mt_remote` cache from the first
pull is still warm — and the second pull may read **stale git data** instead of fetching
fresh content from the remote repository.

---

## When Is the Cache Reset?

`reset_remote()` is called in the following situations:

| Trigger | Condition |
|---------|-----------|
| `select_branch()` | Called whenever `iv_branch` is non-empty in `pull` |
| `deserialize_checks` override | Only present when the SylvosCai/abapGit fork (PR #7) is installed |

### Risk matrix

| abapGit version | `iv_branch` set? | Cache reliably reset? |
|---|---|---|
| Fork (PRs #5/#6/#7) | Yes | ✅ `select_branch()` resets it |
| Fork (PRs #5/#6/#7) | No | ✅ `deserialize_checks` override resets it before delegating to `super` |
| Original abapGit (`main`) | Yes | ✅ `select_branch()` resets it |
| Original abapGit (`main`) | No | ⚠️ **No reset — stale cache possible** |

The dangerous case is original abapGit + `iv_branch` empty (e.g. `pull --files` without
specifying a branch). In this case neither `select_branch()` nor the fork's
`deserialize_checks` override fires, so a warm cache from a previous request in the same WP
is silently reused.

---

## History

Prior to commit `12f756d`, the agent called:

```abap
mo_repo->refresh( iv_drop_cache = abap_true ).
```

at the start of every pull. This unconditionally called `reset_remote()` and guaranteed a
fresh download — but it also caused a **double full-pack download** on the `main` branch
(once for `refresh`, once for `deserialize_checks`), which was the root cause of the 60-second
pull time on large repositories.

The call was removed in `12f756d` as part of the performance fix (60s → 12s). The
fork (PRs #5/#6/#7) is not affected because its `deserialize_checks`
override calls `reset_remote()` explicitly before delegating to `super->deserialize_checks`.

---

## Recommended Fix

Guard the `refresh()` call so it only runs when the fork's `deserialize_checks` override
(with its built-in `reset_remote()`) is **not** present, and no branch switch was requested.

The agent already detects whether the fork is installed via `filter_param_available()` — which
checks at runtime whether `II_OBJ_FILTER` exists on `ZIF_ABAPGIT_REPO~DESERIALIZE`:

```abap
" filter_param_available() = true  → fork (PRs #5/#6/#7) installed → deserialize_checks resets cache
" filter_param_available() = false → original abapGit → no automatic reset; refresh to avoid stale WP cache
IF filter_param_available( ) = abap_false AND iv_branch IS INITIAL.
  mo_repo->refresh( iv_drop_cache = abap_true ).
ENDIF.
```

This preserves the performance gain on the fork (no double download) while restoring
correctness on original abapGit.

---

## Impact

| Scenario | Without fix | With fix |
|---|---|---|
| Fork (PRs #5/#6/#7), any pull | ✅ correct, fast | ✅ correct, fast — no change |
| Original abapGit, branch specified | ✅ correct | ✅ correct — no change |
| Original abapGit, no branch, same WP reused | ⚠️ may use stale data | ✅ correct |
| Original abapGit, no branch, fresh WP | ✅ correct (cold cache) | ✅ correct |
