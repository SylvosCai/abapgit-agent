---
layout: default
title: Partial Download (abapGit fork)
nav_order: 5
parent: pull - Pull & Activate
---

# Partial Download — abapGit Fork

## Overview

When `--files` is used, abapgit-agent sends the requested file list to the ABAP backend. How abapGit processes that list on the server side depends on which version of abapGit is installed.

Two behaviours are possible:

| abapGit version | Behaviour | Status |
|---|---|---|
| **Original abapGit** (`larshp/abapGit`) | Full repository fetched from Git; only the requested objects are deserialised | Works, not fully tested |
| **SylvosCai/abapGit fork** | Partial blob fetch — only the blobs for the requested files are downloaded from the Git server | Tested, significantly faster on large repos |

---

## SylvosCai/abapGit Fork — Partial Blob Fetch

The fork ([SylvosCai/abapGit PR #1](https://github.com/SylvosCai/abapGit/pull/1)) adds two connected optimisations so that a filtered pull avoids processing the full repository at every layer of the stack.

### How It Works

**Change 1 — Filtered deserialise**

The object filter (`ii_obj_filter`) is threaded through the entire deserialise call chain. Only the requested objects are status-checked and deserialised; the rest are skipped.

```
zif_abapgit_repo~deserialize( ii_obj_filter )
  └─ zcl_abapgit_objects=>deserialize( ii_obj_filter )
       ├─ get_files_remote( ii_obj_filter )
       └─ zcl_abapgit_file_deserialize=>get_results( ii_obj_filter )
            └─ zcl_abapgit_repo_status=>calculate( ii_obj_filter )
```

**Change 2 — Partial blob fetch (two-phase Git fetch)**

When the Git server supports the `filter` capability (Git ≥ 2.17 — GitHub, GitLab, Gitea all qualify), abapGit uses a two-phase fetch:

- **Phase 1** — `filter=blob:none`: server returns commits and trees only; no blob data is transferred
- **Phase 2** — targeted blob fetch: only the blob SHAs for the requested files are fetched

Servers without `filter` support fall back transparently to a full fetch.

### Performance Impact

| Scenario | Original abapGit | Fork |
|---|---|---|
| `--files` pull, large repo, capable server | Full fetch (~50 MB) | Phase 1 tree + Phase 2 ~5 blobs (~100 KB) |
| `--files` pull, filter matches zero objects | Full blob fetch | Zero blobs fetched |
| Full pull (no `--files`) | Unchanged | Unchanged |

### Bugs Fixed in the Fork

The PR also fixes three bugs that prevented the optimisation from working correctly:

1. **Phase 2 fetched all blobs** — `walk_for_blobs` collected stubs for the entire tree before filtering. Phase 2 then requested all ~19,500 blob SHAs instead of the handful needed. Fixed by passing wanted filenames into `pull()` and calling `filter_stubs` before Phase 2.

2. **`blob:none` was never sent** — The first call to `fetch_remote` came from `find_remote_dot_abapgit` inside `deserialize_checks` with no filter. By the time `deserialize()` called `get_files_remote( ii_obj_filter )`, the cache was already warm, so the filter was silently ignored. Fixed by overriding `deserialize_checks` in `zcl_abapgit_repo_online` to store the pending filter and reset the remote cache before delegating to `super`.

3. **Empty filter fetched all blobs** — If the object filter matched zero objects, `lt_wanted_files` was empty and a guard skipped `filter_stubs`, causing Phase 2 to fetch all blobs. Fixed by removing the guard so `filter_stubs` is always called.

### ABAP Files Changed

| File | Change |
|---|---|
| `zif_abapgit_repo.intf.abap` | `deserialize` gains `ii_obj_filter OPTIONAL` |
| `zcl_abapgit_repo.clas.abap` | Passes filter through call chain |
| `zcl_abapgit_repo_online.clas.abap` | Overrides `deserialize_checks`; manages `mi_pending_obj_filter` |
| `zcl_abapgit_objects.clas.abap` | Accepts and forwards filter |
| `zcl_abapgit_file_deserialize.clas.abap` | Applies filter during status calculation |
| `zcl_abapgit_objects_check.clas.abap` | Applies filter during check phase |
| `zcl_abapgit_git_porcelain.clas.abap` | `pull()` accepts wanted files; calls `filter_stubs` |
| `zcl_abapgit_git_transport.clas.abap` | Sends `filter=blob:none` when capability present |
| `zif_abapgit_git_branch_list.intf.abap` | Adds `get_capabilities` method |
| `zcl_abapgit_git_branch_list.clas.abap` | Implements `get_capabilities` |

### Backward Compatibility

- All new parameters are `OPTIONAL` — existing callers are unaffected
- Custom implementations that override `zif_abapgit_repo~deserialize` must add the `ii_obj_filter OPTIONAL` parameter
- Custom implementations of `zif_abapgit_git_branch_list` must add the `get_capabilities` method (returning initial is sufficient)

---

## Original abapGit — Existing Behaviour

The current abapgit-agent ABAP backend (`ZCL_ABGAGT_COMMAND_PULL`) is compatible with the original abapGit. When `--files` is used:

1. The file list is converted to `(obj_type, obj_name)` pairs
2. An `ii_obj_filter` is constructed and passed to `zif_abapgit_repo~deserialize`
3. Original abapGit **ignores** the `ii_obj_filter` parameter (the method signature predates the fork) — it performs a full repository fetch and full deserialisation

This means `--files` with original abapGit still activates only the requested objects correctly, but offers no network or CPU savings on large repositories compared to a full pull.

> **Not fully tested** — the `--files` path against stock abapGit has not been exercised across all object types and edge cases. Use with care on production systems and verify pull output carefully.

---

## How to Tell Which Version Is Installed

Check the capabilities returned during a Git fetch. The fork advertises `filter` in the ref-advertisement:

```bash
# If the fork is installed, a filtered pull log will show Phase 1 + Phase 2 steps.
# With original abapGit, a full fetch always occurs regardless of --files.
abapgit-agent pull --files src/zcl_my_class.clas.abap
```

Alternatively, check the abapGit version in your SAP system via transaction `/n/ABAPGit/ZABAPGIT` or the abapGit about screen.
