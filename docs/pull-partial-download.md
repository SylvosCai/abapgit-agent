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
| **Original abapGit** ([abapGit/abapGit](https://github.com/abapGit/abapGit)) | Full repository fetched from Git; only the requested objects are deserialised | Works, not fully tested |
| **SylvosCai/abapGit fork** | Partial blob fetch — only the blobs for the requested files are downloaded from the Git server | In review (PRs #5, #6, #7) |

---

## SylvosCai/abapGit Fork — Partial Blob Fetch

The fork adds partial blob fetch across three PRs, each targeting a different layer of the abapGit stack:

| PR | Layer | Title | Status |
|---|---|---|---|
| [#5](https://github.com/SylvosCai/abapGit/pull/5) | git / gitv2 | feat(git): partial blob fetch via gitv2 (phase 1 of 3) | Open |
| [#6](https://github.com/SylvosCai/abapGit/pull/6) | objects / deserialise | feat(objects): thread ii_obj_filter through deserialization (phase 2 of 3) | Open |
| [#7](https://github.com/SylvosCai/abapGit/pull/7) | repo / online | feat: partial blob fetch in repo/online layer (phase 3/3) | Open |

All changes are **additive and backwards-compatible**. Existing callers and the full-pull path are unaffected.

---

### How It Works

#### Phase 1 (PR #5) — git / gitv2 layer

**`get_capabilities()`** is added to `zcl_abapgit_git_branch_list` to expose the server's raw capability string from the git protocol handshake — used downstream to detect whether the server supports `filter`.

**`list_trees_for_paths( iv_url, iv_sha1, it_wanted_paths )`** is added to `zcl_abapgit_gitv2_porcelain`:
- Sends one gitv2 `fetch` with `filter tree:<N>` (where `N` = depth of the deepest wanted path)
- The server returns only the commit and the tree objects needed to reach the wanted directories — no blob data
- The locally-cached tree objects are then walked to produce a flat file listing

**`filter_expanded`** is added as a public class method on `zcl_abapgit_git_porcelain` so the repo layer (phase 3) can reuse it when assembling files from a gitv2 partial fetch.

#### Phase 2 (PR #6) — objects / deserialise layer

Threads `ii_obj_filter TYPE REF TO zif_abapgit_object_filter OPTIONAL` through the entire deserialise call chain so only matching objects are status-checked and deserialised:

```
zcl_abapgit_objects=>deserialize( ii_obj_filter )
  ├─ get_files_remote( ii_obj_filter )
  └─ zcl_abapgit_file_deserialize=>get_results( ii_obj_filter )
       └─ zcl_abapgit_repo_status=>calculate( ii_obj_filter )

zcl_abapgit_objects=>deserialize_checks( ii_obj_filter )
  └─ zcl_abapgit_objects_check=>deserialize_checks( ii_obj_filter )
       └─ zcl_abapgit_repo_status=>calculate( ii_obj_filter )
```

This is pure parameter pass-through — no branching logic added.

#### Phase 3 (PR #7) — repo / online layer

**`zif_abapgit_object_filter`** gains `get_paths()` returning directory path hints (`string_table`). These let the git layer prune the tree traversal to only the directories that contain the requested objects.

**`zcl_abapgit_object_filter_obj`** implements `get_paths()` and gains an optional `it_paths` constructor parameter so callers can supply path hints at construction time (e.g. `'/src/my_package/'`).

**`zif_abapgit_repo~deserialize` and `deserialize_checks`** gain `OPTIONAL ii_obj_filter`. All existing callers are unaffected.

**`zcl_abapgit_repo_online`** — the main addition — overrides `fetch_remote`:

```
fetch_remote( ii_obj_filter ):
  if filter IS BOUND and server has 'filter' capability:
    Phase 1: list_trees_for_paths( get_paths() ) + filter_expanded → wanted file list
    Phase 2: fetch_blobs( wanted SHA1s )          ← only the needed blobs
  else:
    pull_by_branch( it_wanted_files )              ← full pack, client-side filtered
```

**`find_remote_dot_abapgit`** is overridden to fetch `.abapgit.xml` directly via gitv2 (`list_no_blobs` + `fetch_blob`) without downloading the full pack.

**`deserialize_checks`** is overridden to reset the remote cache before delegating to `super->deserialize_checks( ii_obj_filter )` — ensures the filter is applied on the first remote fetch rather than hitting a warm unfiltered cache.

---

### Performance Impact

| Scenario | Original abapGit | Fork |
|---|---|---|
| `--files` pull, large repo, server with `filter` support | Full pack download | Phase 1: trees only (~few KB) + Phase 2: requested blobs only (~few KB each) |
| `--files` pull, server without `filter` support | Full pack download | Full pack download, client-side filtered |
| Full pull (no `--files`) | Unchanged | Unchanged |

GitHub, GitHub Enterprise, GitLab, and Gitea all support the git `filter` capability.

---

### Files Changed

| File | PR | Change |
|---|---|---|
| `zif_abapgit_git_branch_list.intf.abap` | #5 | Adds `get_capabilities` method |
| `zcl_abapgit_git_branch_list.clas.abap` | #5 | Implements `get_capabilities` |
| `zif_abapgit_gitv2_porcelain.intf.abap` | #5 | Adds `list_trees_for_paths` |
| `zcl_abapgit_gitv2_porcelain.clas.abap` | #5 | Implements `list_trees_for_paths`, `compute_max_depth`, `fetch_trees_at_depth`, `walk_tree_from_objects` |
| `zcl_abapgit_git_porcelain.clas.abap` | #5 | `pull()` gains `it_wanted_files`; adds public `filter_expanded` class method |
| `zcl_abapgit_objects.clas.abap` | #6 | `deserialize` + `deserialize_checks` gain `ii_obj_filter OPTIONAL` |
| `zcl_abapgit_objects_check.clas.abap` | #6 | `deserialize_checks` gains `ii_obj_filter OPTIONAL` |
| `zcl_abapgit_file_deserialize.clas.abap` | #6 | `get_results` gains `ii_obj_filter OPTIONAL` |
| `zif_abapgit_object_filter.intf.abap` | #7 | Adds `get_paths` method |
| `zcl_abapgit_object_filter_obj.clas.abap` | #7 | Implements `get_paths`; gains `it_paths OPTIONAL` constructor param |
| `zcl_abapgit_object_filter_tran.clas.abap` | #7 | Implements `get_paths` returning empty |
| `zif_abapgit_repo.intf.abap` | #7 | `deserialize` + `deserialize_checks` gain `ii_obj_filter OPTIONAL` |
| `zcl_abapgit_repo.clas.abap` | #7 | Threads `ii_obj_filter` through `deserialize_objects` + `deserialize_checks` |
| `zcl_abapgit_repo_online.clas.abap` | #7 | Overrides `fetch_remote`, `find_remote_dot_abapgit`, `deserialize_checks` |

### Backward Compatibility

- All new parameters are `OPTIONAL` — existing callers are completely unaffected
- Custom implementations of `zif_abapgit_object_filter` must add `get_paths()` (returning initial is sufficient)
- Custom implementations of `zif_abapgit_git_branch_list` must add `get_capabilities()` (returning initial is sufficient)
- Custom implementations that override `zif_abapgit_repo~deserialize` or `deserialize_checks` must add the `ii_obj_filter OPTIONAL` parameter

---

## Original abapGit ([abapGit/abapGit](https://github.com/abapGit/abapGit)) — Existing Behaviour

The current abapgit-agent ABAP backend (`ZCL_ABGAGT_COMMAND_PULL`) is compatible with the original abapGit. When `--files` is used:

1. The file list is converted to `(obj_type, obj_name)` pairs
2. An `ii_obj_filter` is constructed and passed to `zif_abapgit_repo~deserialize`
3. Original abapGit **ignores** the `ii_obj_filter` parameter (the method signature predates the fork) — it performs a full repository fetch and full deserialisation

This means `--files` with original abapGit still activates only the requested objects correctly, but offers no network or CPU savings on large repositories compared to a full pull.

> **Not fully tested** — the `--files` path against stock abapGit has not been exercised across all object types and edge cases. Use with care on production systems and verify pull output carefully.

---

## How to Tell Which Version Is Installed

Check the abapGit version in your SAP system via transaction `/n/ABAPGit/ZABAPGIT` or the abapGit about screen.

Once the fork PRs are merged and installed, a filtered pull will perform a two-phase fetch (trees + targeted blobs) rather than downloading the full pack:

```bash
# With the fork installed and a filter-capable Git server:
abapgit-agent pull --files src/zcl_my_class.clas.abap
# → Pull log will show only 1 deserialised object, with no full pack download
```
