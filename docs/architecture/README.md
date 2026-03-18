---
layout: default
title: Architecture
nav_order: 3
parent: Reference
has_children: true
---

# Architecture

Internal architecture and design documents for developers and maintainers.

## System Architecture

The diagram below shows the full project architecture — from the CLI layer through to the ABAP backend and SAP system.

![Architecture Diagram](architecture.svg)

## Design Documents

| Document | Description |
|----------|-------------|
| [Background Job Architecture](background-job-architecture) | Asynchronous command execution with real-time progress reporting via polling — used by the `import` command |
