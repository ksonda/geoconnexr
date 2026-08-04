# ADR 0061: Pin the Quarto R report capability

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0058, ADR 0059, ADR 0060
- Supersedes: the Quarto-unpinned portion of ADR 0058

## Context

M9t can safely observe installed Quarto package metadata, but it leaves the
report feature blocked because the minimum version, required exports, and
function signatures were not reviewed. Report rendering must not begin by
loading an arbitrary installed namespace or discovering changed symbols after
filesystem staging has started.

The official Quarto R package 1.5.1 exposes the report-facing surfaces needed
by the later renderer: `quarto_render()`, `quarto_path()`, `quarto_version()`,
and `quarto_available()`. The rendering API admits explicit input, HTML output,
output filename, execution disablement, metadata, quiet mode, raw Quarto/Pandoc
arguments, and background-job control. Package capability and CLI capability
remain distinct: resolving these functions does not prove that an acceptable
Quarto CLI is installed.

## Decision

Add the internal M9w `gx_package_quarto_capability_impl()` boundary.

1. Pin the Quarto R package at `>= 1.5.1` in `Suggests` and in M9t's bounded
   installed-metadata preflight.
2. Reject a missing or older package before loading its namespace.
3. Load the namespace only after metadata admission, immediately require its
   version to equal the inspected version, and resolve only the four reviewed
   exports.
4. Require the reviewed report formals on `quarto_render()`, `normalize` on
   `quarto_path()`, no arguments on `quarto_version()`, and `min`, `max`, and
   `error` on `quarto_available()`.
5. Return the resolved functions and exact package version only after complete
   validation. Do not invoke any resolved function, locate or execute the CLI,
   render a document, create files, or add a public API.
6. Keep M9t advisory and namespace-free. A version-satisfied Quarto row remains
   `blocked_symbols_unchecked`; M9w is the separate operation-time symbol gate.
7. Defer the Quarto CLI minimum/version check, fixed report source, sandboxed
   rendering, output verification, package integration, and public report API
   to later milestones.

## Acceptance criteria

- Missing and too-old Quarto packages fail before namespace loading.
- Metadata/namespace version races and missing or changed exports fail closed.
- The exact reviewed exports and formals resolve at version 1.5.1 or newer.
- Capability resolution invokes none of the resolved functions and performs no
  CLI, network, cache, rendering, filesystem publication, or public API work.
- Malformed resolvers and warnings fail closed under typed Quarto errors.
- M9t reports both Arrow and Quarto reviewed minimums while remaining advisory,
  namespace-free, and never execution-ready.

## Consequences

- Report implementation now has a reviewed R-package dependency boundary.
- A suitable R package alone does not authorize rendering; the CLI and fixed
  report contract still need independent admission.
- Hosts without the optional Quarto R package continue to load and use all
  non-report package features.
- The next M9 checkpoint can pin and inspect the CLI before attempting a fixed,
  non-executing HTML report render.

## Follow-up

[ADR 0062](0062-reviewed-quarto-cli-admission.md) supersedes the CLI-admission
deferral while preserving rendering and public report exposure as later work.
