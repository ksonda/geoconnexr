# ADR 0062: Admit a reviewed Quarto CLI without rendering

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0058, ADR 0061
- Supersedes: the CLI-admission deferral in ADR 0061

## Context

M9w verifies the optional Quarto R package and report-facing symbols, but it
does not establish that the package resolves a real, executable, or compatible
Quarto CLI. Report rendering must bind the exact host executable before any
report staging begins.

The official Quarto download index identifies 1.8.27 as the final 1.8 patch and
continues to publish newer stable minor lines. Using 1.8.27 as the minimum keeps
the report contract on the corrected 1.8 runtime line without requiring the
latest minor release. The repository host currently resolves Quarto CLI
1.3.433, which is intentionally outside this admitted profile.

## Decision

Add the internal M9x `gx_package_quarto_cli_impl()` boundary.

1. Resolve and validate the complete M9w Quarto R package capability first.
2. Invoke only the reviewed `quarto_path(normalize = TRUE)` surface and require
   one absolute normalized path to an existing nonempty executable file.
3. Execute that exact path only with `--version`, capture combined output under
   a five-second timeout, and require one strict normalized version value.
4. Record filesystem identity before the command and require size, mode,
   modification time, and change time to remain identical afterward.
5. Require Quarto CLI `>= 1.8.27`; reject missing paths, command failures,
   malformed or multiline output, older versions, and executable races under
   typed CLI errors.
6. Return exact host-specific evidence binding the Quarto R package version,
   normalized CLI path, CLI version, file metadata, command, timeout, limits,
   and deterministic identity.
7. Mark CLI version admission ready while keeping report rendering, public API,
   replay, and distribution authenticity false or deferred.
8. Do not render, inspect a document, execute user code, write package
   resources, access the network, or mutate caches.

## Acceptance criteria

- A reviewed package capability plus one unchanged executable reporting CLI
  1.8.27 or newer produces exact validated host evidence.
- Missing package capability and invalid or non-executable paths fail before
  CLI execution.
- Old CLI versions, command failures, malformed output, and file mutation fail
  closed.
- The exact normalized path and five-second timeout reach the injected command
  boundary; no alternate PATH lookup occurs after admission.
- Evidence forgery and later executable replacement fail validation.
- No render function, network, cache, report staging, publication, replay, or
  public API is invoked.

## Consequences

- A later renderer can rely on separately reviewed R-package and CLI gates.
- The current development host is correctly blocked until its old CLI and
  missing optional Quarto R package are upgraded.
- CLI admission is host-specific and unsigned; it does not authenticate the
  Quarto distribution or make the evidence portable.
- The next M9 checkpoint can define a fixed non-executing HTML report source,
  render controls, output inventory, and byte/structure verification.
