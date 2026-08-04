# ADR 0058: Inspect optional package metadata without authorizing execution

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0021, ADR 0051, ADR 0057

## Context

The M9 roadmap names Parquet output through Arrow and report rendering through
Quarto, but neither dependency has a reviewed minimum version or symbol
contract in the current package profile. Treating an installed package as
ready would make results depend silently on local state. Loading optional
namespaces merely to discover availability would also execute package startup
code before geoconnexr has selected an operation.

The M7b package preflight already establishes a safer pattern: inspect bounded
installed `DESCRIPTION` bytes directly, bind the observed version as
host-specific advisory evidence, and withhold execution until a separate
capability boundary resolves reviewed symbols.

## Decision

Add the internal M9t `gx_package_options_impl()` preflight for the fixed feature
rows `parquet`/`arrow` and `report`/`quarto`.

1. Probe both package versions through bounded, race-aware direct
   `DESCRIPTION` reads. Generalize the M7b version probe with an explicit
   internal allowlist argument; its fetch-handler default allowlist is
   unchanged.
2. Do not load either namespace or resolve any symbol.
3. Record missing packages as `missing` and `skipped_missing_pkg`.
4. Record installed packages as `present_requirement_unpinned` and
   `blocked_version_unpinned`. Installation alone never implies readiness.
5. Return an exact host-specific `gx_package_options` value with the inspection
   time, reconciled counts, explicit non-readiness limitations, and an identity
   over the observed capability rows.
6. Keep Parquet serialization and report rendering false and unauthorized.
   Public `gx_package(timeseries = "parquet")` remains rejected by the existing
   scope gate.

The preflight is advisory and non-replayable: installed package state can
change after inspection. A later execution boundary must recheck a reviewed
minimum version and resolve every required symbol immediately before use.

## Acceptance criteria

- Missing Arrow or Quarto produces `skipped_missing_pkg`, not a namespace-load
  or package-load failure.
- Installed but unpinned packages remain blocked and never become ready merely
  because they are present.
- Both fixed feature rows, observed versions, statuses, counts, limitations,
  timestamp, and identity fail closed under forgery.
- Malformed, warning-producing, or failed metadata probes fail closed.
- The preflight performs no network, DNS, cache, namespace-load,
  serialization, rendering, filesystem write, or public API work.
- Existing M7b package-preflight behavior remains unchanged.

## Consequences

- Optional-package absence now has one explicit M9 behavior before any Arrow or
  Quarto implementation is admitted.
- The next Parquet checkpoint must review and pin an Arrow version and required
  symbols, then define deterministic serialization and loading semantics.
- Quarto symbol resolution remains coupled to the later report boundary rather
  than package creation.

## Follow-up

ADR 0059 pins the Arrow serializer, and
[ADR 0061](0061-reviewed-quarto-runtime-capability.md) supersedes the
Quarto-unpinned portion of this decision while preserving this preflight as an
advisory, namespace-free boundary.
