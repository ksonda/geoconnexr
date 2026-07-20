# ADR 0021: Keep package capability preflight host-specific and non-executable

- Status: Accepted internal substrate
- Date: 2026-07-16

## Context

M7a deliberately produces the same selection-only plan from the same catalog
and bundled handler assets on every host. It records optional R package
requirements but does not inspect the local library. Missing and incompatible
Suggests packages must eventually become explicit fetch outcomes rather than
package-load failures.

Installed package state is machine-local and can change after inspection.
Loading a third-party namespace merely to inspect it can execute `.onLoad`
code, while package presence or a satisfying version does not prove that the
planned handler functions exist or that their provider semantics are correct.
The repository still lacks executable fixture-backed parameter mappings and
request contracts for EDR, USGS Water Data, WQP, and OGC API Features. Direct
CSV likewise lacks transport, response, ledger, budget-allocation, and parser
contracts, although ADR 0022 now implements a narrower inert GET-intent
substrate.

## Decision

Add an unexported M7b `gx_fetch_preflight` contract 0.1.0 as a separate,
ephemeral value object. It embeds and revalidates the exact M7a plan rather than
changing the deterministic plan contract. Only unique optional packages needed
by selected distributions are inspected, in bytewise package-name order.

The default probe scans at most 256 entries from `.libPaths()` in their existing
precedence order. For the first candidate with a `DESCRIPTION`, it reads that
file directly as at most 256 KiB of raw DCF data, rejects a terminal symlink or
special file, and compares filesystem identity before and after the read. It
requires exactly one `Package` and `Version` field and binds the package field
to the allowlisted requirement. It deliberately does not deserialize
`Meta/package.rds`; already-loaded development namespaces outside `.libPaths()`
are ignored.

The default probe does not use `requireNamespace()`, `loadNamespace()`,
`library()`, exported symbols, or handler functions. It does not retain library
paths, package descriptions, warnings, or underlying probe errors. R permits
non-UTF-8 `DESCRIPTION` metadata, so non-version fields are handled as bounded
raw DCF bytes while package identity and version remain exact ASCII values.
Malformed DCF, duplicate or mismatched fields, control bytes, warnings, errors,
and invalid or oversized versions fail closed. An injected version resolver is
a trusted internal test seam; its own side effects are outside the default
probe's safety guarantee.

Handler rows distinguish package state as `not_checked`, `not_required`,
`missing`, `version_too_old`, `version_satisfied`, or
`present_requirement_unpinned`. Missing or old requirements become
`skipped_missing_pkg` or `skipped_package_version`. A native handler, an
unpinned present package, or a satisfying minimum version still receives
`blocked_implementation_planned`; no status says that a handler is ready or
available. Reference-only and unselected rows remain explicit.

The report is `host_specific = TRUE`, `replayable = FALSE`, and
`execution_ready = FALSE`. It preserves the M7a plan byte-for-byte, including
its exact empty request list and unconsumed budgets. With the default resolver,
construction and validation call no handler or package symbol, initiate no DNS
or provider transport, touch no cache, and write no file. The bounded host
library reads may traverse a filesystem mounted from another machine. A future
executor must recheck the loaded package, version, and required symbols
immediately before invocation; this report is never replay authority.

Provider request construction remains deferred. It requires reviewed mapping
and request assets, an arbitrary-provider transport boundary, request/attempt
ledger alignment, credential-safe redirect behavior, and encoded/decoded and
parser budget semantics. ADR 0022 separately implements an M7c direct-CSV GET
intent substrate that records exact inert policy facts but no budget allocation
or transport authority. That description is not a provider request and does
not close any execution gate. ADR 0023 adds a host-independent M7d reservation
and direct-CSV logical-request-plan substrate without consulting this advisory
report. M7d still requires a fresh runtime package and symbol preflight before
any future executor may invoke provider code.

## Consequences

- Missing and too-old optional packages are visible as bounded data without
  loading third-party code.
- Identical M7a plans can yield different M7b reports on different hosts; that
  difference is explicit and intentionally non-replayable.
- Package presence never upgrades a planned implementation to executable
  status and never authorizes provider transport.
- Symbol availability, function signatures, provider requests, redirects,
  transport, response parsing, and execution accounting remain later M7 work;
  M7d binds only the direct-CSV inert policy and global reservation facts
  described by ADR 0023.
- M7b introduces no public API or serialized schema.
