# ADR 0020: Build an internal deterministic selection-only fetch plan

- Status: Accepted internal substrate
- Date: 2026-07-15

## Context

M6c supplies a strict offline catalog whose dataset cardinality is one row per
dataset, distribution, and variable. M7 ultimately needs to turn those rows
into provider requests, execute them through handler-specific implementations,
and normalize their payloads. Those later steps still depend on fixture-backed
provider semantics, optional-package preflight, execution accounting, and a
replay contract.

Performing package discovery or calling handler code during the first planning
slice would make otherwise identical plans depend on the local R library and
could accidentally cross the network or mutate caches. Treating classifier
metadata as executable would also overstate what the bundled handler assets
currently prove.

## Decision

Add an unexported M7a constructor and validator for contract 0.1.0
`gx_fetch_plan` objects. M7a is selection-only and non-executable. It accepts a
revalidated M6c catalog whose selected components include datasets, binds the
source dataset rows through a deterministic SHA-256, and records the requested
UTC interval and count/byte budgets.

Load the handler definition from two strictly validated bundled assets:
`registry.yml` contains portable first-match classifier facts, while
`implementations-r.json` contains R implementation identities and optional
package metadata. Both must be bounded regular non-symlink UTF-8/LF files.
Ambiguous YAML features are rejected, the two assets must agree one-to-one on
contract version and handler identity, and their exact byte hashes are bound
into the plan. This metadata does not grant permission to execute a handler.

Catalog rows with non-missing dataset, distribution, and URL identities are
grouped by `distribution_id`. All non-variable facts must agree within a group.
The plan emits one distribution row per unique admitted identity and one
parameter row per corresponding catalog variable row. Parameter keys and
provider-specific mappings remain explicitly unplanned.

Every admitted distribution URL is checked through the offline target-safety
policy without DNS resolution. Its catalog classifier must agree with the
first matching rule in the bound registry. Effective time is the intersection
of the requested interval and catalog temporal coverage. Selection decisions
distinguish selected-but-unplanned, reference-only, non-fetchable,
outside-time, and budget-skipped distributions.

Distribution selection uses bytewise order over provider-URI missingness and
value, provider-name missingness and value, site URI, and distribution ID. The
`max_datasets` cap applies only to otherwise eligible fetch candidates. Request
and byte budgets are recorded for later stages but are not consumed by M7a.

The request list is exactly empty. All fetch implementations are recorded as
`planned`, the unknown fallback is classifier-only, every handler is marked
non-replayable, and `execution_ready` is false. Construction and validation do
not probe installed packages, call handlers, resolve DNS, use the network,
touch the cache, or write files. Validation rebinds classifications and handler
metadata to the bundled asset hashes rather than trusting mutable object fields.

Public `gx_fetch_plan()` and `gx_fetch()` remain unexported. Provider request
construction, optional-package preflight, execution, runtime registration, and
serialized/replayable plan contracts are deferred to M7b/M7c.

## Consequences

- Catalog distributions now have deterministic, count-reconciled selection and
  parameter views independent of the packages installed on the host.
- A selected row states only that it survived M7a classification, time, and
  count limits; it does not represent a provider request or permission to
  fetch.
- Asset or classifier changes are visible through exact hashes and invalidate
  forged or stale handler bindings.
- Offline URL checks reject known-unsafe targets, but execution must still
  resolve and revalidate DNS and every redirect immediately before transport.
- No fetch-plan serialization schema or replay guarantee is introduced by this
  checkpoint.
