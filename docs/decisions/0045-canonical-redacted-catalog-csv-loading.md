# ADR 0045: Load redacted catalog CSVs only as canonical character tables

- Status: Accepted internal substrate
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0019, ADR 0042, ADR 0043, ADR 0044

## Context

M9f publishes verified typed request-ledger access, but the three catalog CSV
resources remain opaque. Unlike the request ledger, their complete typed values
are not duplicated in the manifest. They must be parsed before any broader
catalog-view loader can exist.

The CSV projection is deliberately lossy. Missing values become blank fields,
timestamps and logicals become text, list columns become JSON text, geometry
becomes WKT, and sensitive URI values are redacted. A loader must not infer
which blank fields were missing, restore discarded secrets, or present the
result as the original live `gx_catalog`.

## Decision

Add the internal M9g canonical CSV parser and
`gx_snapshot_load_catalog_csv_impl(dir, resource)` boundary for `sites`,
`datasets`, and `problems`.

The boundary:

1. recognizes only the exact M9b writer, quote-all CSV, non-replayable, path,
   media-type, required-resource, and role profiles;
2. reads at most 64 MiB per CSV, with fixed column, field, and resource-specific
   row ceilings;
3. accepts only unmarked UTF-8 with LF endings and exact fixed column names;
4. parses every cell as character data and requires the parsed table to
   reserialize byte-for-byte to the original quote-all CSV;
5. verifies the resource hash and complete snapshot before and after loading;
6. returns exact `gx_snapshot_catalog_csv` evidence binding the resource name,
   normalized path, character table, dimensions, manifest and resource hashes,
   and deterministic table identity; and
7. remains internal until typed redacted-view semantics across all three
   resources are defined.

It does not interpret blank cells, WKT, JSON arrays, timestamps, logicals,
identities, or provenance claims. It performs no network, DNS, cache, write,
repair, refresh, live-catalog reconstruction, or replay.

## Acceptance criteria

- Empty and populated sites, datasets, and problems resources load to exact
  canonical character tables.
- Unquoted headers, CRLF, BOMs, extra blank lines, reordered headers, and other
  noncanonical representations fail.
- A validly rehashed but noncanonical CSV fails after closed-tree verification.
- Unknown resource names, profile changes, schema changes, evidence forgery,
  mutation, and loading-budget violations fail closed.
- The loader invokes no network or write seam and leaves snapshot bytes
  unchanged.
- No public catalog loader or `gx_catalog` reconstruction claim is introduced.

## Consequences

- Every fixed M9b CSV now has a canonical offline loading substrate.
- Later work can add explicitly typed redacted catalog views over proven
  character tables without trusting inferred CSV schemas.
- Blank-versus-missing distinctions and discarded URI identities remain
  intentionally unrecoverable.
- Public catalog-resource access, Frictionless loading, and replay remain later
  contracts.
