# ADR 0041: Publish offline snapshot verification as evidence only

- Status: Accepted experimental policy
- Date: 2026-07-28
- Owners: package maintainers
- Related: ADR 0016, ADR 0017, ADR 0019

## Context

M9a already provides a hardened internal verifier for one closed snapshot tree.
It validates the bounded manifest, rebinds AOI identity through M6b, rejects
unsafe filesystem entries, and verifies declared resource sizes and SHA-256
values without interpreting resource contents. M9b uses that verifier before
and after publishing an internal catalog-only snapshot.

The broader public package, load, and replay APIs remain blocked on fetched
source-context retention, a canonical request-export loading contract,
Frictionless profile validation, overwrite ownership, and refresh semantics.
Those gaps do not prevent users from obtaining precise offline integrity
evidence for an already existing snapshot.

## Decision

Export `gx_snapshot_verify(dir)` as a narrow wrapper over the exact M9a
closed-tree verifier.

The function:

1. accepts one existing snapshot directory and reads only its fixed
   `manifest.json` plus declared resource bytes;
2. validates and normalizes manifest-v1, including inert recipe fields and the
   embedded request-ledger shape;
3. rehydrates the AOI recipe without executing catalog, fetch, harmonization,
   or replay stages;
4. inventories the closed tree before and after hashing and rejects aliases,
   links, special files, undeclared entries, and mutations;
5. verifies required resources and reports permitted optional absences;
6. returns an exact `gx_snapshot_verification` evidence object whose validator
   rebinds the normalized manifest, AOI, request count, expected resources, and
   observed resource status; and
7. performs no DNS, network, cache, optional-package, parser, decompression,
   repair, refresh, or write work.

Resources remain opaque. Request records are shape-validated rather than
authenticated or re-executed. The manifest SHA-256 identifies the exact
manifest bytes observed by the verifier.

## Acceptance criteria

- Identifier and custom-geometry recipes rebind exact AOI identity.
- Required resource size/hash mismatches and unsafe or undeclared filesystem
  entries fail closed.
- Optional absences remain explicit in both resource and overall status.
- The public result has an exact class and field contract; coherent manifest,
  request-count, status, and resource projections are revalidated.
- Evidence forgery fails with the snapshot error hierarchy.
- Snapshot bytes and filesystem state are unchanged, and blocked external-work
  seams are never invoked.

## Consequences

- Users can verify snapshot integrity offline without waiting for public
  loading or replay.
- A successful result proves internal consistency relative to an unsigned
  manifest at verification time. It does not prove authenticity, historical
  request truth, licence accuracy, currentness, or protection from coordinated
  replacement of both manifest and resources.
- `gx_package()`, `gx_snapshot()`, and `gx_replay()` remain gated. Verification
  does not parse resources into R objects or authorize refresh.
