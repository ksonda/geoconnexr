# ADR 0088: Add the central bounded concurrency scheduler

- Status: Accepted for the experimental M1 protocol slice
- Date: 2026-08-15
- Owner: ksonda

## Context

ADR 0011 requires one single-process scheduler before concurrent transport can
be enabled. The current fetch loops reconcile request and byte budgets after a
request finishes. Dispatching those loops independently could exceed the
declared ceilings, duplicate an identical cache request, or return results in
completion order instead of source order.

The scheduler must be testable without the network. Curl multi integration and
public fetch orchestration are separate changes because they also affect
transport, retry, cache, and handler contracts.

## Decision

Add one internal workflow-owned scheduler with these rules:

- Jobs have a fixed input order, unique logical identifier, normalized host,
  64-character cache key, and physical-attempt byte ceiling.
- Total and per-host active permits are reserved centrally. A host blocked by
  its own limit does not prevent an eligible job for another host from starting.
- One request and the full attempt byte ceiling are reserved before a job
  becomes active. Completion charges the observed bytes and releases the unused
  reservation. A job that cannot fit after active work finishes is explicitly
  deferred by its request or byte budget.
- Concurrent jobs with the same cache key share one physical attempt only when
  their normalized host and byte ceiling are identical. Followers consume no
  permit, request, or byte reservation and receive the leader's exact result.
- Completion may occur in any order, but collection exposes only the contiguous
  terminal prefix in original job order.
- Invalid completion amounts, stale tokens, incompatible duplicate keys, and
  corrupt accounting fail before scheduler state changes.

The scheduler remains internal and performs no DNS lookup, cache operation,
retry, sleep, or transport. A later slice will bind its reservations to curl
multi handles and the existing physical-attempt ledger.

## Consequences

- Concurrent transport can no longer rely on completion-time budget checks.
- Compatible identical requests have a defined single-flight owner before any
  network handle is created.
- Deterministic collection does not depend on network completion order.
- M1 remains partial until curl multi and public fetch orchestration use this
  scheduler.

## Acceptance evidence

The deterministic scheduler suite covers total and per-host permits,
cross-host progress, out-of-order completion, single-flight sharing, request
exhaustion, byte reservation release, permanent byte deferral, invalid
completion rollback, and corrupt-state rejection without sleeping or using the
network.
