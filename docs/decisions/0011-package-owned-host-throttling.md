# ADR 0011: Reserve physical attempts through a package-owned host throttle

- Status: Accepted for the experimental M1 protocol slice
- Date: 2026-07-13
- Owner: ksonda

## Context

Retries, redirects, PID resolution, reference pagination, and explicit asset
downloads can produce several requests to one provider. Per-workflow sleeps do
not compose across clients, and wall-clock timestamps are unsuitable for rate
control because they can move backward. Generic process parallelism would also
copy rather than share throttle and budget state.

## Decision

- Every physical HTTP or file-download attempt reserves a slot in one
  package-owned, per-process throttle keyed by normalized hostname.
- `gx_client(min_interval=)` controls the minimum start interval and defaults
  to 0.1 seconds. All clients in one R process share reservations for a host;
  different hosts have independent schedules. Between clients with different
  intervals, the next gap is the larger of the preceding and current values,
  so a zero-interval client cannot cut ahead of an existing reservation.
- The throttle uses a numeric monotonic clock and sleeps only for the remaining
  interval. Test-only injected clock and sleeper functions make scheduling
  deterministic and avoid real waits. When the package-default sleeper
  completes a delay below the platform clock's observable resolution, the
  logical reservation advances to the requested boundary; injected timing
  functions remain fail-closed if time does not advance.
- Retries and manually followed redirects reserve a new slot. Retry delay and
  host delay compose conservatively, so one never shortens the other.
- Cache hits and offline cache misses do not consult the throttle because they
  perform no DNS or network work. A failed DNS/safety check after reservation
  still consumes that start slot and records one zero-byte rejected attempt so
  workflow request budgets close their reservation deterministically.
- Rich internal attempt metadata records throttle delay alongside retry delay
  and the revalidated pinned address.
- Forked or spawned workers do not constitute a supported concurrency path.
  Process identity changes clear inherited throttle state rather than implying
  cross-process coordination.

Bounded concurrency remains a separate M1 task. It must use a central,
single-process scheduler with total and per-host permits, atomic request/byte
reservations, single-flight cache keys, and deterministic ordered collection.
Wrapping current loops in futures, forks, or PSOCK workers is rejected because
the JSON-LD, reference, and crosswalk budget hooks reconcile on completion and
could otherwise oversubscribe their ceilings.

## Consequences

- Sequential workflows are polite by default without changing cache identity
  or serialized public result contracts.
- Invalid clocks, sleepers, intervals, or non-advancing injected time fail
  before another transport dispatch.
- The throttle coordinates clients only within one R process. A future curl
  multi scheduler, rather than process-global locks, will own active-request
  limits and concurrent budget reservations.
- M1 remains partial until that bounded concurrency scheduler and concurrent
  cache-write acceptance tests are implemented.
