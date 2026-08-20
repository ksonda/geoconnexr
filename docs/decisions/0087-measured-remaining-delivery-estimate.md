# ADR 0087: Replace provisional phase ranges with a measured remaining estimate

- Status: Accepted planning baseline
- Date: 2026-08-15
- Owner: ksonda
- Supersedes: the provisional week ranges in Section 9

## Context

The original roadmap estimated every phase before implementation began. Main
moved from the architecture scaffold on July 12 to completed fixed-package M9
on August 11 and merged M10 plus the installed HUC10 demo on August 15. The
remaining work is narrower than the old phase ranges imply.

Validation time is now measurable. The PR 26 rerun completed its longest fixed
package profile in 41 minutes 3 seconds and its macOS package check in 49
minutes 29 seconds. Publisher conformance completed in seconds and pkgdown in 2
minutes 17 seconds. A head revision should therefore reserve 90 minutes for one
aggregate matrix, including queue and platform variation, before a result is
treated as late.

Automatic HUC10 graph discovery is different from the other work. ADR 0085
records that the endpoint evaluates a bound spatial control but times out on an
unbound one-row AOI search. Package engineering cannot supply a credible date
for an upstream query capability.

## Decision

Replace phase-level week ranges with the following remaining delivery estimate.
A focused engineering day includes implementation, fixtures, documentation,
review, and local validation. CI time is listed separately because each pushed
head revision starts the aggregate matrix.

| Remaining work | Estimate | Validation allowance |
|---|---:|---:|
| Merge the current roadmap stack and triage the live smoke | 1 focused day | 1 aggregate matrix plus 1 live run |
| M1 bounded single-process concurrency | 3 to 5 focused days | 3 aggregate matrices |
| Remaining M6 HUC8, county, and upstream-basin fixtures | 3 to 5 focused days | 2 aggregate matrices and bounded live probes |
| One additional named M8 provider schema or variable family | 1 to 2 focused days each | 1 aggregate matrix per family |
| Release documentation, lifecycle review, and CRAN hardening | 2 to 3 focused days | 1 aggregate matrix plus release checks |
| Automatic HUC10 graph site discovery | No internal estimate | Reprobe after upstream capability changes |

The unblocked fixed scope is 9 to 14 focused engineering days. Broader M8 work
is additive by named schema or variable family. The graph item is excluded from
that total because it has an external prerequisite.

Bounded concurrency should ship as three reviewable slices: central permits,
budgets, single-flight keys, and deterministic tests; curl-multi transport and
cache integration; then public fetch orchestration. The estimate must be
revisited if the first slice shows that a handler cannot expose an asynchronous
request without changing its frozen result contract.

## Consequences

- Completed phases no longer carry fictional future week ranges.
- CI latency is part of delivery planning rather than hidden inside each slice.
- Provider expansion is estimated per reviewed family instead of as an
  undefined request to support broader schemas.
- The graph dependency remains visible and does not inflate the internal work
  estimate.
- Estimates should be updated from completed PR and live-run evidence rather
  than by extending these numbers automatically.
