# ADR 0003: CRAN and r-universe release channels

- Status: Accepted
- Date: 2026-07-12
- Owner: ksonda

## Context

The project needs a stable R distribution channel and a faster channel for
architecture-spike and development builds. Live upstream checks are valuable
but are not suitable as CRAN checks or examples.

## Decision

Target CRAN for stable releases and r-universe for development builds. GitHub
remains the source of record.

CRAN-bound checks, examples, tests, and vignettes must succeed without network
access. Weekly live semantic checks run in a separate, bounded GitHub Actions
workflow and are not required pull-request checks.

## Consequences

- CRAN constraints shape examples, dependency declarations, file sizes, and
  offline fixtures from the beginning.
- r-universe may expose development behavior before public contracts are
  frozen; development versioning and release notes must make that explicit.
- A CRAN submission is not permitted until `cran-comments.md` contains verified
  check results rather than placeholders.
- Publishing credentials and r-universe repository configuration remain
  operational setup tasks outside this ADR.
