# ADR 0072: Share publisher conformance assets across languages

- Status: Accepted P4 portability boundary
- Date: 2026-08-12
- Owners: package maintainers
- Related: ADR 0071

## Context

ADR 0071 defines the R publisher profile, but an R-only test suite does not
establish that another implementation can reproduce its output. P4 requires
shared conformance assets and a Python feasibility result. The test data must
remain available in installed packages and must not depend on live services,
remote contexts, or optional libraries.

## Decision

1. Install the publisher 1.0.0 corpus under
   `inst/conformance/publisher-v1/`.
2. Include one language-neutral input, deterministic profile output, invalid
   profile, exact validation findings, sitemap URI input, and exact sitemap
   bytes.
3. Pin every resource by byte length and SHA-256 in a closed manifest. Also
   pin the compact JSON profile digest, finding count, and sitemap digest.
4. Make the R suite rebuild the profile from the language-neutral input and
   compare the exact semantic object, findings, sitemap bytes, and digests.
5. Provide a Python standard-library harness that independently rebuilds and
   checks the same known answers. It is a feasibility and portability check,
   not a public Python package.
6. Run the Python harness on the oldest and newest reviewed Python versions in
   a dedicated, dependency-free CI job.

## Acceptance criteria

- The installed corpus has a closed inventory and all resource sizes and
  digests match its manifest.
- R reproduces the shared profile, findings, and sitemap known answers.
- Python reproduces the same results without third-party packages or network
  access.
- The harness passes on Python 3.9 and 3.13 in CI.
- The shared assets contain only synthetic data and make no live provider
  claims.

## Consequences

- Publisher behavior can be checked by a future port without loading R.
- The known-answer corpus is now a compatibility surface. Changes require a
  new conformance contract version or an explicit migration.
- A complete supported Python client remains separate work.
