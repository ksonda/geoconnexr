# Contributing to geoconnexr

Thank you for contributing. `geoconnexr` is in an architecture-spike stage,
so small changes backed by fixtures and explicit contracts are especially
valuable.

## Before starting

For a bug fix, open or reference an issue with a minimal reproducer. For a new
public API, serialized format, upstream contract, or security boundary, open a
design issue before implementation and record the decision in
`docs/decisions/` when agreement is reached.

Security vulnerabilities must follow [SECURITY.md](SECURITY.md), not the
public issue tracker.

## Development setup

Install a current R release and the package development dependencies. From the
repository root:

```r
install.packages("pak")
pak::pak()
```

Run the fast local checks with:

```r
devtools::test()
devtools::check()
```

Before a release-oriented pull request, also run:

```r
devtools::check(args = "--as-cran")
```

Use roxygen comments for R help and regenerate derived documentation with
`devtools::document()`. Do not hand-edit generated `NAMESPACE` or `.Rd` files.

## Test policy

The normal test suite must be deterministic, fixture-backed, and usable
without network access. Cover classed errors, warnings, zero-row results,
leading-zero identifiers, and partial-result diagnostics where relevant.

Live service checks are exceptional:

- Name live-only files `test-live-<topic>.R`; this filename is the CI tag.
- Start each live test with an opt-in guard such as
  `testthat::skip_if(Sys.getenv("GEOCONNEXR_RUN_LIVE") != "true")`.
- Assert stable semantics, not mutable collection sizes.
- Never run a full graph triple count.
- Obey the request, row, time, encoded-byte, and decoded-byte budgets exposed
  by the weekly live workflow.
- Redact tokens, query parameters, and provider payloads from failure output.

The scheduled workflow runs only files bearing the `test-live-` tag. It is
separate from required pull-request checks and is skipped on CRAN.

## Code and data changes

- Keep protocol traffic behind the package request layer.
- Preserve hydrologic identifiers as character values.
- Treat upstream failure and truncation as explicit data, not empty success.
- Add small, provenance-noted fixtures instead of downloading data in tests.
- Do not commit secrets, personal data, or private service endpoints.
- Verify the license and attribution for any bundled code or data.

## Pull requests

Keep each pull request focused. Explain user-visible behavior, link the issue
or ADR, list validation performed, and call out any upstream assumptions.
Update `NEWS.md` for changes users will notice.

By contributing, you agree that your contribution is licensed under the
project's MIT license and that you will follow the
[Code of Conduct](CODE_OF_CONDUCT.md).
