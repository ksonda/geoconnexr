# ADR 0064: Integrate verified reports under a private package profile

- Status: Accepted internal substrate
- Date: 2026-08-04
- Owners: package maintainers
- Related: ADR 0050, ADR 0056, ADR 0063
- Supersedes: the package-integration deferral in ADR 0063
- Superseded in part: public creation, loading, and access gates by ADR 0065

## Context

M9y returns exact verified HTML bytes and complete host-specific render
evidence, but deliberately leaves those bytes outside package resources. The
next boundary must prove that report bytes can pass through the same bounded
resource inventory, manifest, closed-tree staging, atomic creation, and owned
replacement guarantees as other package resources.

Public `gx_package(report = TRUE)` is still premature. The public loader and
hydrator recognize only the report-free writer profile, and a report accessor
needs a separately reviewed byte and HTML inspection contract. Package
integration should therefore be testable without silently widening any public
API or making existing report-free manifests change.

## Decision

Add the internal M9z `gx_package_report_resources_impl()` boundary and a private
report-aware manifest profile.

1. Accept one exact M9k `gx_package_resources` bundle and one exact M9y report.
   Revalidate both completely.
2. Require the report's hydrated source package to bind the base bundle's
   stage, timeseries format, package-input identity, bundle identity, fixed
   report-free resource profile, and manifest identity.
3. Preserve every base resource and byte unchanged. Add exactly one bounded
   `report/index.html` resource whose bytes and SHA-256 equal the verified M9y
   output.
4. Return a separate `gx_package_report_resources` value object. Its identity
   binds the base bundle, report, full resource evidence, counts, and stored
   bytes. Cross-Quarto-version byte determinism remains false.
5. Extend the internal package writer and replacement substrate through one
   shared bundle validator. Report bundles set recipe `output.report = true`,
   use `fixed-in-memory-resources-v2+fixed-report-v1`, and record exact report,
   base-bundle, hydration, source-manifest, source-byte, HTML-byte, Quarto R,
   and Quarto CLI evidence.
6. Require staged and final closed-tree verification to rebind that private
   manifest profile and every resource byte. Existing report-free bundles keep
   their prior manifest shape and profile.
7. Admit both intact public report-free packages and intact private report
   packages as replacement ownership markers, retaining the existing sibling
   backup and synchronous rollback guarantees.
8. Keep all M9z constructors and profile validators internal. The public
   package loader continues to reject report packages, and public
   `gx_package(..., report = TRUE)` remains disabled.

## Acceptance criteria

- An M9y report derived from the exact published form of an M9k bundle adds one
  byte-identical HTML resource and preserves every base resource unchanged.
- Private report packages pass staged and final closed-tree verification for
  both absent-destination creation and owned replacement.
- A private report package can itself be replaced through the same ownership
  and rollback substrate.
- Mismatched source bundle, stage, format, input identity, report identity,
  bytes, resource evidence, manifest metadata, counts, or bundle identity fail
  closed.
- Public loading and typed hydration reject the private report profile, while
  report-free public manifests and behavior remain unchanged.
- No rendering, network, cache, refresh, replay, or public API is added by the
  integration boundary.

## Consequences

- Verified report bytes now participate in the same package integrity and
  replacement guarantees as other fixed resources.
- This decision established report packages as private; ADR 0065 later opened
  the reviewed public creation and byte/HTML access boundaries without
  reopening the rendering, resource, staging, or rollback design.
