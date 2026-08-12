# ADR 0065: Publish and access fixed verified package reports

- Status: Accepted public boundary
- Date: 2026-08-04
- Owners: package maintainers
- Related: ADR 0051, ADR 0052, ADR 0055, ADR 0057, ADR 0064
- Supersedes: the public-creation, loading, and access gates in ADR 0064

## Context

M9z proves that one fixed M9y HTML report can be bound to its exact originating
package bundle and carried through staged creation and owned replacement. The
remaining public boundary must orchestrate that substrate without weakening
its lineage, allowing implicit rendering, retaining private source packages,
or treating unsigned internal consistency as authenticity.

Public access also needs to distinguish exact byte exposure from rendering.
Reading an existing report must remain offline and must not require Quarto.
Writing a standalone copy must preserve the package bytes and must not replace
an existing file.

## Decision

Add M9aa public report creation and access.

1. Extend `gx_package()` with `report = FALSE`. Only one explicit `TRUE`
   authorizes report runtime admission and rendering; the default path never
   invokes a report seam.
2. For `report = TRUE`, serialize the exact report-free base bundle to an owned
   private package, hydrate its fixed typed view, render through M9y, integrate
   through M9z, and retain that source package until staged/final package
   verification has rebound its lineage. Remove the private source immediately
   afterward on success or failure.
3. Publish and replace report-bearing bundles through the existing atomic
   writer and sibling-backup rollback. Allow verified transitions from
   report-free to report, report to report, and report to report-free.
4. Extend `gx_package_load()`, `gx_package_tables()`, and
   `gx_package_hydrate()` to admit the fixed report profile. The HTML remains an
   opaque, byte-preserved non-table resource.
5. Add `gx_report(x, output = NULL)` for a report-bearing package, loaded
   package, or path. Reverify the package and exact HTML bytes, reject active or
   externally linked HTML again, and return bounded `gx_report` evidence.
6. If `output` is supplied, atomically expose the exact bytes only to an absent
   file under an existing safe parent. Never invoke Quarto during access.
7. Preserve report-free resource and manifest shape. Continue to disclaim
   authenticity, replay, refresh, Frictionless validation, and cross-Quarto
   byte determinism.

## Acceptance criteria

- Explicit report creation produces one verified `report/index.html` resource
  and public package evidence with report and Quarto scope set to true.
- Public byte loading, canonical table parsing, and typed hydration accept the
  report profile without interpreting HTML as tabular data.
- `gx_report()` returns the exact manifest-bound HTML and can atomically copy
  it to one absent output file without rendering.
- Report-free, report-bearing, and repeated report replacements retain prior
  and final verification and synchronous rollback behavior.
- Invalid flags, missing capabilities, render failures, unsafe or existing
  outputs, lineage changes, byte mutation, descriptor mutation, and evidence
  forgery fail closed.
- Report failure before replacement leaves the prior package unchanged, all
  owned private source/render stages are removed, and default report-free
  creation invokes no report seam.

## Consequences

- Fixed reports are now a public opt-in package feature and an offline
  byte-access feature.
- Report creation remains host-specific and requires the reviewed Quarto R and
  CLI versions; report reading requires neither.
- Refresh/replay and full Frictionless CLI validation remain separate gates.
