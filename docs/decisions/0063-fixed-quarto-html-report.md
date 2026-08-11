# ADR 0063: Render one fixed isolated Quarto HTML report

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0055, ADR 0061, ADR 0062
- Supersedes: the fixed-report-contract deferral in ADR 0062

## Context

M9w admits the reviewed Quarto R package surfaces and M9x admits one exact
Quarto CLI executable and version command. Neither boundary defines a report
source, permits code execution, constrains render artifacts, or verifies HTML.
Publishing a report into a package before those controls exist would allow
runtime-dependent files, external resources, active content, and unbounded
outputs into an otherwise closed package profile.

The first report checkpoint should prove the complete isolated render contract
without yet changing package manifests or adding a public report API. A typed
M9q package view is already the narrowest redacted input that can supply stable
summary counts without reconstructing live workflow objects.

## Decision

Add the internal M9y `gx_package_report_impl()` boundary.

1. Accept and completely revalidate one exact `gx_package_hydrated` input.
2. Resolve M9x runtime evidence before staging and require its normalized CLI
   executable to remain unchanged across rendering.
3. Derive one bounded UTF-8 `report.qmd` from fixed literals, the hydration
   identity, source stage, and eight integer summary counts. The source contains
   no executable cell and fixes minimal standalone HTML, embedded resources,
   disabled execution, and disabled cache.
4. Invoke only the admitted CLI path with the exact arguments `render`, the
   staged input, `--to html`, `--output report.html`, `--no-execute`,
   `--no-cache`, and `--quiet`, under a 30-second timeout.
5. Render only inside a private mode-0700 temporary directory. Require the
   closed tree to contain exactly unchanged `report.qmd` and `report.html`,
   then remove the complete owned tree before returning.
6. Limit source bytes to 64 KiB and HTML bytes to 8 MiB. Read both across
   filesystem identity checks and retain only their exact bounded in-memory
   bytes, hashes, and structure evidence.
7. Parse HTML with network access disabled. Require one title and one report
   landmark binding the contract, hydration identity, stage, and every summary
   count. Reject scripts, frames, embedded objects, forms, base URLs, refreshes,
   and external HTTP(S) references.
8. Keep the result host-specific, internal, non-replayable, and separate from
   package resources. Package integration and public exposure remain gated.

## Acceptance criteria

- A valid hydrated package view and M9x runtime produce exact validated source,
  summary, HTML, runtime, control, and report-identity evidence.
- The exact CLI path, input, arguments, and 30-second timeout reach the render
  seam; execution and caching are disabled in both source and CLI controls.
- Active or externally linked HTML, malformed identity/count markers,
  unexpected artifacts, source mutation, CLI mutation, oversized output,
  render failure, and evidence forgery fail closed.
- Successful and failed renders remove only the private owned staging tree.
- No network, cache, package publication, manifest mutation, replay, or public
  API is introduced.

## Consequences

- The fixed report now has an end-to-end internal execution and verification
  contract rather than only dependency admission.
- Minimal HTML deliberately excludes scripts and interactive features. A later
  presentation profile would require a separate review.
- Report bytes remain host- and Quarto-version-specific; the contract binds
  exact output bytes but makes no cross-version determinism claim.
- The next M9 checkpoint can integrate the verified HTML bytes into staged
  package creation and replacement before considering public exposure.
