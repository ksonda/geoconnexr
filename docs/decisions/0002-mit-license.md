# ADR 0002: MIT license

- Status: Accepted
- Date: 2026-07-12
- Owner: ksonda

## Context

The license choice was a P0 gate. The package is intended for broad use across
research, government, nonprofit, and commercial R environments and primarily
wraps public service contracts without bundling their implementations.

## Decision

License original `geoconnexr` code under the MIT License. R package metadata
uses `MIT + file LICENSE`; `LICENSE` supplies the year and copyright holder,
and `LICENSE.md` supplies the full text for the source repository.

## Consequences

- Users may reuse and redistribute the software subject to retaining the MIT
  notice.
- Contributions are accepted under the same license.
- Bundled code, schemas, examples, and data fixtures require a compatibility
  and attribution review; this ADR does not relicense third-party material.
- Relicensing later may require consent from all copyright holders.
