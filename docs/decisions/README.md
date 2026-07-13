# Architecture decision records

This directory records durable repository, product, and architecture choices.
An accepted ADR is append-only: if a decision changes, add a superseding ADR
and link both records rather than rewriting history.

| ADR | Status | Decision |
|---|---|---|
| [0001](0001-repository-identity.md) | Accepted | Use `ksonda/geoconnexr` |
| [0002](0002-mit-license.md) | Accepted | License package code under MIT |
| [0003](0003-release-channels.md) | Accepted | Target CRAN and r-universe |
| [0004](0004-upstream-contract-gates.md) | Accepted release gate | Defer mainstem and SPARQL contracts pending evidence |
| [0005](0005-http-safety-boundary.md) | Accepted experimental policy | Fail closed at the provider-controlled HTTP boundary |

Each ADR states its context, decision, consequences, and status. Proposed ADRs
may change; accepted ADRs govern implementation until superseded.
