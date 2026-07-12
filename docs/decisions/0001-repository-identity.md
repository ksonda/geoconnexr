# ADR 0001: Repository identity

- Status: Accepted
- Date: 2026-07-12
- Owner: ksonda

## Context

The package name and owning GitHub organization were P0 product decisions.
They affect package metadata, issue links, CI ownership, release automation,
and persistent references in manifests and documentation.

## Decision

Name the R package `geoconnexr` and host its canonical repository at
`ksonda/geoconnexr`.

The `ksonda` owner is initially responsible for repository administration and
triage of the weekly live-smoke workflow until that responsibility is
explicitly delegated.

## Consequences

- Package URLs, bug reports, security reporting, and release configuration use
  `https://github.com/ksonda/geoconnexr`.
- A future organization transfer must preserve GitHub redirects and update
  package metadata and signed release processes.
- Repository identity is closed as a P0 gate; this ADR does not freeze the
  package's public R API.
