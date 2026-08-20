# ADR 0086: Keep administrative layers outside the 0.x core

- Status: Accepted product scope
- Date: 2026-08-15
- Owner: ksonda
- Closes: the remaining product decision in Section 11

## Context

The roadmap left administrative layers such as water rights undecided. These
records have different authorities, identifier systems, licences, time
semantics, and legal meaning from the hydrologic locations and observations in
the current package. Treating them as another optional reference layer would
imply a reviewed contract that the repository does not have.

The generic catalog and package boundaries can preserve caller-supplied data,
but that does not make an administrative dataset a supported geoconnexr source.

## Decision

Keep water rights and other administrative layers outside the supported 0.x
core. Do not add a built-in collection, identifier crosswalk, harmonization
rule, or completeness claim for them under the existing hydrologic contracts.

A later feature may add one named administrative source after a separate review
establishes its authority, licence, stable identifiers, temporal model, spatial
meaning, update policy, and fixture-backed request contract. That review must
also define what the package result proves and what it does not prove about
legal status.

Caller-supplied administrative resources may still be stored through generic
package mechanisms when they satisfy those mechanisms' existing input
contracts. They remain caller data, not a package-supported administrative
integration.

## Consequences

- The 0.x release does not wait for a water-rights provider audit.
- Existing hydrologic objects do not acquire legal or administrative meaning.
- A future integration needs its own contract and evidence rather than an
  undocumented collection name.
- The last open product-scope decision is closed without expanding runtime
  behavior.
