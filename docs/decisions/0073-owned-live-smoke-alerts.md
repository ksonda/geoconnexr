# ADR 0073: Assign and notify the weekly live-smoke owner

- Status: Accepted operations policy
- Date: 2026-08-15
- Owner: ksonda

## Context

The bounded weekly workflow already checks stable live-service behavior, but a
failed run only appears in the Actions history. The P0 release gate requires a
named owner, a notification path, and response expectations. It must also avoid
creating a new issue for every run while an upstream problem remains open.

## Decision

The repository owner, ksonda, owns the weekly live-smoke signal. A failed
scheduled or manually dispatched run opens one issue titled
"[live-smoke] Weekly upstream compatibility failure", or comments on the
existing open issue. The workflow assigns that issue to the repository owner
and links the failing run and commit.

The response target is triage within three business days. Triage classifies the
failure as a package regression, an upstream contract change, or a temporary
service outage. Mutable result-count drift remains diagnostic and does not open
an incident by itself. A later successful live-smoke run comments with recovery
evidence and closes the open issue.

The workflow receives only read access to repository contents and write access
to issues. It does not receive permission to change code, workflows, releases,
or upstream services.

## Consequences

- Every weekly failure has a durable notification and one accountable owner.
- Repeated failures add evidence to one issue instead of creating duplicates.
- Recovery closes the incident automatically while preserving its history.
- The response target is an operating expectation, not an upstream uptime
  guarantee.
