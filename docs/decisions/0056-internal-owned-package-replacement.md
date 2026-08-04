# ADR 0056: Replace only verified owned packages with synchronous rollback

- Status: Accepted internal substrate
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0019, ADR 0050, ADR 0051, ADR 0052

## Context

Creation-only publication avoids deleting arbitrary user data, but the roadmap
requires an eventual `overwrite = TRUE` path. ADR 0019 gates that path on proof
of package ownership plus rollback and recovery semantics. Adding an
undeclared marker file would invalidate the existing closed-tree profile and
would not help packages already produced by M9m.

The exact fixed-writer manifest already acts as a content-bound ownership
marker: the destination must pass complete closed-tree verification, declare
the fixed geoconnexr writer and resource profiles, and bind package-input and
bundle identities. That evidence proves package-relative ownership, not user
identity or authenticity.

## Decision

Add the internal M9r `gx_package_replace_impl()` substrate.

Replacement:

1. accepts one fully revalidated M9k bundle and one existing destination;
2. admits the destination only when it verifies as the exact fixed M9m package
   profile;
3. publishes and verifies the replacement at a unique sibling path before
   touching the destination;
4. rechecks the parent, destination filesystem identity, and complete prior
   package evidence immediately before the swap;
5. renames the prior package to a unique sibling backup and then renames the
   prepared replacement into place;
6. verifies the exposed replacement against the exact M9k bundle before
   deleting the backup; and
7. synchronously restores the prior package when installation or final
   verification fails.

If rollback cannot complete, the typed error reports explicit recovery paths
for every retained package generation. If replacement commits but backup
cleanup fails, the error marks the replacement committed and reports the
retained prior-package path. No recovery path is silently deleted after its
ownership becomes uncertain.

Successful replacement returns an internal `gx_package_replacement` value
binding the prior verification, final verification, new bundle, fixed recovery
policy, resource counts, and deterministic replacement identity.

Portable R cannot provide a hostile-filesystem `renameat2(RENAME_EXCHANGE)` or
`openat` guarantee. The contract covers detected races and synchronous process
failures on one filesystem; process termination between rename operations can
leave clearly prefixed sibling recovery directories for manual inspection.

## Acceptance criteria

- Catalog packages can be replaced by exact catalog, fetched, or harmonized
  bundles only after both generations verify.
- Missing, arbitrary, linked, malformed, corrupted, or non-fixed-profile
  destinations remain untouched and fail ownership admission.
- A failed install restores the prior destination; a failed restoration
  retains and reports both verified recovery paths.
- A failed final verification moves the rejected replacement aside, restores
  and re-verifies the prior package, and removes only the owned rejected tree.
- A failed post-commit backup cleanup reports both committed state and the
  retained recovery path.
- Prior/final evidence, metadata, and identity forgery fail closed.
- Replacement performs no network, DNS, cache, optional-package, report,
  refresh, or replay work.
- The substrate remains unexported and public `overwrite = TRUE` remains
  rejected.

## Consequences

- The destructive part of overwrite now has an explicit ownership and recovery
  contract that can be exercised independently of the public API.
- A later public checkpoint can expose replacement without changing the fixed
  serialized package profile.
- Arrow/Parquet, Quarto, Frictionless validation, reports, authenticity,
  refresh, and replay remain later M9 work.
