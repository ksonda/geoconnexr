# ADR 0057: Expose verified package replacement through `gx_package()`

- Status: Accepted public boundary
- Date: 2026-08-03
- Owners: package maintainers
- Related: ADR 0019, ADR 0050, ADR 0051, ADR 0056

## Context

M9m exposes deterministic package creation only when the destination is absent.
M9r now provides the missing destructive substrate: it recognizes ownership
from the complete fixed-writer package profile, prepares the new generation at
a verified sibling path, preserves the prior generation in a sibling backup,
and synchronously restores it when installation or final verification fails.

The public `overwrite` argument can therefore be enabled without adding a
second writer or weakening the creation path. Public evidence must still make
the destructive operation visible and bind both package generations rather
than returning a result indistinguishable from first creation.

## Decision

Enable `overwrite = TRUE` in `gx_package()` as the M9s public replacement
boundary.

1. `overwrite = FALSE` retains the M9m behavior and requires an absent
   destination.
2. `overwrite = TRUE` builds and revalidates the same exact M9j–M9k bundle,
   then delegates all filesystem replacement and recovery work to M9r.
3. A missing, arbitrary, malformed, corrupt, linked, or non-fixed-profile
   destination fails ownership admission without being changed.
4. The public `gx_package` contract advances to version 0.2.0. Creation results
   have `mode = "fixed_package_creation"`, `status =
   "written_and_verified"`, and `previous = NULL`. Replacement results have
   `mode = "fixed_package_replacement"`, `status =
   "replaced_and_verified"`, and embed the complete prior verification as
   `previous`.
5. Public metadata explicitly distinguishes creation from replacement through
   `scope`, `creation_only`, and `overwrite`. The package identity binds the
   operation, normalized destination, prior manifest hash when present, final
   manifest hash, package-input identity, bundle identity, and resource facts.
6. M9r recovery errors pass through with their rollback state and recovery
   paths. The replacement implementation and its evidence object remain
   internal.

This is package-relative ownership and unsigned integrity evidence. It does not
authenticate a user or publisher. Portable R still cannot promise crash-atomic
directory exchange; interruption between renames can leave clearly prefixed
sibling recovery directories described by ADR 0056.

## Acceptance criteria

- Catalog, fetched, and harmonized inputs can replace an intact fixed-profile
  package through the public API and return evidence binding both generations.
- Ordinary creation continues to require an absent destination and returns no
  prior-generation evidence.
- Missing, arbitrary, and corrupt overwrite targets remain untouched.
- Detected installation and final-verification failures preserve the M9r
  synchronous rollback and recovery behavior.
- Mode, status, prior/final verification, metadata, path, stage, and identity
  forgery fail closed.
- Creation and replacement perform no network, DNS, cache, report, refresh, or
  replay work.
- Replacement helpers and intermediate evidence remain unexported.

## Consequences

- The documented `overwrite` argument now has a safe public implementation for
  packages produced by the exact fixed writer profile.
- Callers can distinguish first publication from replacement and inspect the
  verified prior package generation.
- Arrow/Parquet, Quarto, Frictionless validation, reports, authenticity,
  refresh, and replay remain later M9 work.
