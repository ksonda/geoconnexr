# Minimized M4 crosswalk fixtures

These fixtures preserve the queryable, feature, and lookup shapes required to
verify the first M4 crosswalk slices. The v3.2 CSV sample contains observed
forward-mapping rows but deliberately does not claim complete inverse groups.
Inverse tests over this excerpt are adapter-conformance tests within a mocked
fixture release, not evidence that the excerpt contains full production groups.
The ambiguity CSV is synthetic and tests a future zero-to-many adapter without
misrepresenting the pinned v3.2 asset, whose audited COMIDs are unique.
`manifest-v1.json` records sources, transformations, evidence kinds, reuse
bases, licenses, and stored SHA-256 values. The full optional v3.2 lookup is
not bundled; the six-row CC0 excerpt is only a conformance fixture.
