# Minimized reference-client fixtures

These hand-minimized fixtures preserve the OGC API response shapes and identity
queryable roles needed by deterministic M3 tests. They cover `gages`, legacy
and v3 mainstems, HUC12s, and counties without embedding full upstream
geometries. Live behavior remains mutable and is checked only by bounded,
opt-in smoke tests. `manifest-v1.json` records each source URL, checked date,
minimization basis, and SHA-256 of the stored fixture.
