# Evidence fixtures

`checked-evidence-v1.json` records the small invariants observed during the
2026-07-12 specification review. It is evidence, not a permanent service-level
contract. Mutable counts and statuses should be reported as drift by scheduled
checks; they should not make ordinary offline tests depend on the network.

No response body or large geometry is committed here. Protocol fixtures should
be minimized and sanitized from separately captured responses, with retrieval
time, request, media type, final URL, content hash, and original byte count in a
sidecar. The P0 five-profile JSON-LD matrix still needs four provider-diverse
captures in addition to the checked reference-gage profile.
