# Snapshot verification fixtures

`catalog-only-v1/` is the minimal closed-world M9a fixture. Its embedded
request ledger is authoritative; `requests.csv` is intentionally treated as an
opaque, hash-verified export until a canonical ledger serialization contract is
accepted. Every file under the fixture root other than `manifest.json` must be
declared in the manifest.
