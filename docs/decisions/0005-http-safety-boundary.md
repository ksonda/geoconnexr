# ADR 0005: Fail-closed HTTP safety boundary

- Status: Accepted for the experimental protocol client
- Date: 2026-07-13
- Owner: ksonda

## Context

Geoconnex landing pages and described distributions can direct clients to
provider-controlled URLs. Syntax-only URL checks do not prevent server-side
request forgery, DNS rebinding, decompression bombs, cache collisions, or
credential persistence. Redirects also need to be inspected before the next
request is dispatched.

## Decision

The experimental L1 HTTP client fails closed:

- resolve hostnames before each request, reject any non-public answer, select a
  public IPv4 address, pin libcurl to that address, and bypass proxy DNS;
- reject URL user information, local/private/link-local targets, and IPv6 URL
  literals until an equally testable IPv6 pinning policy is implemented;
- disable automatic content decoding, request identity encoding, reject
  compressed responses, and stream response bytes through the configured
  ceiling;
- disable libcurl redirect following so PID redirects can be checked and
  recorded one hop at a time;
- key cache entries by the full supplied header representation and request-body
  hash, never cache credentialed/range requests or any query-bearing URL, honor
  response `no-store`, `no-cache`, `private`, revalidation, `Vary: *`, and
  `Set-Cookie` exclusions, and never persist redirect `Location` values carrying
  queries, fragments, or user information; apply a fixed retrieval-time TTL,
  and revalidate cached bodies against current byte limits; and
- clear only directories carrying the package cache ownership marker.

## Consequences

- Users behind mandatory HTTP proxies may need a future reviewed connection
  policy; silently delegating target DNS to a proxy is not allowed today.
- Servers that ignore `Accept-Encoding: identity` are rejected rather than
  decompressed in memory. Bounded streaming decompression is future work.
- IPv6 literals are temporarily unsupported. Hostnames with both public A and
  AAAA records remain usable through a pinned public IPv4 address.
- Retry behavior uses `httr2`; deterministic retry/throttle/concurrency tests
  remain required before M1 is declared complete.

## Follow-up

ADR 0010 supersedes the retry portion of this consequence: retries are now
package-owned, DNS is revalidated for every physical attempt, and deterministic
retry accounting is implemented. At that decision's acceptance,
throttle/concurrency remained open. ADR 0011 now implements package-owned
per-host throttling; bounded concurrency remains open.
