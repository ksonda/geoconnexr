# Security policy

## Supported versions

`geoconnexr` has not made its first stable release. Security fixes
currently target the `main` branch. After releases begin, the latest
CRAN release and the development branch will be supported; older
versions may be asked to upgrade.

## Reporting a vulnerability

Do not report a vulnerability in a public issue, discussion, test
fixture, or request log. Use GitHub’s [private vulnerability
reporting](https://github.com/ksonda/geoconnexr/security/advisories/new).
If that form is unavailable, contact the repository owner using the
private contact method listed on the [ksonda GitHub
profile](https://github.com/ksonda).

Include, when possible:

- the affected version or commit;
- a minimal reproduction that does not expose third-party data;
- impact and realistic attack conditions;
- suggested mitigations; and
- whether the issue has been disclosed elsewhere.

The maintainer aims to acknowledge a report within three business days
and provide an initial assessment within seven. Remediation and
disclosure timing will depend on severity, exploitability, and upstream
coordination. Credit is offered unless the reporter prefers anonymity.

## Relevant threat boundaries

Reports are especially useful for weaknesses involving:

- server-side request forgery through provider-controlled URLs or
  redirects;
- access to loopback, private, link-local, or cloud-metadata addresses;
- unsafe remote JSON-LD contexts;
- decompression bombs, parser-depth exhaustion, or budget bypasses;
- cache poisoning or cross-representation cache collisions;
- SPARQL injection or unsafe query-template substitution;
- unintended code execution while classifying or fetching a
  distribution; or
- credentials or sensitive values exposed in diagnostics and request
  ledgers.

Service outages, ordinary upstream schema drift, and public-data
corrections are usually reliability issues rather than vulnerabilities.
Report those with the bug template unless they create a security impact.
