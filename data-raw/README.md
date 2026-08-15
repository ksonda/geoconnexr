# Maintainer evidence scripts

Nothing in this directory runs during package load, examples, ordinary tests, or
CRAN checks. Live scripts require the explicit environment gate:

```sh
GEOCONNEXR_RUN_LIVE=true sh data-raw/live/validate-known-answers.sh
GEOCONNEXR_RUN_LIVE=true Rscript data-raw/live/validate-jsonld-profile.R
```

Run from the repository root. These probes are deliberately bounded and print
their findings; they do not overwrite committed evidence. Review and minimize
any future captures before adding them under `tests/fixtures/`.

The publisher portability harness is offline and uses only the Python standard
library:

```sh
python3 data-raw/conformance/validate-publisher-v1.py
```

It checks the same installed conformance corpus used by the R publisher tests.
