## Release summary

This will be the first CRAN submission of `geoconnexr`.

Development builds are intended for r-universe. CRAN releases will contain only
contracts that have completed the architecture spike and fixture-backed
validation.

## Test environments

Pending. Before submission, replace this section with the exact local and CI
platforms used, including R release, R-devel, and R-oldrel results.

## R CMD check results

Pending. Before submission, record the final error, warning, and note counts
from `R CMD check --as-cran`, and explain every remaining NOTE.

## External services

Release checks, examples, and vignettes must be CRAN-safe and must not require
network access. Upstream behavior is covered by committed fixtures. Small live
semantic smoke tests run separately on a weekly GitHub Actions schedule, with
finite request, time, row, and byte budgets; they do not run on CRAN and do not
perform full-graph counts.

## Downstream dependencies

None expected for the initial submission. Confirm with `revdepcheck` before any
later release that has downstream packages.
