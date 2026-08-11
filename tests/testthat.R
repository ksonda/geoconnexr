library(testthat)
library(geoconnexr)

# Generic check and coverage jobs omit only the slow fixed-package integration
# files. The dedicated profile workflow runs each of them independently.
skip_slow_m9 <- identical(
  tolower(Sys.getenv("GEOCONNEXR_SKIP_SLOW_M9", unset = "false")),
  "true"
)
slow_m9_filter <- paste0(
  "^(",
  paste(c(
    "package-frictionless",
    "package-frictionless-mixed",
    "package-frictionless-public",
    "package-report",
    "package-report-resources",
    "package-report-public",
    "replay-public"
  ), collapse = "|"),
  ")$"
)

test_check(
  "geoconnexr",
  filter = if (skip_slow_m9) slow_m9_filter else NULL,
  invert = skip_slow_m9
)
