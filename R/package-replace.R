.gx_package_replacement_contract_version <- "0.1.0"

.gx_package_replacement_fields <- c(
  "contract_version", "mode", "status", "stage", "path", "bundle",
  "previous", "verification", "metadata", "replacement_id"
)

.gx_package_replacement_metadata_fields <- c(
  "scope", "overwrite", "ownership_profile", "rollback_strategy",
  "prior_manifest_sha256", "resources", "stored_bytes", "backup_retained",
  "authenticity", "replayable"
)

gx_package_replace_abort <- function(
    message,
    class = "gx_error_package_replace_contract",
    ...,
    call = rlang::caller_env()) {
  gx_package_writer_abort(
    message,
    class = unique(c(class, "gx_error_package_replace")),
    ...,
    call = call
  )
}

gx_package_replace_rename <- function(from, to) {
  gx_package_writer_rename(from, to)
}

gx_package_replace_cleanup <- function(path) {
  gx_package_writer_cleanup_impl(path)
}

gx_package_replace_verify <- function(path) {
  gx_snapshot_verify(path)
}

gx_package_replace_write <- function(bundle, path) {
  gx_package_write_impl(bundle, path)
}

gx_package_replace_owned_impl <- function(path, allow_report = TRUE) {
  verification <- tryCatch(
    gx_package_replace_verify(path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  profile <- if (is.null(verification)) NULL else tryCatch(
    if (isTRUE(allow_report)) {
      gx_package_owned_manifest_profile_impl(verification)
    } else {
      gx_package_manifest_profile_impl(verification)
    },
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  if (is.null(verification) || is.null(profile)) {
    gx_package_replace_abort(
      paste0(
        "The existing destination is not an intact package owned by the ",
        "fixed geoconnexr writer profile."
      ),
      "gx_error_package_replace_ownership"
    )
  }
  list(verification = verification, profile = profile)
}

gx_package_replace_same_previous_impl <- function(left, right) {
  identical(left$status, right$status) &&
    identical(left$manifest, right$manifest) &&
    identical(left$aoi, right$aoi) &&
    identical(left$request_count, right$request_count) &&
    identical(left$resources, right$resources) &&
    identical(left$manifest_sha256, right$manifest_sha256)
}

gx_package_replace_temp_path_impl <- function(parent, prefix) {
  path <- tempfile(pattern = prefix, tmpdir = parent)
  if (gx_snapshot_writer_entry_exists(path)) {
    gx_package_replace_abort(
      "A replacement recovery path unexpectedly already exists.",
      "gx_error_package_replace_race"
    )
  }
  path
}

gx_package_replace_cleanup_impl <- function(path, committed = FALSE) {
  if (isTRUE(gx_package_replace_cleanup(path))) return(invisible(TRUE))
  gx_package_replace_abort(
    if (committed) {
      paste0(
        "The replacement committed, but its verified prior package remains ",
        "at a recovery path."
      )
    } else {
      "Replacement rollback succeeded, but staged content remains at a recovery path."
    },
    "gx_error_package_replace_cleanup",
    replacement_committed = committed,
    recovery_path = normalizePath(path, winslash = "/", mustWork = FALSE)
  )
}

gx_package_replace_restore_before_install_impl <- function(
    backup,
    target,
    prepared) {
  restored <- isTRUE(gx_package_replace_rename(backup, target))
  if (!restored) {
    gx_package_replace_abort(
      paste0(
        "The replacement could not be installed or rolled back; the prior ",
        "and prepared packages remain at recovery paths."
      ),
      "gx_error_package_replace_recovery",
      rollback_restored = FALSE,
      recovery_path = normalizePath(
        backup, winslash = "/", mustWork = FALSE
      ),
      prepared_path = normalizePath(
        prepared, winslash = "/", mustWork = FALSE
      )
    )
  }
  gx_package_replace_cleanup_impl(prepared, committed = FALSE)
  gx_package_replace_abort(
    "The replacement could not be installed; the prior package was restored.",
    "gx_error_package_replace_io",
    rollback_restored = TRUE
  )
}

gx_package_replace_restore_after_install_impl <- function(
    target,
    backup,
    parent,
    previous) {
  failed <- gx_package_replace_temp_path_impl(
    parent, ".gx-package-failed-replacement-"
  )
  moved <- isTRUE(gx_package_replace_rename(target, failed))
  if (!moved) {
    gx_package_replace_abort(
      paste0(
        "Final replacement verification failed and the new destination ",
        "could not be moved for rollback; the prior package remains at a ",
        "recovery path."
      ),
      "gx_error_package_replace_recovery",
      rollback_restored = FALSE,
      recovery_path = normalizePath(
        backup, winslash = "/", mustWork = FALSE
      ),
      installed_path = normalizePath(
        target, winslash = "/", mustWork = FALSE
      )
    )
  }
  restored <- isTRUE(gx_package_replace_rename(backup, target))
  if (!restored) {
    gx_package_replace_abort(
      paste0(
        "Final replacement verification failed and rollback could not be ",
        "completed; both packages remain at recovery paths."
      ),
      "gx_error_package_replace_recovery",
      rollback_restored = FALSE,
      recovery_path = normalizePath(
        backup, winslash = "/", mustWork = FALSE
      ),
      failed_path = normalizePath(
        failed, winslash = "/", mustWork = FALSE
      )
    )
  }
  restored_verification <- tryCatch(
    gx_package_replace_verify(target),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  valid_restore <- !is.null(restored_verification) &&
    gx_package_replace_same_previous_impl(previous, restored_verification)
  if (!valid_restore) {
    gx_package_replace_abort(
      paste0(
        "Rollback restored a destination whose package identity could not ",
        "be re-established; the rejected replacement remains recoverable."
      ),
      "gx_error_package_replace_recovery",
      rollback_restored = FALSE,
      failed_path = normalizePath(
        failed, winslash = "/", mustWork = FALSE
      )
    )
  }
  gx_package_replace_cleanup_impl(failed, committed = FALSE)
  gx_package_replace_abort(
    "Final replacement verification failed; the prior package was restored.",
    "gx_error_package_replace_verification",
    rollback_restored = TRUE
  )
}

gx_package_replacement_metadata_impl <- function(
    bundle,
    previous) {
  list(
    scope = "fixed_package_replacement_v1",
    overwrite = TRUE,
    ownership_profile = .gx_package_writer_profile,
    rollback_strategy = "verified_sibling_backup_v1",
    prior_manifest_sha256 = previous$manifest_sha256,
    resources = unname(as.integer(nrow(bundle$resources))),
    stored_bytes = unname(as.double(sum(bundle$resources$bytes))),
    backup_retained = FALSE,
    authenticity = FALSE,
    replayable = FALSE
  )
}

gx_package_replacement_id_impl <- function(
    path,
    bundle,
    previous,
    verification,
    metadata) {
  gx_contract_hash(
    list(
      "mode", "fixed_package_replacement",
      "status", "replaced_and_verified",
      "path", path,
      "stage", bundle$stage,
      "bundle_id", bundle$bundle_id,
      "prior_manifest_sha256", previous$manifest_sha256,
      "manifest_sha256", verification$manifest_sha256,
      "resources", metadata$resources,
      "stored_bytes", metadata$stored_bytes
    ),
    namespace = "geoconnexr.package-replacement.v1",
    contract_version = .gx_package_replacement_contract_version
  )
}

gx_package_replacement_validate_impl <- function(x) {
  valid_top <- is.list(x) && identical(class(x), "gx_package_replacement") &&
    identical(names(attributes(x)), c("names", "class")) &&
    identical(names(x), .gx_package_replacement_fields) &&
    identical(
      x$contract_version, .gx_package_replacement_contract_version
    ) &&
    identical(x$mode, "fixed_package_replacement") &&
    identical(x$status, "replaced_and_verified") &&
    is.character(x$stage) && length(x$stage) == 1L && !is.na(x$stage) &&
    is.character(x$path) && length(x$path) == 1L && !is.na(x$path) &&
    nzchar(x$path) &&
    is.character(x$replacement_id) && length(x$replacement_id) == 1L &&
    gx_catalog_is_sha256(x$replacement_id)
  if (!valid_top) {
    gx_package_replace_abort(
      "Package-replacement evidence violates its exact top-level contract."
    )
  }
  bundle_valid <- tryCatch({
    gx_package_bundle_validate_impl(x$bundle)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  previous_valid <- tryCatch({
    gx_snapshot_verification_validate_impl(x$previous)
    gx_package_owned_manifest_profile_impl(x$previous)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  verification_valid <- tryCatch({
    gx_snapshot_verification_validate_impl(x$verification)
    gx_package_bundle_manifest_validate_impl(x$verification, x$bundle)
    TRUE
  }, error = function(cnd) FALSE, warning = function(cnd) FALSE)
  root <- tryCatch(
    gx_snapshot_root(x$path),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )
  metadata <- if (bundle_valid && previous_valid) {
    gx_package_replacement_metadata_impl(x$bundle, x$previous)
  } else {
    NULL
  }
  valid <- bundle_valid && previous_valid && verification_valid &&
    !is.null(root) && identical(root$path, x$path) &&
    identical(x$stage, x$bundle$stage) &&
    identical(x$verification$resources$path, x$bundle$resources$path) &&
    identical(
      x$verification$resources$expected_bytes,
      x$bundle$resources$bytes
    ) &&
    identical(
      x$verification$resources$expected_sha256,
      x$bundle$resources$sha256
    ) &&
    is.list(x$metadata) &&
    identical(names(x$metadata), .gx_package_replacement_metadata_fields) &&
    identical(x$metadata, metadata) &&
    identical(
      x$replacement_id,
      gx_package_replacement_id_impl(
        x$path, x$bundle, x$previous, x$verification, x$metadata
      )
    )
  if (!valid) {
    gx_package_replace_abort(
      "Package-replacement evidence no longer binds both package generations."
    )
  }
  invisible(x)
}

# Internal M9r boundary. It replaces only a closed, verified package produced
# by the fixed geoconnexr writer profile and retains synchronous recovery paths
# whenever rollback cannot be completed safely.
gx_package_replace_impl <- function(bundle, dir) {
  cleanup <- new.env(parent = emptyenv())
  cleanup$prepared <- NULL
  cleanup$owned <- FALSE
  tryCatch(
    {
      gx_package_bundle_validate_impl(bundle)
      destination <- gx_snapshot_writer_scalar_path(dir)
      if (!gx_snapshot_writer_entry_exists(destination$target)) {
        gx_package_replace_abort(
          "Package replacement requires an existing owned destination.",
          "gx_error_package_replace_ownership"
        )
      }
      target_info <- tryCatch(
        gx_snapshot_assert_fs_type(destination$target, "directory"),
        error = function(cnd) NULL,
        warning = function(cnd) NULL
      )
      if (is.null(target_info)) {
        gx_package_replace_abort(
          "The replacement destination is not a regular directory.",
          "gx_error_package_replace_ownership"
        )
      }
      owned <- gx_package_replace_owned_impl(
        destination$target
      )
      previous <- owned$verification
      prepared <- gx_package_replace_temp_path_impl(
        destination$parent, ".gx-package-prepared-replacement-"
      )
      cleanup$prepared <- prepared
      cleanup$owned <- TRUE
      gx_package_replace_write(bundle, prepared)

      parent_info <- gx_snapshot_assert_fs_type(
        destination$parent, "directory"
      )
      gx_snapshot_assert_same_info(
        target_info,
        gx_snapshot_assert_fs_type(destination$target, "directory")
      )
      current <- gx_package_replace_owned_impl(
        destination$target
      )$verification
      if (!gx_package_replace_same_previous_impl(previous, current)) {
        gx_package_replace_abort(
          "The owned destination changed while its replacement was staged.",
          "gx_error_package_replace_race"
        )
      }
      gx_snapshot_assert_same_info(
        parent_info,
        gx_snapshot_assert_fs_type(destination$parent, "directory")
      )

      backup <- gx_package_replace_temp_path_impl(
        destination$parent, ".gx-package-prior-recovery-"
      )
      if (!isTRUE(gx_package_replace_rename(destination$target, backup))) {
        gx_package_replace_cleanup_impl(prepared, committed = FALSE)
        cleanup$owned <- FALSE
        gx_package_replace_abort(
          "The verified prior package could not be moved for replacement.",
          "gx_error_package_replace_io",
          rollback_restored = TRUE
        )
      }
      installed <- isTRUE(gx_package_replace_rename(
        prepared, destination$target
      ))
      if (!installed) {
        cleanup$owned <- FALSE
        gx_package_replace_restore_before_install_impl(
          backup, destination$target, prepared
        )
      }
      cleanup$owned <- FALSE

      final <- tryCatch(
        {
          verification <- gx_package_replace_verify(destination$target)
          gx_package_writer_assert_verification_impl(verification, bundle)
          verification
        },
        error = function(cnd) NULL,
        warning = function(cnd) NULL
      )
      if (is.null(final)) {
        gx_package_replace_restore_after_install_impl(
          destination$target,
          backup,
          destination$parent,
          previous
        )
      }
      gx_package_replace_cleanup_impl(backup, committed = TRUE)

      path <- normalizePath(
        destination$target, winslash = "/", mustWork = TRUE
      )
      metadata <- gx_package_replacement_metadata_impl(bundle, previous)
      object <- structure(
        list(
          contract_version = .gx_package_replacement_contract_version,
          mode = "fixed_package_replacement",
          status = "replaced_and_verified",
          stage = bundle$stage,
          path = unname(path),
          bundle = bundle,
          previous = previous,
          verification = final,
          metadata = metadata,
          replacement_id = gx_package_replacement_id_impl(
            unname(path), bundle, previous, final, metadata
          )
        ),
        class = "gx_package_replacement"
      )
      gx_package_replacement_validate_impl(object)
      object
    },
    error = function(cnd) {
      if (isTRUE(cleanup$owned) && is.character(cleanup$prepared)) {
        cleaned <- gx_package_replace_cleanup(cleanup$prepared)
        if (!isTRUE(cleaned)) {
          gx_package_replace_abort(
            paste0(
              "Package replacement failed and its prepared package remains ",
              "at a recovery path."
            ),
            "gx_error_package_replace_cleanup",
            replacement_committed = FALSE,
            recovery_path = normalizePath(
              cleanup$prepared, winslash = "/", mustWork = FALSE
            )
          )
        }
        cleanup$owned <- FALSE
      }
      if (inherits(cnd, "gx_error_package_replace")) stop(cnd)
      gx_package_replace_abort(
        "Package replacement failed closed.",
        "gx_error_package_replace_contract"
      )
    }
  )
}
