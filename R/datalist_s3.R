#' Create a `metpipe_datalist` object
#'
#' Constructs the S3 container used by metpipe workflow wrappers. The object is
#' a list with `pos` and `neg` entries (either may be `NULL`) and optional
#' downstream entries such as `datatable`, `sample.info`, and `feature.info`.
#'
#' @param pos Positive-mode data list or `NULL`.
#' @param neg Negative-mode data list or `NULL`.
#' @param ... Additional named elements to store in the object.
#' @param validate Logical; validate object structure before returning.
#' @param stage Validation stage passed to [validate_metpipe_datalist()].
#'
#' @return A `metpipe_datalist` object.
#' @export
metpipe_schema_version <- function() {
  "1.0"
}


canonical_mode_fields <- function() {
  c(
    "peaks", "features", "meta",
    "peaks_cleaned", "features_cleaned", "meta_cleaned",
    "peaks_norm", "meta_norm", "normalizer",
    "stages"
  )
}


canonical_stage_names <- function() {
  c("raw", "cleaned", "norm", "merged")
}


normalize_stage_payload <- function(stage_obj = NULL) {
  out <- stage_obj
  if (is.null(out)) out <- list()
  if (!is.list(out)) {
    stop("Stage payload must be a list with peaks/features/meta")
  }
  if (is.null(out$peaks)) out$peaks <- NULL
  if (is.null(out$features)) out$features <- NULL
  if (is.null(out$meta)) out$meta <- NULL
  out
}


normalize_mode_schema <- function(mode_obj) {
  if (is.null(mode_obj)) return(NULL)
  if (!is.list(mode_obj)) {
    stop("Each mode object must be a list or NULL")
  }

  required_fields <- canonical_mode_fields()
  missing <- setdiff(required_fields, names(mode_obj))
  if (length(missing) > 0) {
    for (nm in missing) mode_obj[[nm]] <- NULL
  }

  if (is.null(mode_obj$stages) || !is.list(mode_obj$stages)) {
    mode_obj$stages <- list()
  }

  if (is.null(mode_obj$stages$raw)) {
    mode_obj$stages$raw <- list(
      peaks = mode_obj$peaks,
      features = mode_obj$features,
      meta = mode_obj$meta
    )
  }

  if (is.null(mode_obj$stages$cleaned)) {
    mode_obj$stages$cleaned <- list(
      peaks = mode_obj$peaks_cleaned,
      features = mode_obj$features_cleaned,
      meta = mode_obj$meta_cleaned
    )
  }

  if (is.null(mode_obj$stages$norm)) {
    mode_obj$stages$norm <- list(
      peaks = mode_obj$peaks_norm,
      features = if (!is.null(mode_obj$features_cleaned)) mode_obj$features_cleaned else mode_obj$features,
      meta = mode_obj$meta_norm
    )
  }

  mode_obj$stages$raw <- normalize_stage_payload(mode_obj$stages$raw)
  mode_obj$stages$cleaned <- normalize_stage_payload(mode_obj$stages$cleaned)
  mode_obj$stages$norm <- normalize_stage_payload(mode_obj$stages$norm)

  mode_obj
}


normalize_metpipe_datalist_schema <- function(x, stage = NULL) {
  if (!is.list(x)) {
    stop("datalist must be a list")
  }

  if (is.null(names(x))) {
    stop("datalist must be a named list containing pos and/or neg")
  }

  if (!"pos" %in% names(x)) x$pos <- NULL
  if (!"neg" %in% names(x)) x$neg <- NULL

  x$pos <- normalize_mode_schema(x$pos)
  x$neg <- normalize_mode_schema(x$neg)

  top_level_fields <- c("datatable", "sample.info", "feature.info")
  missing_top <- setdiff(top_level_fields, names(x))
  if (length(missing_top) > 0) {
    for (nm in missing_top) x[[nm]] <- NULL
  }

  if (!"merged" %in% names(x) || is.null(x$merged)) {
    x$merged <- list(
      peaks = x$datatable,
      features = x$feature.info,
      meta = x$sample.info
    )
  }
  x$merged <- normalize_stage_payload(x$merged)

  if (!"history" %in% names(x) || is.null(x$history)) {
    x$history <- list()
  }

  class(x) <- c("metpipe_datalist", "list")
  if (!is.null(stage)) {
    attr(x, "stage") <- stage
  }
  attr(x, "schema_version") <- metpipe_schema_version()

  x
}


resolve_stage_name <- function(stage) {
  key <- tolower(trimws(as.character(stage)[1]))
  aliases <- c(
    "imported" = "raw",
    "raw" = "raw",
    "cleaned" = "cleaned",
    "normalized" = "norm",
    "norm" = "norm",
    "merged" = "merged"
  )
  out <- unname(aliases[[key]])
  if (is.null(out) || !nzchar(out)) {
    stop("Invalid stage: ", stage, ". Allowed: raw, cleaned, norm, merged")
  }
  out
}


#' Set a stage snapshot in `metpipe_datalist`
#'
#' Stores a stage payload (`peaks`, `features`, `meta`) for mode-level stages
#' (`raw`, `cleaned`, `norm`) or merged stage (`merged`). Existing flat fields
#' are kept in sync for backward compatibility.
#'
#' @param x A `metpipe_datalist` (or compatible list).
#' @param stage Stage name: `raw`, `cleaned`, `norm`, or `merged`.
#' @param peaks Peaks matrix/data.frame/list for the stage.
#' @param features Feature metadata data.frame for the stage.
#' @param meta Sample metadata data.frame for the stage.
#' @param mode Mode name (`"pos"` or `"neg"`) for non-merged stages.
#' @param validate Logical; validate inserted payload before returning.
#'
#' @return Updated `metpipe_datalist`.
#' @export
set_stage <- function(
    x,
    stage = c("raw", "cleaned", "norm", "merged"),
    peaks = NULL,
    features = NULL,
    meta = NULL,
    mode = c("pos", "neg"),
    validate = TRUE
) {
  stage_name <- resolve_stage_name(stage)
  x <- as_metpipe_datalist(x, validate = FALSE)

  payload <- normalize_stage_payload(list(peaks = peaks, features = features, meta = meta))

  if (identical(stage_name, "merged")) {
    x$merged <- payload
    x$datatable <- payload$peaks
    x$feature.info <- payload$features
    x$sample.info <- payload$meta
  } else {
    mode <- match.arg(mode)
    mode_obj <- normalize_mode_schema(x[[mode]])
    if (is.null(mode_obj$stages) || !is.list(mode_obj$stages)) {
      mode_obj$stages <- list()
    }
    mode_obj$stages[[stage_name]] <- payload

    if (identical(stage_name, "raw")) {
      mode_obj$peaks <- payload$peaks
      mode_obj$features <- payload$features
      mode_obj$meta <- payload$meta
    }
    if (identical(stage_name, "cleaned")) {
      mode_obj$peaks_cleaned <- payload$peaks
      mode_obj$features_cleaned <- payload$features
      mode_obj$meta_cleaned <- payload$meta
    }
    if (identical(stage_name, "norm")) {
      mode_obj$peaks_norm <- payload$peaks
      mode_obj$meta_norm <- payload$meta
      if (!is.null(payload$features)) {
        mode_obj$features_cleaned <- payload$features
      }
    }

    x[[mode]] <- mode_obj
  }

  if (isTRUE(validate)) {
    validate_stage(x, stage = stage_name, mode = if (identical(stage_name, "merged")) NULL else mode)
  }

  x$history <- c(x$history, list(list(
    event = "set_stage",
    stage = stage_name,
    mode = if (identical(stage_name, "merged")) NA_character_ else mode,
    timestamp = as.character(Sys.time())
  )))

  x
}


#' Get a stage snapshot from `metpipe_datalist`
#'
#' @param x A `metpipe_datalist` (or compatible list).
#' @param stage Stage name: `raw`, `cleaned`, `norm`, or `merged`.
#' @param mode Mode name (`"pos"` or `"neg"`) for non-merged stages.
#' @param required Logical; if `TRUE`, error when stage payload is unavailable.
#'
#' @return A list with `peaks`, `features`, and `meta`.
#' @export
get_stage <- function(
    x,
    stage = c("raw", "cleaned", "norm", "merged"),
    mode = c("pos", "neg"),
    required = TRUE
) {
  stage_name <- resolve_stage_name(stage)
  x <- as_metpipe_datalist(x, validate = FALSE)

  if (identical(stage_name, "merged")) {
    out <- x$merged
  } else {
    mode <- match.arg(mode)
    mode_obj <- normalize_mode_schema(x[[mode]])
    out <- mode_obj$stages[[stage_name]]
  }

  out <- normalize_stage_payload(out)

  if (isTRUE(required) && is.null(out$peaks) && is.null(out$features) && is.null(out$meta)) {
    if (identical(stage_name, "merged")) {
      stop("Stage payload not available: merged")
    }
    stop("Stage payload not available: ", stage_name, " (mode=", mode, ")")
  }

  out
}


#' Check whether a stage payload exists
#'
#' @param x A `metpipe_datalist` (or compatible list).
#' @param stage Stage name.
#' @param mode Mode name for non-merged stages.
#'
#' @return `TRUE` when any payload element is present, otherwise `FALSE`.
#' @export
has_stage <- function(
    x,
    stage = c("raw", "cleaned", "norm", "merged"),
    mode = c("pos", "neg")
) {
  stage_name <- resolve_stage_name(stage)
  payload <- tryCatch(
    get_stage(x, stage = stage_name, mode = mode, required = FALSE),
    error = function(e) NULL
  )
  if (is.null(payload)) return(FALSE)
  !is.null(payload$peaks) || !is.null(payload$features) || !is.null(payload$meta)
}


#' Validate a single stage payload
#'
#' @param x A `metpipe_datalist` (or compatible list).
#' @param stage Stage name.
#' @param mode Mode name for non-merged stages.
#'
#' @return `TRUE` invisibly when valid.
#' @export
validate_stage <- function(
    x,
    stage = c("raw", "cleaned", "norm", "merged"),
    mode = c("pos", "neg")
) {
  stage_name <- resolve_stage_name(stage)
  payload <- get_stage(x, stage = stage_name, mode = mode, required = TRUE)

  peaks <- payload$peaks
  features <- payload$features
  meta <- payload$meta

  if (!is.null(peaks) && !is.matrix(peaks) && !is.data.frame(peaks) && !is.list(peaks)) {
    stop("stage '", stage_name, "' peaks must be matrix/data.frame/list")
  }
  if (!is.null(features) && !is.data.frame(features)) {
    stop("stage '", stage_name, "' features must be a data.frame")
  }
  if (!is.null(meta) && !is.data.frame(meta)) {
    stop("stage '", stage_name, "' meta must be a data.frame")
  }

  if (!is.null(peaks) && (is.matrix(peaks) || is.data.frame(peaks)) && !is.null(meta)) {
    if (ncol(peaks) != nrow(meta)) {
      stop("stage '", stage_name, "' dimension mismatch: ncol(peaks) must equal nrow(meta)")
    }
  }

  if (!is.null(peaks) && (is.matrix(peaks) || is.data.frame(peaks)) && !is.null(features)) {
    if (nrow(peaks) != nrow(features)) {
      stop("stage '", stage_name, "' dimension mismatch: nrow(peaks) must equal nrow(features)")
    }
  }

  invisible(TRUE)
}


new_metpipe_datalist <- function(
    pos = NULL,
    neg = NULL,
    ...,
    validate = TRUE,
    stage = "imported"
) {
  x <- list(pos = pos, neg = neg, ...)
  x <- normalize_metpipe_datalist_schema(x, stage = stage)
  if (isTRUE(validate)) {
    validate_metpipe_datalist(x, stage = stage)
  }
  x
}


#' Check if object is a `metpipe_datalist`
#'
#' @param x Object to test.
#'
#' @return `TRUE` when `x` inherits from `metpipe_datalist`, otherwise `FALSE`.
#' @export
is_metpipe_datalist <- function(x) {
  inherits(x, "metpipe_datalist")
}


#' Check whether a mode exists in datalist
#'
#' @param x A `metpipe_datalist` (or compatible list).
#' @param mode Mode name: `"pos"` or `"neg"`.
#'
#' @return `TRUE` if requested mode is present and non-`NULL`, else `FALSE`.
#' @export
has_mode <- function(x, mode = c("pos", "neg")) {
  mode <- match.arg(mode)
  x <- as_metpipe_datalist(x, validate = FALSE)
  !is.null(x[[mode]])
}


#' Get a mode object from datalist
#'
#' @param x A `metpipe_datalist` (or compatible list).
#' @param mode Mode name: `"pos"` or `"neg"`.
#' @param required Logical; if `TRUE`, throw an error when mode is unavailable.
#'
#' @return Mode sub-list for `pos`/`neg`, or `NULL` when unavailable and
#'   `required = FALSE`.
#' @export
get_mode <- function(x, mode = c("pos", "neg"), required = TRUE) {
  mode <- match.arg(mode)
  x <- as_metpipe_datalist(x, validate = FALSE)
  out <- x[[mode]]
  if (isTRUE(required) && is.null(out)) {
    stop("datalist does not contain requested mode: ", mode)
  }
  out
}


#' Check whether merged outputs are present
#'
#' @param x A `metpipe_datalist` (or compatible list).
#'
#' @return `TRUE` when `datatable`, `sample.info`, and `feature.info` are all
#'   present, otherwise `FALSE`.
#' @export
is_merged <- function(x) {
  x <- as_metpipe_datalist(x, validate = FALSE)
  !is.null(x$datatable) && !is.null(x$sample.info) && !is.null(x$feature.info)
}


#' Coerce object to `metpipe_datalist`
#'
#' Converts an existing list-based datalist into the S3 class while preserving
#' all existing named entries.
#'
#' @param x A `metpipe_datalist` or list-like object.
#' @param stage Validation stage passed to [validate_metpipe_datalist()].
#' @param validate Logical; validate object structure after coercion.
#'
#' @return A `metpipe_datalist` object.
#' @export
as_metpipe_datalist <- function(
    x,
    stage = "imported",
    validate = TRUE
) {
  if (is_metpipe_datalist(x)) {
    x <- normalize_metpipe_datalist_schema(x, stage = stage)
    if (isTRUE(validate)) {
      validate_metpipe_datalist(x, stage = stage)
    }
    return(x)
  }

  if (!is.list(x)) {
    stop("datalist must be a list or metpipe_datalist")
  }

  x <- normalize_metpipe_datalist_schema(x, stage = stage)

  if (isTRUE(validate)) {
    validate_metpipe_datalist(x, stage = stage)
  }

  x
}


#' Validate a `metpipe_datalist`
#'
#' Performs light structural checks for workflow stage compatibility.
#'
#' @param x A `metpipe_datalist` or list-like object.
#' @param stage Validation stage. Supported: `"imported"`, `"cleaned"`,
#'   `"normalized"`, `"merged"`.
#'
#' @return The validated object, invisibly.
#' @export
validate_metpipe_datalist <- function(
    x,
    stage = c("imported", "cleaned", "normalized", "merged")
) {
  stage <- match.arg(stage)
  mode_names <- c("pos", "neg")

  x <- normalize_metpipe_datalist_schema(x, stage = stage)

  if (!is.list(x)) {
    stop("datalist must be a list or metpipe_datalist")
  }

  if (is.null(x$pos) && is.null(x$neg)) {
    stop("datalist does not contain pos or neg mode data")
  }

  validate_mode_shape <- function(mode_obj, mode_name) {
    required <- c("peaks", "features", "meta")
    missing <- required[vapply(required, function(nm) is.null(mode_obj[[nm]]), logical(1))]
    if (length(missing) > 0) {
      stop("datalist$", mode_name, " is missing required fields: ", paste(missing, collapse = ", "))
    }

    if (!is.null(mode_obj$peaks) && !is.matrix(mode_obj$peaks) && !is.data.frame(mode_obj$peaks)) {
      stop("datalist$", mode_name, "$peaks must be a matrix or data.frame")
    }
    if (!is.null(mode_obj$meta) && !is.data.frame(mode_obj$meta)) {
      stop("datalist$", mode_name, "$meta must be a data.frame")
    }

    if (!is.null(mode_obj$peaks) && !is.null(mode_obj$meta) && ncol(mode_obj$peaks) != nrow(mode_obj$meta)) {
      stop("datalist$", mode_name, " has inconsistent dimensions: ncol(peaks) must equal nrow(meta)")
    }

    if (!is.null(mode_obj$features) && !is.data.frame(mode_obj$features)) {
      stop("datalist$", mode_name, "$features must be a data.frame")
    }

    if (!is.null(mode_obj$peaks) && !is.null(mode_obj$features) && nrow(mode_obj$peaks) != nrow(mode_obj$features)) {
      stop("datalist$", mode_name, " has inconsistent dimensions: nrow(peaks) must equal nrow(features)")
    }
  }

  if (stage %in% c("imported", "cleaned", "normalized", "merged")) {
    for (mode_name in mode_names) {
      mode_obj <- x[[mode_name]]
      if (!is.null(mode_obj)) {
        if (!is.list(mode_obj)) {
          stop("datalist$", mode_name, " must be a list")
        }
        validate_mode_shape(mode_obj, mode_name)
      }
    }
  }

  if (stage == "normalized") {
    if (!is.null(x$pos) && is.null(x$pos$peaks_norm)) {
      stop("datalist$pos is missing peaks_norm for normalized stage")
    }
    if (!is.null(x$neg) && is.null(x$neg$peaks_norm)) {
      stop("datalist$neg is missing peaks_norm for normalized stage")
    }
  }

  if (stage == "merged") {
    required <- c("datatable", "sample.info", "feature.info")
    missing <- required[vapply(required, function(nm) is.null(x[[nm]]), logical(1))]
    if (length(missing) > 0) {
      stop("datalist is missing merged outputs: ", paste(missing, collapse = ", "))
    }
  }

  attr(x, "stage") <- stage

  invisible(x)
}


#' @export
print.metpipe_datalist <- function(x, ...) {
  has_pos <- !is.null(x$pos)
  has_neg <- !is.null(x$neg)
  has_merged <- !is.null(x$datatable) && !is.null(x$sample.info) && !is.null(x$feature.info)
  stage <- attr(x, "stage", exact = TRUE)

  cat("<metpipe_datalist>\n")
  cat("  modes: ", paste(c(if (has_pos) "pos", if (has_neg) "neg"), collapse = ", "), "\n", sep = "")
  if (!is.null(stage) && nzchar(stage)) {
    cat("  stage: ", stage, "\n", sep = "")
  }
  cat("  merged: ", if (has_merged) "yes" else "no", "\n", sep = "")
  invisible(x)
}


#' @export
summary.metpipe_datalist <- function(object, ...) {
  has_pos <- !is.null(object$pos)
  has_neg <- !is.null(object$neg)
  has_merged <- !is.null(object$datatable) && !is.null(object$sample.info) && !is.null(object$feature.info)

  out <- list(
    class = "metpipe_datalist",
    has_pos = has_pos,
    has_neg = has_neg,
    stage = attr(object, "stage", exact = TRUE),
    has_merged = has_merged,
    n_samples = if (has_merged) nrow(object$sample.info) else NA_integer_,
    n_features = if (has_merged) nrow(object$feature.info) else NA_integer_
  )
  class(out) <- "summary.metpipe_datalist"
  out
}


#' @export
print.summary.metpipe_datalist <- function(x, ...) {
  cat("<summary.metpipe_datalist>\n")
  cat("  stage: ", ifelse(is.null(x$stage) || !nzchar(x$stage), "unknown", x$stage), "\n", sep = "")
  cat("  modes: ", paste(c(if (isTRUE(x$has_pos)) "pos", if (isTRUE(x$has_neg)) "neg"), collapse = ", "), "\n", sep = "")
  cat("  merged: ", ifelse(isTRUE(x$has_merged), "yes", "no"), "\n", sep = "")
  cat("  n_samples: ", ifelse(is.na(x$n_samples), "NA", as.character(x$n_samples)), "\n", sep = "")
  cat("  n_features: ", ifelse(is.na(x$n_features), "NA", as.character(x$n_features)), "\n", sep = "")
  invisible(x)
}
