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
new_metpipe_datalist <- function(
    pos = NULL,
    neg = NULL,
    ...,
    validate = TRUE,
    stage = "imported"
) {
  x <- list(pos = pos, neg = neg, ...)
  class(x) <- c("metpipe_datalist", "list")
  attr(x, "stage") <- stage
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
    attr(x, "stage") <- stage
    if (isTRUE(validate)) {
      validate_metpipe_datalist(x, stage = stage)
    }
    return(x)
  }

  if (!is.list(x)) {
    stop("datalist must be a list or metpipe_datalist")
  }

  if (is.null(names(x))) {
    stop("datalist must be a named list containing pos and/or neg")
  }

  if (!"pos" %in% names(x)) {
    x$pos <- NULL
  }
  if (!"neg" %in% names(x)) {
    x$neg <- NULL
  }

  class(x) <- c("metpipe_datalist", "list")
  attr(x, "stage") <- stage

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
