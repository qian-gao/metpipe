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

  if (!is.list(x)) {
    stop("datalist must be a list or metpipe_datalist")
  }

  if (is.null(x$pos) && is.null(x$neg)) {
    stop("datalist does not contain pos or neg mode data")
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

  invisible(x)
}


#' @export
print.metpipe_datalist <- function(x, ...) {
  has_pos <- !is.null(x$pos)
  has_neg <- !is.null(x$neg)
  has_merged <- !is.null(x$datatable) && !is.null(x$sample.info) && !is.null(x$feature.info)

  cat("<metpipe_datalist>\n")
  cat("  modes: ", paste(c(if (has_pos) "pos", if (has_neg) "neg"), collapse = ", "), "\n", sep = "")
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
    has_merged = has_merged,
    n_samples = if (has_merged) nrow(object$sample.info) else NA_integer_,
    n_features = if (has_merged) nrow(object$feature.info) else NA_integer_
  )
  class(out) <- "summary.metpipe_datalist"
  out
}
