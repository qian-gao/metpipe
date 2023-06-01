#' @title normalize_with_is
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
#' @export
#' @importFrom  dplyr "%>%" relocate
#'
normalize_with_is <-
  function(
    x,
    istds
  ) {

    istds.mean <-
      apply(istds, 1, function(x){ mean(x, na.rm = TRUE) })

    is.names <- unique(rownames(istds))

    x.norm <- list()
    length(x.norm) <- length(is.names)
    names(x.norm) <- is.names
    for (i in 1:length(is.names)){

      is.i <- istds[i, ]

      norm.i <-
        is.i[rep(rownames(is.i), nrow(x)), ]

      x.norm.i <-
        x / norm.i * istds.mean[i]

      x.norm[[i]] <-
        x.norm.i %>%
        tibble::rownames_to_column("Identity")

    }

    # Re-arrange
    x.norm.is <-
      data.table::rbindlist(x.norm, idcol = "Normalizer") %>%
      relocate(Identity, .before = Normalizer)

    #result <- split(x.norm.is, f = x.norm.is$Identity)
    result <- x.norm.is

    return( result )

  }
