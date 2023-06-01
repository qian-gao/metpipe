#' @title extract_peak_table
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
#' @import dplyr


extract_peak_table <- function( x,
                                rt.col = "rt",
                                mz.col = "mz",
                                id.col = "Identity",
                                colsample.start = 8
                              ){

  ### Extract feature info

  feature <- data.frame(rt = x[ , rt.col],
                        mz = x[ , mz.col],
                        identity = x[ , id.col]) %>%
                mutate(identity = ifelse(is.na(identity),
                                       paste0("mz_", round(mz, 4), "_RT_", round(rt, 2)),
                                       identity),
                       Feature.id = row_number()) %>%
                group_by(identity) %>%
                mutate(Dup = row_number(),
                       Count = n()) %>%
                ungroup() %>%
                mutate(identity = ifelse(Count > 1, paste0(identity, "_", Dup), identity))

  ### Prepare data matrix for processing

  x.loaded <- as.matrix(t(x[ , colsample.start:ncol(x)]))

  rownames(x.loaded) <- colnames(x[ , colsample.start:ncol(x)])
  colnames(x.loaded) <- feature$identity

  output <- list(data = x.loaded, feature = feature)

  return(output)

}
