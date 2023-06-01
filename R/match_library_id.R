#' @title match_library_id
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
#' @importFrom dplyr "%>%" left_join


### Function
match_library_id <-
  function( peaklist = NULL,
            lib = NULL){

  sheets <- openxlsx::getSheetNames(peaklist)

  match.list <- list()
  if ("POS" %in% sheets){

    input.pos <- openxlsx::read.xlsx(peaklist, sheet = "POS")
    pos <-
      input.pos %>%
        left_join(lib$pos, by = c("Identity" = "Library.name"))

    pos[is.na(pos)] <- ""
    match.list$pos <- pos

  } else {

    match.list$pos <- data.frame()

  }

  if ("NEG" %in% sheets){

    input.neg <- openxlsx::read.xlsx(peaklist, sheet = "NEG")
    neg <-
      input.neg %>%
      left_join(lib$neg, by = c("Identity" = "Library.name"))

    neg[is.na(neg)] <- ""
    match.list$neg <- neg

  } else {

    match.list$neg <- data.frame()

  }

  return(match.list)

  }




