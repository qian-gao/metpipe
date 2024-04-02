#' @title match_library_id
#'
#' @description Match features with library based on identity
#'
#' @param peaklist A xlsx of features with column "Identity
#' @param lib A library file in dataframe format. It must contain a column named 
#'      "Library.name"
#'
#' @return A data frame of features matched with library
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




