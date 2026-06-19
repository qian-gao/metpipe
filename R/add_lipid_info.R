#' @title add_lipid_info
#'
#' @description Parse lipid annotations and add summarized lipid identities.
#'
#' @param df Feature table containing `Identity_raw` and `Feature_type` columns.
#' @param keep.lipid.orig Optional character vector of identities to keep unchanged.
#'
#' @return Input feature table with additional lipid annotation columns,
#' including `Identity_sum`, `Class`, and elemental summary fields.
#' @examples
#' \dontrun{
#' annotated <- add_lipid_info(features)
#' }
#' @export
#' @import dplyr

add_lipid_info <- 
  function(df,
           keep.lipid.orig = NULL){

              if (!is.data.frame(df)) {
                     stop("df must be a data.frame")
              }
              required_cols <- c("Identity_raw", "Feature_type")
              if (!all(required_cols %in% names(df))) {
                     stop("df must contain columns: Identity_raw, Feature_type")
              }

    output <- 
      df %>% 
        mutate(Identity_sum = ifelse(Feature_type == "Known", gsub("low score: ", "", Identity_raw), NA)
               #istd = grepl("^[0-9][0-9]\\.", substr(Identity_raw, 1, 3))
               ) %>%
          rowwise() %>%
          mutate(Note.2 = trimws(str_split(Identity_sum, "[/_]")[[1]][2]),
                 Note.3 = trimws(str_split(Identity_sum, "[/_]")[[1]][3]),
                 Identity_sum = str_split(Identity_sum, "[/_]")[[1]][1],
                 Class = if_else(grepl(" (", Identity_sum, fixed = TRUE),
                                 str_split(Identity_sum, "[ (]",)[[1]][1],
                                 if_else(grepl("O-|P-", Identity_sum), 
                                         str_split(Identity_sum, "-")[[1]][1],
                                         str_split(Identity_sum, " ")[[1]][1])),
                 Identity_sum = if_else(grepl("O-|P-", Identity_sum), 
                                        str_split(Identity_sum, "-")[[1]][2],
                                        str_split(Identity_sum, " ")[[1]][2]),
                 N.carbons.1 = as.numeric(str_split(Identity_sum, ":")[[1]][1]),
                 N.double.bonds.1 = as.numeric(str_split(Identity_sum, "[:;-]")[[1]][2]),
                 N.O.1 = as.numeric(gsub("O", "", str_split(Identity_sum, ";")[[1]][2])),
                 N.O.1 = if_else(grepl("O", Identity_sum) & is.na(N.O.1), 1, N.O.1),
                 
                 N.carbons.2 = as.numeric(str_split(Note.2, ":")[[1]][1]),
                 N.double.bonds.2 = as.numeric(str_split(Note.2, "[:;]")[[1]][2]),
                 N.O.2 = as.numeric(gsub("O", "", str_split(Note.2, ";")[[1]][2])),
                 N.O.2 = if_else(grepl("O", Note.2) & is.na(N.O.2), 1, N.O.2),
                 
                 N.carbons.3 = as.numeric(str_split(Note.3, ":")[[1]][1]),
                 N.double.bonds.3 = as.numeric(str_split(Note.3, "[:;]")[[1]][2]),
                 N.O.3 = as.numeric(gsub("O", "", str_split(Note.3, ";")[[1]][2])),
                 N.O.3 = if_else(grepl("O", Note.3) & is.na(N.O.3), 1, N.O.3),
                 
                 N.carbons = sum(N.carbons.1, N.carbons.2, N.carbons.3, na.rm = TRUE),
                 N.double.bonds = sum(N.double.bonds.1, N.double.bonds.2, N.double.bonds.3, na.rm = TRUE),
                 N.O = sum(N.O.1, N.O.2, N.O.3, na.rm = TRUE),
                 
                 N.double.bonds = ifelse(is.na(N.double.bonds), 0, N.double.bonds),
                 
                 Identity_sum = ifelse(N.O == 0, 
                                       paste0(Class, " ", N.carbons, ":", N.double.bonds),
                                       paste0(Class, " ", N.carbons, ":", N.double.bonds, ";O", N.O)),
                 Identity_sum = if_else(Identity_raw %in% keep.lipid.orig,
                                        Identity_raw, 
                                        Identity_sum),
                 Identity_sum = if_else(grepl("-SN2", Identity_raw),
                                        paste0(Identity_sum, "-SN2"), 
                                        Identity_sum),
                 Identity_sum = if_else(grepl("O ", Identity_sum),
                                        gsub("O ", "O-", Identity_sum), 
                                        Identity_sum),
                 Identity_sum = if_else(grepl("P ", Identity_sum),
                                        gsub("P ", "P-", Identity_sum), 
                                        Identity_sum),
                 Identity_sum = if_else(Feature_type != "Known", Identity_raw, Identity_sum)
                 
          ) %>%
          dplyr::select(-c(N.carbons.1, N.carbons.2, N.carbons.3, N.double.bonds.1, N.double.bonds.2, N.double.bonds.3, 
                    N.O.1, N.O.2, N.O.3, Note.2, Note.3, N.O)) %>%
          ungroup() %>%
          group_by(Identity_sum) %>%
          mutate(Rep = row_number()
                 #Identity_sum = ifelse(Rep > 1 & Feature_type == "Known", paste0(Identity_sum, "_rep", Rep), Identity_sum)
                 ) %>%
          ungroup()
    
    return(output)
  }