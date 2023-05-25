#' @title extract_feature_info
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param files A dataframe of peaktable
#' @param mz_col_nr Column position of mz
#' @param rt_col_nr Column position of rt
#' @param identity_col_nr Column position of identity
#' @param library_file A dataframe of library
#'
#' @return A data frame object that contains feature info
#' @examples
#'
#' @export
#' @import dplyr
#'
extract_feature_info <-
  function ( files = NULL,
             mz_col_nr = 1,
             rt_col_nr = 2,
             identity_col_nr = 3,
             library_file = NULL
  ){


    feature.info <-
      files[, c(mz_col_nr, rt_col_nr, identity_col_nr)] %>%
      dplyr::rename( mz = 1,
                     rt = 2,
                     Identity = 3) %>%
      mutate(Identity = gsub("##", "", Identity),
             Identity = if_else(is.na(Identity),
                                paste0(round(mz, 3), "_", round(rt, 3)),
                                Identity),
             feature.type = if_else(substr(Identity, 3, 3) == ".", "IS", ""),
             feature.type = if_else(substr(Identity, 4, 4) == ".", "IS", feature.type),
             Feature.id = row_number()) %>% ###!!!!! fix in the future
      group_by(Identity) %>%
      mutate( n = n(),
              Identity = ifelse( n > 1,
                                 paste0(Identity, "-iso", row_number()),
                                 Identity),
              Identity = stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", Identity)),
              m_name = make.names(Identity) ) %>%
      ungroup() %>%
      select( -n) %>%
      as.data.frame()

    if ( !is.null(library_file) ){

      feature.info <-
        feature.info %>%
        left_join( library_file[, c("Library.name", "Compound.name", "Ion", "Molecular.formula",
                                    "KEGG.ID.CSID", "HMDB.YMDB.ID", "METLIN.ID", "PC.CID",
                                    "Library.rt", "Library.mz")],
                   by = c("Identity" = "Library.name"))
    }

    return(feature.info)
  }
