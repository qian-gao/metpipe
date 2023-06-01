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
             lip_names = NULL,
             met_names = NULL,
             mz_col_nr = 1,
             rt_col_nr = 2,
             identity_col_nr = 3,
             library_file = NULL
  ){

    if (!is.null(files)){
      feature.info <-
        files[, c(mz_col_nr, rt_col_nr, identity_col_nr)] %>%
        dplyr::rename( mz = 1,
                       rt = 2,
                       Identity = 3) %>%
        mutate(Identity = if_else( grepl(": 0.", Identity, fixed = TRUE),
                                   substr(Identity, 1, regexpr("\\:[^\\:]*$", Identity)-1),
                                   Identity),
               Library.name = Identity,
               Identity = gsub("##", "", Identity),
               Identity = if_else(is.na(Identity),
                                  paste0(round(mz, 3), "_", round(rt, 3)),
                                  Identity),
               feature.type = if_else(substr(Identity, 3, 3) == ".", "IS", ""),
               feature.type = if_else(substr(Identity, 4, 4) == ".", "IS", feature.type),
               Feature.id = row_number()) %>% ###!!!!! fix in the future
        group_by(Identity) %>%
        mutate( n = n(),
                Identity = ifelse( n > 1,
                                   paste0(Identity, "_iso", row_number()),
                                   Identity),
                Identity = stringi::stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", Identity)),
                m_name = make.names(Identity) ) %>%
        ungroup() %>%
        select( -n) %>%
        as.data.frame()

    } else if (!is.null(lip_names)) {

      feature.info <-
        data.frame( Identity = lip_names) %>%
        mutate(Identity = if_else( grepl(": 0.", Identity, fixed = TRUE),
                                   substr(Identity, 1, regexpr("\\:[^\\:]*$", Identity)-1),
                                   Identity),
               Name = Identity ) %>%
        group_by( Name ) %>%
        mutate( n = row_number(),
                m = max(row_number()),
                Name = ifelse( m > 1,
                               paste0( Name, "_iso", row_number()),
                               Name),
                m_name = make.names(Name)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(Rep =  strsplit(Name, "_iso", fixed = TRUE)[[1]][2],
               temp = strsplit(Name, "_iso", fixed = TRUE)[[1]][1],
               elements = case_when( grepl("O-", temp) ~ strsplit( temp, "[-:;]" ),
                                     TRUE              ~ strsplit( temp, "[ :;]" ) ),
               Class = elements[1],
               N.carbons = as.numeric( elements[2] ),
               N.double.bonds = as.numeric( elements[3] ) ) %>%
        select(-c(n, m, temp)) %>%
        ungroup()

    } else if (!is.null(met_names)) {

      feature.info <-
        data.frame( Identity = met_names ) %>%
        mutate(Identity = if_else( grepl(": 0.", Identity, fixed = TRUE),
                                   substr(Identity, 1, regexpr("\\:[^\\:]*$", Identity)-1),
                                   Identity),
               Name = Identity ) %>%
        group_by( Name ) %>%
        mutate( n = row_number(),
                m = max(row_number()),
                Name = ifelse( m > 1,
                               paste0( Name, "_iso", row_number()),
                               Name),
                m_name = make.names(Name)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(Rep =  strsplit(Name, "_iso", fixed = TRUE)[[1]][2]) %>%
        select(-c(n, m)) %>%
        ungroup()

    }

    if ( !is.null(library_file) ){

      feature.info <-
        feature.info %>%
        left_join( library_file[, c("Library.name", "Compound.name", "Ion", "Molecular.formula",
                                    "KEGG.ID.CSID", "HMDB.YMDB.ID", "METLIN.ID", "PC.CID",
                                    "Library.rt", "Library.mz")],
                   by = "Library.name")
    }

    return(feature.info)
  }
