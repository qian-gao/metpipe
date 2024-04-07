#' @title match_library
#'
#' @description Match features with library based on m/z and RT
#'     
#' @param feature_info A dataframe or xlsx of features with column mz and rt
#' @param rt_tolerance Retention time tolerance for matching, default is 0.1
#' @param mz_tolerance m/z tolerance for matching, default is 0.01
#' @param library_file A library file in dataframe or xlsx format. mz, rt and 
#'    compound names are in 1st, 2nd and 2rd column. 
#' @param mode Polarity, 'pos' or 'neg'
#' @param output.file Path and name for the output file
#'
#' @return A data frame contains featured annotated with library
#' @examples
#'
#' @export
#' @impor openxlsx
#' @importFrom dplyr "%>%" mutate left_join select
#'
# Usage
# feature_info = "Test_input.xlsx"
# library_file = "LCMS_library.xlsx"
#
# result <-
#   match_library( feature_info,
#                  rt_tolerance = 0.1,
#                  mz_tolerance = 0.01,
#                  library_file)

match_library <-
  function( feature_info = NULL, # contains mz and rt
            rt_tolerance = 0.1,
            mz_tolerance = 0.01,
            library_file = NULL,
            mode = NULL,
            output.file = NULL ) {

    if (!is.data.frame(feature_info)){
      x <-
        openxlsx::read.xlsx(feature_info, sheet = 1) %>%
        mutate(rt = as.numeric(rt),
               mz = as.numeric(mz),
               identifier = row_number())
    } else {
      x <-
        feature_info %>%
        mutate(rt = as.numeric(rt),
               mz = as.numeric(mz),
               identifier = row_number())
    }

    if (!is.data.frame(library_file)){
      y <-
        openxlsx::read.xlsx(library_file, sheet = 1) %>%
        dplyr::rename(Library.mz = 1, Library.rt = 2, Library.name = 3) %>%
        mutate(Library.rt = as.numeric(Library.rt),
               Library.mz = as.numeric(Library.mz))
    } else {
      y <-
        library_file %>%
        mutate(Library.rt = as.numeric(Library.rt),
               Library.mz = as.numeric(Library.mz))
    }

    rt_diff <- outer(x$rt, y$Library.rt, "-") %>% as.data.frame()
    mz_diff <- outer(x$mz, y$Library.mz, "-") %>% as.data.frame()

    match.ind <-
      abs(rt_diff) < rt_tolerance & abs(mz_diff) < mz_tolerance & !is.na(rt_diff) & !is.na(mz_diff)

    match.from.y <-
      data.table::rbindlist(
        apply( data.frame(id = 1:nrow(match.ind)),
               1,
               function(x) { y[match.ind[x, ], ] })
        , idcol = "identifier")

    result <-
      x %>%
        left_join(match.from.y, by = "identifier") %>%
        #select(-identifier) %>%
        group_by(Library.name) %>%
        mutate( Library.name = ifelse( is.na(Library.name),
                                     paste0("Unknown_", ifelse(is.null(mode), "", mode), "_", row_number()),
                                     Library.name) ) %>%
        ungroup() %>%
        mutate( mz_diff = ifelse(!is.na(mz) & !is.na(Library.mz), abs(mz - Library.mz), NA)) %>%
        group_by(identifier) %>%
        arrange(mz_diff) %>%
        filter( row_number() == 1) %>%
        ungroup() %>%
        select(-c(mz_diff, identifier))

    output <-
      result %>%
      dplyr::relocate(colnames(x)[4:(ncol(x)-1)], .after = last_col()) %>% 
      relocate(Library.name, .after = rt) 

    if (!is.null(output.file)){

      write.table( output, sep = ";",
                   file = output.file,
                   row.names = FALSE)

    }

    return(output)
  }
