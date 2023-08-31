#' @title match_library
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
      dplyr::relocate(colnames(result)[4:ncol(result)], .after = last_col())

    if (!is.null(output.file)){

      # saveRDS( output,
      #          file = gsub(".csv", ".rds", output.file))

      write.table( output, sep = ";",
                   file = output.file)

    }

    return(output)
  }
