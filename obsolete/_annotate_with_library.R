#' @title annotate_with_library
#'
#' @description Wrapper function, annotate peaks based on m/z and RT in the library
#'
#' @param precursor = NULL,
#' @param method = NULL,
#' @param mode = NULL,
#' @param rt_tolerance = NULL,
#' @param mz_tolerance = NULL,
#' @param path.lib = NULL,
#' @param lib = NULL
#' @param output.file = NULL
#'
#' @return A data frame object
#' @examples
#' @export
#' @importFrom dplyr "%>%" mutate left_join arrange select
#'
annotate_with_library <-
  function( precursor = NULL,
            method = NULL,
            mode = NULL,
            rt_tolerance = NULL,
            mz_tolerance = NULL,
            path.lib = NULL,
            lib = NULL,
            output.file = NULL

  ){

    output.file.rds <- gsub(".csv", ".rds", output.file)
    if ( file.exists( output.file.rds )){

      prec.lib <-
        readRDS( output.file.rds )

    } else {

      if (!is.null(path.lib)){

        prec.lib <-
          precursor %>%
          match_library( feature_info = .,
                         rt_tolerance = rt_tolerance,
                         mz_tolerance = mz_tolerance,
                         library_file = path.lib) %>%
          mutate( Library.name = ifelse( is.na(Library.name),
                                         paste0("Unknown_", mode, "_", row_number()),
                                         Library.name) )

      } else if (is.data.frame(lib[[method]][[mode]])) {

        prec.lib <-
          precursor %>%
          match_library( feature_info = .,
                         rt_tolerance = rt_tolerance,
                         mz_tolerance = mz_tolerance,
                         library_file = lib[[method]][[mode]]) %>%
          mutate( Library.name = ifelse( is.na(Library.name),
                                         paste0("Unknown_", mode, "_", row_number()),
                                         Library.name) )

      } else {

        prec.lib <-
          precursor %>%
          match_library( feature_info = .,
                         rt_tolerance = rt_tolerance,
                         mz_tolerance = mz_tolerance,
                         library_file = lib[[method]][[mode]]) %>%
          mutate( Library.name = ifelse( is.na(Library.name),
                                         paste0("Unknown_", mode, "_", row_number()),
                                         Library.name) )

      }

      prec.lib <-
        prec.lib %>%
        dplyr::relocate(colnames(precursor)[4:ncol(precursor)], .after = last_col())

      saveRDS( prec.lib,
               file = output.file.rds )

      write.table( prec.lib, sep = ";",
                   file = output.file )

    }

    return(prec.lib)

  }
