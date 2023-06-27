#' @title annotate_with_library
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param precursor = NULL,
#' @param method = NULL,
#' @param mode = NULL,
#' @param rt_tolerance = NULL,
#' @param mz_tolerance = NULL,
#' @param path.lib = NULL,
#' @param lib = NULL
#' @param path.result = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
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
            path.result = NULL

  ){

    if ( file.exists(paste0(path.result, "precursor_annotation_", mode, ".rds")) ){

      prec.lib <-
        readRDS( paste0(path.result, "precursor_annotation_", mode, ".rds"))

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

      saveRDS( prec.lib,
               file = paste0(path.result, "precursor_annotation_", mode, ".rds") )

      openxlsx::write.xlsx( prec.lib,
                            file = paste0(path.result, "precursor_annotation_", mode, ".xlsx") )

    }

    return(prec.lib)

  }
