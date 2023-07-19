#' @title prepare_msdiale_txt
#'
#' @description Prepare MSDIAL txt file
#'
#' @param target = NULL,
#' @param mode = NULL,
#' @param path_lib = NULL,
#' @param mz_tol = NULL,
#' @param mz_tol_bigger = NULL,
#' @param rt_tol = NULL,
#' @param rt_tol_bigger = NULL
#' @param align_ref = NULL
#' @param output_file = NULL
#'
#' @return A list of summaries for all types of samples
#'
#' @examples
#'
#' @export

prepare_msdial_txt <-
  function(
    target = NULL, # Metabolomics, Lipidomics
    mode = NULL,
    path_lib = NULL,
    path_msp = NULL,
    mz_tol = NULL,
    mz_tol_bigger = NULL,
    rt_tol = NULL,
    rt_tol_bigger = NULL,
    align_ref = NULL,
    output_file = NULL

  ){

    ##### MSDIAL settings #####

    params_msdial <- list()

    params_msdial$target=target
    params_msdial$path_lib=path_lib
    params_msdial$path_msp=path_msp

    params_msdial$mz_tol=mz_tol
    params_msdial$mz_tol_bigger=mz_tol_bigger
    params_msdial$rt_tol=rt_tol
    params_msdial$rt_tol_bigger=rt_tol_bigger

    if ( tolower(mode) %in% c('pos', 'positive')) {
      params_msdial$mode='Positive'
      params_msdial$ion <-
        paste( "[M+H]+", "[M+NH4]+", "[M+Na]+", "[M+K]+", "[M+H-H2O]+",
               collapse = "\n" )

    } else if (tolower(mode) %in% c('neg', 'negative')) {
      params_msdial$mode='Negative'
      params_msdial$ion <-
        paste( "[M-H]-", "[M+HCOOH-H]-",
               collapse = "\n" )

    }

    params_msdial$align_ref <- align_ref

    ### Generate MSDIAL txt file ###

    template <- readRDS(system.file("template", "msdial.template.rds", package="metpipe"))

    names(params_msdial) <- paste0("\\$\\{", names(params_msdial), "\\}")

    output <-
      data.frame(
        step = stringr::str_replace_all(template$para, unlist(params_msdial)) )

    write.table(output, output_file,
                row.names = FALSE, col.names = FALSE,
                quote = FALSE)

  }
