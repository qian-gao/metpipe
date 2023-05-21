#' @title render_rmarkdown
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param file = NULL,
#' @param params = NULL,
#' @param output = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
#' @export

render_rmarkdown <-
  function(
    file = NULL,
    params = NULL,
    output = NULL

  ){

    rmarkdown::render( file,
                       params = params,
                       output_file = paste0( output, "_", Sys.Date(), ".html"))

   }
