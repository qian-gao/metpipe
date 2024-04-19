#' @title compute_concentrations
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
#' @importFrom dplyr "%>%" left_join
#'
# Usage
# sep <- ";"
# mod <- m.pos
# raw.file <- "202111_Reduction_IS_dilution_test_data/Calibration_lip1_pos_05012022_targeted_automatic.csv"
#
# peak.concentrations <-
#   compute_concentrations( raw.file = "202111_Reduction_IS_dilution_test_data/Calibration_lip1_pos_05012022_targeted_automatic.csv",
#                           mod = m.pos)

compute_concentrations <-
  function( raw.file = NULL,
            mod = NULL){

    #library(tidyverse)

    if (!is.data.frame(raw.file)){

      raw <-
        openxlsx::read.xlsx(raw.file, sheet = 1)

    } else {

      raw <- raw.file

    }

    is.info <-
      raw[, 1:2] %>%
      dplyr::rename( Identity = 1,
              Normalizer = 2)

    peaks.raw <-
      raw[, 3:ncol(raw)]

    ids <- unique(is.info$Identity)
    ids_mod <- names(mod)

    ids_in <- ids[ids %in% ids_mod]


    predicted_x <- sapply( ids_in,
                           function(x){
                             x_interval <- range(mod[[x]]$model[, 2])
                             predict_x( y = as.numeric(peaks.raw[is.info$Identity == x, ]),
                                        mod = mod[[x]],
                                        x_interval = x_interval)
                           })

    rownames(predicted_x) <- names(peaks.raw)

    pred <-
      t(predicted_x) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Identity")

    result <-
      is.info %>%
      left_join(pred, by = "Identity") %>%
      as.data.frame()

    return(result)

  }

#' @title predict_x
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object
#' @examples
#'
predict_x <-
  function( y,
            mod,
            x_interval = NULL){

    pred_fun <-
      function(x, y, mod) {
        predict(mod, newdata = data.frame(Concentration = x)) - y
      }

    y.min <- min(mod$y)
    y.max <- max(mod$y)
    x_interval_initial <- x_interval

    xpred <-
      sapply( y,
              function(z){
                if (is.na(z) | z < y.min){
                  pred <- 0
                } else if (z <= y.max) {

                  pred <- NULL
                  attempt <- 1
                  x_interval<- x_interval_initial
                  x_interval_new <- x_interval_initial

                  while( is.null(pred) && attempt <= 5 ) {
                    attempt <- attempt + 1
                    x_interval <- x_interval_new
                    x_interval_new <- c(0, x_interval[2]*2)

                    try(
                      pred <- uniroot(pred_fun, interval = x_interval, mod = mod, y = z, tol = 0.0001)$root
                    )

                  }

                  if (is.null(pred)){ pred <- 0 }

                  return(pred)

                } else if (z > y.max) {

                  pred <- NULL
                  attempt <- 1
                  x_interval<- x_interval_initial
                  x_interval_new <- x_interval_initial

                  while( is.null(pred) && attempt <= 50000 ) {
                    attempt <- attempt + 1
                    x_interval <- x_interval_new
                    x_interval_new <- c(x_interval[2]/2, x_interval[2]*2)

                    try(
                      pred <- uniroot(pred_fun, interval = x_interval, mod = mod, y = z, tol = 0.0001)$root
                    )

                  }

                  if (is.null(pred)){ pred <- -99}

                  return(pred)

                }

              })

    return(xpred)

  }
