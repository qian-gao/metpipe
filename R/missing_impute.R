#' @title missing_impute
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param datalist = NULL,
#' @param missing.impute.method.sample = NULL,
#' @param missing.impute.method.is = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
#' @export
#'
missing_impute <-
  function( datalist = NULL,
            missing.impute.method.sample = NULL,
            missing.impute.method.is = NULL){

    if (!is.null(missing.impute.method.sample)){

      print("For samples:", quote = FALSE)

      data.impute <-
        impute_various_methods( datalist$data,
                                method = missing.impute.method.sample
        )

      datalist$data <- data.impute$x

      datalist$pre.method["missing imputation sample", "method"] <- data.impute$method
    }

    if (!is.null(missing.impute.method.is)){

      print("For internal standards:", quote = FALSE)

      data.impute <-
        impute_various_methods( datalist$data.istd,
                                method = missing.impute.method.is
        )

      datalist$data.istd <- data.impute$x

      datalist$pre.method["missing imputation IS", "method"] <- data.impute$method

    }

    return(datalist)

  }
