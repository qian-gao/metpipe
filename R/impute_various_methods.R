#' @title impute_various_methods
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param x Input data frame
#' @param method = NULL,
#' @param verbose = FALSE
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
#' @export

impute_various_methods <-
  function(
    x,
    method = NULL
  ) {

    missings_nr <- sum( is.na(x) | x <= 0 )

    if (is.null(method)) {

      x_imputed <- x
      method <- "no imputation"

    } else if (method == 'HF') {

      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- 0.5*min(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })

    } else if (method == 'LoD') {

      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- 0.2*min(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })

    } else if (method == 'median') {

      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- median(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })

    } else if (method == 'min') {

      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- min(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })

    } else if (method == 'mean') {

      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- mean(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })

    }

    x_imputed <- data.frame(x_imputed)
    rownames(x_imputed) <- rownames(x)
    colnames(x_imputed) <- colnames(x)

    result <- list( x = x_imputed,
                    method = method)

    print(paste(missings_nr, "missing values are found and imputed using method: ", method))

    return( result )
    }

