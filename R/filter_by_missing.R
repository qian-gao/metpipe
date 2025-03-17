#' @title filter_by_missing
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param x Input data frame with missing values
#' @param method Filter based on sample or feature, c("sample", "feature")
#' @param threshold Threshold for remove missings, percentage, e.g. 80
#'
#' @return A list containing a filtered dataframe and method used
#' @examples
#'
#' @export

filter_by_missing <-
  function(x,
           method,    #feature or sample
           threshold  #Percentage
  ) {

    if ( method == "sample" ) {

      missings <- apply( x, 2, function(x) { sum(is.na(x) | x==0) } )
      nsample <- nrow(x)
      feature.keep <- missings < nsample*threshold/100

      x.filter <- x[ , feature.keep, drop = FALSE]
      result <- list(x = x.filter, index = feature.keep)

      print( paste0( "Only keep samples having missings less than ", threshold, "% : ",
                     sum(!result$index), " samples have been removed" ),
             quote = FALSE)

    } else if ( method == "feature") {

      missings <- apply( x, 1, function(x) { sum(is.na(x) | x==0) } )
      nfeature <- ncol(x)
      sample.keep <- missings < nfeature*threshold/100

      x.filter <- x[ sample.keep, , drop = FALSE]
      result <- list(x = x.filter, index = sample.keep)

      print( paste0( "Only keep features having missings less than ", threshold, "% : ",
                     sum(!result$index), " features have been removed" ),
             quote = FALSE)

    }

    result$method <- paste0(method, "_", threshold, "%")
    return(result)
}
