#' @title filter_by_missing
#'
#' @description Filter rows or columns by missing-value percentage.
#'
#' @param x Input numeric data frame/matrix with missing values.
#' @param method Filter mode: `"sample"` (filter columns) or `"feature"` (filter rows).
#' @param threshold Maximum allowed missing percentage (0-100).
#'
#' @return A list with filtered data (`x`), kept index (`index`), and `method` label.
#' @examples
#' filter_by_missing(matrix(c(1, NA, 3, 4), nrow = 2), "feature", 50)
#'
#' @export

filter_by_missing <-
  function(x,
           method,    #feature or sample
           threshold  #Percentage
  ) {

    if (!is.data.frame(x) && !is.matrix(x)) {
      stop("x must be a data.frame or matrix")
    }
    if (!method %in% c("sample", "feature")) {
      stop("method must be either 'sample' or 'feature'")
    }
    if (!is.numeric(threshold) || length(threshold) != 1 || threshold < 0 || threshold > 100) {
      stop("threshold must be a single number between 0 and 100")
    }

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
