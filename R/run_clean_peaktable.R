#' run_clean_peaktable
#'
#' @param datalist 
#' @param mean.filter 
#' @param rsd.filter 
#' @param rt.range 
#' @param filter_by_missing_feature_pct 
#' @param outliers.sample 
#' @param po.sample.to.use 
#'
#' @return
#' @export
#'
#' @examples
run_clean_peaktable <- 
  function(
    datalist = NULL,
    
    # Feature filtering
    mean.filter = NULL,
    rsd.filter = NULL,      
    rt.range = NULL, 
    filter_by_missing_feature_pct = NULL,
    
    # Outliers
    outliers.sample = NULL,
    
    # Sample type to use to filter duplicate istd
    po.sample.to.use = NULL
    ){
    
    if (!is.null(datalist$pos)){
      cat("Positive mode: \n")
      datalist$pos <- 
        clean_peaktable(
          datalist = datalist$pos,
          mean.filter = mean.filter,
          rsd.filter = rsd.filter,      
          rt.range = rt.range, 
          filter_by_missing_feature_pct = filter_by_missing_feature_pct,
          outliers.sample = outliers.sample,
          po.sample.to.use = po.sample.to.use
        )
    }

    if (!is.null(datalist$neg)){
      cat("Negative mode: \n")
      datalist$neg <- 
        clean_peaktable(
          datalist = datalist$neg,
          mean.filter = mean.filter,
          rsd.filter = rsd.filter,      
          rt.range = rt.range, 
          filter_by_missing_feature_pct = filter_by_missing_feature_pct,
          outliers.sample = outliers.sample,
          po.sample.to.use = po.sample.to.use
        )
    }
    
    return(datalist)
  }