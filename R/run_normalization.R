#' run_normalization
#'
#' @param datalist 
#' @param impute.method.sample 
#' @param impute.method.is 
#' @param po.sample.to.use 
#' @param norm.method 
#' @param sample.type.keep 
#'
#' @return
#' @export
#'
#' @examples
run_normalization <- 
  function(
    datalist = NULL,
    
    # Misssing imputation
    # "LoD", "HF", "median", "min", "mean"
    impute.method.sample = NULL,  
    impute.method.is = NULL,
    
    # Normalization method
    #c("bestis", "low_cv", "pqn", "loess", "sum", "median", "limma")
    po.sample.to.use = NULL,
    norm.method = NULL,
    
    # Sample types to keep in datatable
    sample.type.keep = "Sample"
  ){
    
    if (!is.null(datalist$pos)){
      cat("Positive mode: \n")
      datalist$pos <- 
        normalization(
          datalist = datalist$pos,
          impute.method.sample = impute.method.sample,  
          impute.method.is = impute.method.is,
          po.sample.to.use = po.sample.to.use,
          norm.method = norm.method,
          sample.type.keep = sample.type.keep
        )
    }
    
    if (!is.null(datalist$neg)){
      cat("Negative mode: \n")
      datalist$neg <- 
        normalization(
          datalist = datalist$neg,
          impute.method.sample = impute.method.sample,  
          impute.method.is = impute.method.is,
          po.sample.to.use = po.sample.to.use,
          norm.method = norm.method,
          sample.type.keep = sample.type.keep
        )
    }

    return(datalist)
  }
  