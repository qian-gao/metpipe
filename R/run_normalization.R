#' Normalize data for positive/negative modes
#'
#' Runs [normalization()] for each available mode in a `datalist`, including
#' missing-value imputation and one or more normalization methods.
#'
#' @param datalist A `metpipe_datalist` (or compatible list) from import/cleaning steps.
#' @param impute.method.sample Imputation method for non-IS features.
#' @param impute.method.is Imputation method for internal standards.
#' @param po.sample.to.use Sample type used for pool/control-based normalization.
#' @param norm.method Character vector of normalization methods to apply.
#' @param sample.type.keep Sample types retained in the output normalized table.
#'
#' @return Updated `metpipe_datalist` with normalized outputs in `pos` and/or `neg`.
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

    datalist <- as_metpipe_datalist(datalist, stage = "imported")

    has_pos <- !is.null(datalist$pos)
    has_neg <- !is.null(datalist$neg)
    
    if (has_pos){
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
    
    if (has_neg){
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

    validate_metpipe_datalist(datalist, stage = "normalized")
    return(datalist)
  }
  