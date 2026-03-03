#' Clean peak tables for positive/negative modes
#'
#' Applies [clean_peaktable()] to each available mode in a `datalist`, using
#' shared filtering settings and mode-specific outlier options.
#'
#' @param datalist A `metpipe_datalist` (or compatible list) returned by [run_import_peaktable()].
#' @param mean.filter Optional mean-intensity filtering rule(s).
#' @param rsd.filter Optional RSD-based filtering rule(s).
#' @param rt.range Optional retention-time window for feature filtering.
#' @param filter_by_missing_feature_pct Optional missing-value threshold for features.
#' @param filter_by_missing_sample_type Sample type used for missing-value filtering.
#' @param outliers.sample Optional sample names removed in both modes.
#' @param outliers.sample.pos Optional positive-mode outlier sample names.
#' @param outliers.sample.neg Optional negative-mode outlier sample names.
#' @param po.sample.to.use Sample type used when filtering duplicate internal standards.
#'
#' @return Updated `metpipe_datalist` with cleaned `pos` and/or `neg` elements.
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
    filter_by_missing_sample_type = NULL,
    # Outliers
    outliers.sample = NULL,
    outliers.sample.pos = NULL,
    outliers.sample.neg = NULL,
    
    # Sample type to use to filter duplicate istd
    po.sample.to.use = NULL
    ){

    datalist <- as_metpipe_datalist(datalist, stage = "imported")

    has_pos <- has_mode(datalist, "pos")
    has_neg <- has_mode(datalist, "neg")
    
    if (has_pos){
      cat("Positive mode: \n")
      pos_data <- get_mode(datalist, "pos")
      
      if (is.null(outliers.sample.pos)) outliers.sample.pos <- outliers.sample
      datalist$pos <- 
        clean_peaktable(
          datalist = pos_data,
          mean.filter = mean.filter,
          rsd.filter = rsd.filter,      
          rt.range = rt.range, 
          filter_by_missing_feature_pct = filter_by_missing_feature_pct,
          filter_by_missing_sample_type = filter_by_missing_sample_type,
          outliers.sample = outliers.sample.pos,
          po.sample.to.use = po.sample.to.use
        )
    }

      if (has_neg){
      cat("Negative mode: \n")
      neg_data <- get_mode(datalist, "neg")

      if (is.null(outliers.sample.neg)) outliers.sample.neg <- outliers.sample
      datalist$neg <- 
        clean_peaktable(
          datalist = neg_data,
          mean.filter = mean.filter,
          rsd.filter = rsd.filter,      
          rt.range = rt.range, 
          filter_by_missing_feature_pct = filter_by_missing_feature_pct,
          filter_by_missing_sample_type = filter_by_missing_sample_type,
          outliers.sample = outliers.sample.neg,
          po.sample.to.use = po.sample.to.use
        )
    }
    
    validate_metpipe_datalist(datalist, stage = "cleaned")
    return(datalist)
  }