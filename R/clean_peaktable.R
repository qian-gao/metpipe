#' @title clean_peaktable
#'
#' @description Clean one mode-level peak table by removing outliers, filtering
#' features by quality metrics and missingness, and handling internal standards.
#'
#' @param datalist Mode-level list containing `peaks`, `features`, and `meta`.
#' @param mean.filter Optional mean-intensity filter rule(s).
#' @param rsd.filter Optional RSD filter rule(s).
#' @param dilution.filter Optional dilution-series filter configuration passed to [filter_peaks()].
#' @param rt.range Optional retention-time range.
#' @param filter_by_missing_feature_pct Missing-value threshold in percent.
#' @param filter_by_missing_sample_type Sample type used in missingness filtering.
#' @param outliers.sample Optional sample names removed before filtering.
#' @param po.sample.to.use Sample type used to select duplicate internal standards.
#'
#' @return Updated mode-level datalist with cleaned peaks/features/meta.
#' @examples
#' \dontrun{
#' cleaned <- clean_peaktable(datalist)
#' }
#' @export
#' @import dplyr
#' 
clean_peaktable <- 
  function(
    datalist = NULL,
    
    # Feature filtering
    mean.filter = NULL,
    rsd.filter = NULL,      
    dilution.filter = NULL,
    rt.range = NULL, 
    filter_by_missing_feature_pct = NULL,
    filter_by_missing_sample_type = "Sample",
    
    # Outliers
    outliers.sample = NULL,
    
    # Sample type to use to filter duplicate istd
    po.sample.to.use = NULL
    ){

    if (!is.list(datalist) || is.null(datalist$peaks) || is.null(datalist$features) || is.null(datalist$meta)) {
      stop("datalist must contain peaks, features, and meta")
    }

    if (!is.null(filter_by_missing_feature_pct) &&
        (filter_by_missing_feature_pct < 0 || filter_by_missing_feature_pct > 100)) {
      stop("filter_by_missing_feature_pct must be between 0 and 100")
    }
  
    peaks.all <- datalist$peaks
    features.all <- datalist$features
    meta <- datalist$meta
    
    # Remove outliers
    index <- !meta$Sample %in% outliers.sample
    peaks.all <- peaks.all[, index]
    meta <- meta[index, ]
    cat("The following outliers have been removed:\n")
    cat(outliers.sample, "\n")
    
    data.all <- cbind(features.all[, c("rt", "mz", "Identity")], peaks.all)
    is.index <- features.all$Feature_type == "IS"
    
    peaks <- data.all[!is.index, , drop = FALSE]
    peaks.is <- data.all[is.index, , drop = FALSE]
    cat("Detected number of features:", nrow(peaks))

    peaks.is.filtered <- NULL
    features.is.filtered <- NULL
    
    # Filtering based on rsd, rt, mean
    mzrt.filtered <-
      filter_peaks( peaktable = peaks,
                    sample.type = meta$Sample.type,
                    rsd.filter = rsd.filter,
                    rt.range = rt.range,
                    mean.filter = mean.filter,
                    dilution.filter = dilution.filter)
    
    peaks.filtered <- 
      mzrt.filtered$peaktable
    
    features.filtered <- 
      cbind( features.all[match(peaks.filtered$Identity, features.all$Identity),], mzrt.filtered$summary[, -1])
    
    ## IS
    if (nrow(peaks.is) > 0){
      
      mzrt.is.filtered <-
        filter_peaks( peaktable = peaks.is,
                      sample.type = meta$Sample.type)
      
      peaks.is.filtered <- 
        mzrt.is.filtered$peaktable
      
      features.is.filtered <- 
        cbind( features.all[match(peaks.is.filtered$Identity, features.all$Identity),], mzrt.is.filtered$summary[, -1])
      
    }
    
    # Filter based on missings
    sample.index <- meta$Sample.type == filter_by_missing_sample_type
    
    feature_filter <- 
      filter_by_missing( subset(peaks.filtered, select = -c(rt, mz, Identity))[, sample.index], 
                         method = 'feature',                        #feature or sample
                         threshold = filter_by_missing_feature_pct  #Percentage
      )
    
    peaks.filtered <- peaks.filtered[feature_filter$index, -c(1:3)]
    peaks.filtered[, sample.index] <- feature_filter$x
    
    features.filtered <- 
      features.filtered[feature_filter$index, , drop = FALSE]
    
    ## IS
    if (nrow(peaks.is) > 0){
      sample.index <- meta$Sample.type == filter_by_missing_sample_type
      
      cat("IS: ")
      feature_filter.is <- 
        filter_by_missing( subset(peaks.is.filtered, select = -c(rt, mz, Identity))[, sample.index], 
                           method = 'feature',                        #feature or sample
                           threshold = filter_by_missing_feature_pct  #Percentage
        )
      
      peaks.is.filtered <- peaks.is.filtered[feature_filter.is$index, -c(1:3)]
      peaks.is.filtered[, sample.index] <- feature_filter.is$x
      
      features.is.filtered <- 
        features.is.filtered[feature_filter.is$index, , drop = FALSE]
    }
    
    # Remove duplicate internal standard
    if (!is.null(features.is.filtered) && nrow(features.is.filtered) > 0){
      
      features.is.keep <-
        features.is.filtered %>%
        rowwise() %>%
        mutate(Identity_new = strsplit(Identity, "-iso")[[1]][1]) %>%
        group_by(Identity_new) %>%
        arrange(Identity_new, desc(get(make.names(paste0("mean.", po.sample.to.use))))) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        select(-Identity_new)
      
      keep.index <- match(features.is.keep$Identity, features.is.filtered$Identity)
      
      peaks.is.filtered <- peaks.is.filtered[keep.index, , drop = FALSE]
      
      print( "The following internal standards have been removed:", quote = FALSE)
      cat(features.is.filtered$Identity[setdiff(1:nrow(features.is.filtered), keep.index)], sep = '\n')
      
      features.is.filtered <- 
        features.is.filtered[keep.index, , drop = FALSE]
      
    }
    
    if (is.null(peaks.is.filtered) || nrow(peaks.is.filtered) == 0) {
      peaks <- peaks.filtered
      features <- features.filtered
    } else {
      peaks <- rbind(peaks.filtered, peaks.is.filtered)
      features <- rbind(features.filtered, features.is.filtered)
    }
    
    rownames(peaks) <- features$Feature_ID
    
    datalist$peaks <- peaks
    datalist$peaks_cleaned <- peaks
    datalist$features <- features
    datalist$features_cleaned <- features
    datalist$meta <- meta
    datalist$meta_cleaned <- meta
    
    return(datalist)
}

