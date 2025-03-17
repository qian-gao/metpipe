#' @title clean_peaktable
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
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @importFrom dplyr
#' 
clean_peaktable <- 
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
  
    peaks.all <- datalist$peaks
    features.all <- datalist$features
    meta <- datalist$meta
    
    data.all <- cbind(features.all[, c("rt", "mz", "Identity")], peaks.all)
    is.index <- features.all$Feature_type == "IS"
    
    peaks <- data.all[!is.index, , drop = FALSE]
    peaks.is <- data.all[is.index, , drop = FALSE]
    
    # Filtering based on rsd, rt, mean
    mzrt.filtered <-
      filter_peaks( peaktable = peaks,
                    sample.type = meta$Sample.type,
                    rsd.filter = rsd.filter,
                    rt.range = rt.range,
                    mean.filter = mean.filter)
    
    peaks.filtered <- 
      mzrt.filtered$peaktable
    
    features.filtered <- 
      cbind( features.all[match(peaks.filtered$Identity, features.all$Identity),], mzrt.filtered$summary[, -1])
    
    ## IS
    if (nrow(peaks.is > 0)){
      
      mzrt.is.filtered <-
        filter_peaks( peaktable = peaks.is,
                      sample.type = meta$Sample.type)
      
      peaks.is.filtered <- 
        mzrt.is.filtered$peaktable
      
      features.is.filtered <- 
        cbind( features.all[match(peaks.is.filtered$Identity, features.all$Identity),], mzrt.is.filtered$summary[, -1])
      
    }
    
    # Filter based on missings
    sample.index <- meta$Sample.type == "Sample"
    
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
    if (nrow(peaks.is > 0)){
      sample.index <- meta$Sample.type == "Sample"
      
      cat("IS: ")
      feature_filter.is <- 
        filter_by_missing( subset(peaks.is.filtered, select = -c(rt, mz, Identity))[, sample.index], 
                           method = 'feature',                        #feature or sample
                           threshold = filter_by_missing_feature_pct  #Percentage
        )
      
      peaks.is.filtered <- peaks.is.filtered[feature_filter.is$index, -c(1:3)]
      peaks.is.filtered[, sample.index] <- feature_filter.is$x
      
      features.is.filtered <- 
        features.is.filtered[feature_filter.is$index, ]
    }
    
    # Remove duplicate internal standard
    if (nrow(peaks.is > 0)){
      
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
    
    # Remove outliers
    index <- !meta$Sample %in% outliers.sample
    peaks <- rbind(peaks.filtered, peaks.is.filtered)[, index]
    features <- rbind(features.filtered, features.is.filtered)
    meta <- meta[index, ]
    
    rownames(peaks) <- features$Feature_ID
    
    datalist$peaks <- peaks
    datalist$peaks_cleaned <- peaks
    datalist$features <- features
    datalist$features_cleaned <- features
    datalist$meta <- meta
    datalist$meta_cleaned <- meta
    
    return(datalist)
}

