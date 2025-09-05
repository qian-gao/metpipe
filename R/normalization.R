
#' normalization
#'
#' @param datalist 
#' @param impute.method.sample 
#' @param impute.method.is 
#' @param po.sample.to.use 
#' @param norm.method 
#' @param sample.type.keep 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
normalization <- 
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
    
    peaks.all <- datalist$peaks
    features.all <- datalist$features
    meta <- datalist$meta
    
    # Keep certain types of data
    type.keep <- c("Sample", sample.type.keep, po.sample.to.use)
    keep.index <- meta$Sample.type %in% type.keep
    
    peaks.all <- peaks.all[, keep.index, drop = FALSE]
    meta <- meta[keep.index, ,drop = FALSE]
    
    datalist$meta <- meta
    datalist$meta_norm <- meta
    
    is.index <- features.all$Feature_type == "IS"
    
    peaks <- peaks.all[!is.index, , drop = FALSE]
    peaks.is <- peaks.all[is.index, , drop = FALSE]
    features <- features.all[!is.index, , drop = FALSE]
    features.is <- features.all[is.index, , drop = FALSE]
    
    datalist$peaks <- peaks
    datalist$features <- features
    datalist$features_norm <- features
    
    # Missing imputation
    peaks <- 
      impute_missing(    
        t(peaks),
        method = impute.method.sample,
        k = 5,
        missing_thres = 0.2,
        group.info = NULL)$x
    
    colnames(peaks) <- features$Identity
    
    if (nrow(peaks.is) > 0){
      peaks.is <- 
        impute_missing(    
          t(peaks.is),
          method = impute.method.is,
          k = 5,
          missing_thres = 0.2,
          group.info = NULL)$x
      
      colnames(peaks.is) <- features.is$Identity
    }
    
    # Normalization
    data <- peaks
    data.istd <- peaks.is
    sample.info <- meta
    feature.info <- features
    
    peaks_norm <- list()
    normalizer.list <- list()
    peaks_norm$raw <- data
    
    print("The following methods were used for normalization:", quote = FALSE)
    print(norm.method)
    
    ### RSD original
    data.rsd.orig <-
      calculate_rsd( data = data,
                     type = sample.info$Sample.type,
                     names.suffix = "orig")$type.rsd
    
    names(data.rsd.orig)[2:ncol(data.rsd.orig)] <- paste0("rsd.", names(data.rsd.orig)[2:ncol(data.rsd.orig)]) 
    
    if (nrow(peaks.is) > 0){
      istd.rsd <-
        calculate_rsd( data = data.istd,
                       type = sample.info$Sample.type,
                       names.suffix = NULL)$type.rsd
      
      names(istd.rsd)[2:ncol(istd.rsd)] <- paste0("rsd.", names(istd.rsd)[2:ncol(istd.rsd)]) 
      
      datalist$istd.rsd <- istd.rsd
    }
    
    normalizer.list$raw <-
      data.rsd.orig %>%
      dplyr::left_join(feature.info, by = "Identity")
    
    # x.output <-
    #   cbind( Sample.name = sample.info$Sample,
    #          peaks_norm$raw)
    
    # wb <- createWorkbook()
    # addWorksheet(wb, "Normalized.data")
    # writeData(wb, "Normalized.data", x.output)
    # addWorksheet(wb, "Normalizer")
    # writeData(wb, "Normalizer", normalizer.output)
    # addWorksheet(wb, "Istd")
    # writeData(wb, "Istd", istd.rsd)
    # saveWorkbook(wb, file = paste0(path.result, prefix, "normalised_", "none", ".xlsx"), overwrite = TRUE )
    
    if ("bestis" %in% norm.method){
      
      result.normalization <-
        normalize_with_best_internal_standard(
          x = data,
          istds = data.istd,
          batch = sample.info$Batch,
          batch.wise = FALSE,
          type = sample.info$Sample.type,
          use.type = po.sample.to.use
        )
      
      peaks_norm$bestis <- result.normalization$x
      
      normalizer <-
        data.frame(Metabolite = feature.info$Identity,
                   Normalizer = result.normalization$best.istd)
      
      data.rsd <-
        calculate_rsd( data = peaks_norm$bestis,
                       type = sample.info$Sample.type,
                       names.suffix = NULL)$type.rsd
      
      istd.rsd <-
        calculate_rsd( data = data.istd,
                       type = sample.info$Sample.type,
                       names.suffix = NULL)$type.rsd
      
      normalizer.list$bestis <-
        normalizer %>%
        dplyr::left_join(data.rsd.orig, by = c("Metabolite" = "Identity")) %>%
        dplyr::left_join(data.rsd, by = c("Metabolite" = "Identity")) %>%
        dplyr::rename( Identity = Metabolite) %>%
        dplyr::left_join(feature.info, by = "Identity")
      
      rownames(peaks_norm$bestis) <- sample.info$Sample
      
      # x.output <-
      #   cbind( Sample.name = sample.info$Sample,
      #          peaks_norm$bestis)
      
      # wb <- createWorkbook()
      # addWorksheet(wb, "Normalized.data")
      # writeData(wb, "Normalized.data", x.output)
      # addWorksheet(wb, "Normalizer")
      # writeData(wb, "Normalizer", normalizer.output)
      # addWorksheet(wb, "Istd")
      # writeData(wb, "Istd", istd.rsd)
      # saveWorkbook(wb, file = paste0(path.result, prefix, "normalised_", "bestis", ".xlsx"), overwrite = TRUE )
      
    }
    
    if ("low_cv" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          istds = data.istd,
          batch = sample.info$Batch,
          batch.wise = FALSE,
          method = 'low_cv',
          type = sample.info$"Sample.type",
          use.type = po.sample.to.use,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$low_cv <- result.normalization$x
      normalizer.list$low_cv <- result.normalization$normalizer
    }
    
    if ("pqn" %in% norm.method){
      
      if ("Control" %in% sample.info$Sample.group){
        
        result.normalization <-
          normalize_various_methods(
            x = data,
            method = 'pqn',
            type = sample.info$Sample.type,
            reference = sample.info$Sample.group,
            use.reference = "Control",
            data.rsd.orig = data.rsd.orig,
            export.path = path.result,
            prefix = prefix,
            feature.info = feature.info
          )
        
      } else {
        
        result.normalization <-
          normalize_various_methods(
            x = data,
            method = 'pqn',
            type = sample.info$Sample.type,
            reference = sample.info$Sample.group,
            use.reference = po.sample.to.use,
            data.rsd.orig = data.rsd.orig,
            export.path = path.result,
            prefix = prefix,
            feature.info = feature.info
          )
        
      }
      
      peaks_norm$pqn <- result.normalization$x
      normalizer.list$pqn <- result.normalization$normalizer
      
    }
    
    if ("nomis" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          istds = data.istd,
          method = "nomis",
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$nomis <- result.normalization$x
      normalizer.list$nomis <- result.normalization$normalizer
      
    }
    
    if ("qspline" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'qspline',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$qspline <- result.normalization$x
      normalizer.list$qspline <- result.normalization$normalizer
    }
    
    if ("loess" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'loess',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$loess <- result.normalization$x
      normalizer.list$loess <- result.normalization$normalizer
    }
    
    if ("combat" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          batch = sample.info$Batch,
          method = "combat",
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$combat <- result.normalization$x
      normalizer.list$combat <- result.normalization$normalizer
    }
    
    if ("limma" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          batch = sample.info$Batch,
          method = "limma",
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$limma <- result.normalization$x
      normalizer.list$limma <- result.normalization$normalizer
    }
    
    if ("sum" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'sum',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$sum <- result.normalization$x
      normalizer.list$sum <- result.normalization$normalizer
    }
    
    if ("median" %in% norm.method){
      
      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'median',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )
      
      peaks_norm$median <- result.normalization$x
      normalizer.list$median <- result.normalization$normalizer
    }
    
    datalist$peaks_norm <- peaks_norm
    datalist$normalizer <- normalizer.list
    
    return(datalist)

}
