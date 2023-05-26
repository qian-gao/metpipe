#' @title filter_sample_feature
#'
#' @description Filter samples or features from peaktable
#'
#' @param datalist A datalist object
#' @param missing.sample.thres Threshold for keeping samples. e.g. 30 only keep
#'    samples with < 30% missings
#' @param missing.feature.thres Threshold for keeping features. e.g. 30 only keep
#'    features with < 30% missings
#' @param filter.sample.remove Sample.id to remove
#' @param filter.feature.remove Feature.id to remove
#' @param filter.is.remove Feature.id to remove
#'
#' @return A filtered datalist
#' @examples
#'
#' @export
#'
filter_sample_feature <-
  function( datalist = NULL,
            missing.sample.thres = NULL,
            missing.feature.thres = NULL,
            filter.sample.remove = NULL,
            filter.feature.remove = NULL,
            filter.is.remove = NULL){

    data <- datalist$data
    data.istd <- datalist$data.istd
    sample.info <- datalist$sample.info
    feature.info <- datalist$feature.info
    feature.info.istd <- datalist$feature.info.istd
    pre.method <- datalist$pre.method

    # By sample
    if (!is.null(missing.sample.thres)){

      sample_filter <-
        filter_by_missing( data,
                           method = 'sample',    #feature or sample
                           threshold = missing.sample.thres        #Percentage
        )

      data <- sample_filter$x
      sample.info <- sample.info[sample_filter$index, ]
      data.istd <- data.istd[sample_filter$index, ]

      pre.method["filter by missing sample", "method"] <- sample_filter$method

    }

    # By feature
    if (!is.null(missing.feature.thres)){

      feature_filter <-
        filter_by_missing( data,
                           method = 'feature',   #feature or sample
                           threshold = missing.feature.thres        #Percentage
        )

      data <- feature_filter$x
      feature.info <- feature.info[feature_filter$index, , drop = FALSE]

      pre.method["filter by missing sample", "method"] <- sample_filter$method

    }

    ### Filter samples
    if (!is.null(filter.sample.remove)){

      sample.remove <- filter.sample.remove

      sample.keep <- !sample.info$Sample.id %in% sample.remove
      sample.out <- paste(sample.info$Sample.name[!sample.keep], collapse = ", ")

      data <- data[ sample.keep, ]
      data.istd <- data.istd[ sample.keep, , drop = FALSE]
      sample.info <- sample.info[ sample.keep, , drop = FALSE]

      pre.method["filter sample", "method"] <- sample.out

      print("The following samples are removed from data:", quote = FALSE)
      print(sample.out, quote = FALSE)
    }

    ### Filter features
    if (!is.null(filter.feature.remove)){

      feature.remove <- filter.feature.remove

      feature.keep <- !feature.info$Feature.id %in% feature.remove
      feature.out <- paste(feature.info$Identity[!feature.keep], collapse = ", ")

      data <- data[ , feature.keep ]
      feature.info <- feature.info[ feature.keep, , drop = FALSE]

      pre.method["filter feature", "method"] <- feature.out

      print("The following features are removed from data:", quote = FALSE)
      print(feature.out, quote = FALSE)
    }

    ### Filter istd
    if (!is.null(filter.is.remove)){

      feature.remove <- filter.is.remove

      feature.keep <- !feature.info.istd$Feature.id %in% feature.remove
      feature.out <- paste(feature.info.istd$Identity[!feature.keep], collapse = ", ")

      data.istd <- data.istd[ , feature.keep ]
      feature.info.istd <- feature.info.istd[ feature.keep, , drop = FALSE]

      pre.method["filter istd", "method"] <- feature.out

      print("The following internal standards are removed from data:", quote = FALSE)
      print(feature.out, quote = FALSE)
    }

    datalist$data <- data
    datalist$data.istd <- data.istd
    datalist$sample.info <- sample.info
    datalist$feature.info <- feature.info
    datalist$feature.info.istd <- feature.info.istd
    datalist$pre.method <- pre.method

    return(datalist)
  }
