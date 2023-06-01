#' @title prepare_datalist
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
#' @export
#'
prepare_datalist <-
  function( peaks = NULL,
            sample.info = NULL,
            sample.pattern = "",
            type.to.remove = c("Met", "CA")
  ){

    peaks.temp <-
      peaks %>%
      mutate( peaks.type = ifelse( substr(identity, 3, 3) == ".", "IS", ""))

    peaktable <-
      peaks.temp %>%
      filter(peaks.type != "IS") %>%
      select(-peaks.type) %>%
      arrange(identity)

    peaktable.IS <-
      peaks.temp %>%
      filter(peaks.type == "IS") %>%
      select(-peaks.type) %>%
      arrange(identity)

    feature.info <-
      extract_feature_info( files = peaktable,
                            mz_col_nr = 1,
                            rt_col_nr = 2,
                            identity_col_nr = 3)

    rownames(feature.info) <- feature.info$m_name

    sample.names <-
      paste0(sample.info$Sample.name, sample.pattern)

    data <-
      data.frame(t(peaktable[, sample.names]))

    rownames(data) <- sample.info$Sample.name
    colnames(data) <- feature.info$Identity

    feature.info.istd <-
      extract_feature_info( files = peaktable.IS,
                            mz_col_nr = 1,
                            rt_col_nr = 2,
                            identity_col_nr = 3)

    rownames(feature.info.istd) <- feature.info.istd$m_name

    data.istd <-
      data.frame(t(peaktable.IS[, sample.names]))

    rownames(data.istd) <- sample.info$Sample.name
    colnames(data.istd) <- feature.info.istd$Identity

    ### Remove certain sample type
    index <- !sample.info$Sample.type %in% type.to.remove
    data <- data[index, ]
    data.istd <- data.istd[index, ]
    sample.info <- sample.info[index, ]

    ### Set data structure

    pre.method <-
      data.frame(step = c("filter by missing sample",
                          "filter by missing feature",
                          "missing imputation sample",
                          "missing imputation IS",
                          "filter sample",
                          "filter feature",
                          "filter istd",
                          "normalization"),
                 method = "")

    datalist <-
      list( data.raw              = data,
            data.istd.raw         = data.istd,
            sample.info.raw       = sample.info,
            feature.info.raw      = feature.info,
            feature.info.istd.raw = feature.info.istd,
            data                  = data,
            data.istd             = data.istd,
            sample.info           = sample.info,
            feature.info          = feature.info,
            feature.info.istd     = feature.info.istd,
            pre.method            = pre.method)

    return(datalist)
  }
