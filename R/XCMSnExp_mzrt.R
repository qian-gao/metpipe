#' @title XCMSnExp_mzrt
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param XCMSnExp XCMS object?
#' @param method = "medret",
#' @param value = "into",
#' @param mzdigit = 4,
#' @param rtdigit = 1
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#'
#' @export

XCMSnExp_mzrt <-
  function ( XCMSnExp,
             method = "medret",
             value = "into",
             mzdigit = 4,
             rtdigit = 1
  ){

    data <- xcms::featureValues( XCMSnExp,
                                 value = value,
                                 missing = 0 )

    sample.info <- XCMSnExp@phenoData@data

    peaks <- xcms::featureDefinitions(XCMSnExp)

    mz <- peaks$mzmed
    rt <- peaks$rtmed
    mzrange <- peaks[, c("mzmin", "mzmax")]
    rtrange <- peaks[, c("rtmin", "rtmax")]

    rownames(data) <- paste0( "MZ", round(mz, mzdigit), "RT", round(rt, rtdigit))

    mzrt <- list( data = data,
                  sample.info = sample.info,
                  mz = mz,
                  rt = rt,
                  mzrange = mzrange,
                  rtrange = rtrange)

    class(mzrt) <- "mzrt"
    return(mzrt)
  }
