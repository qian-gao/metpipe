#' @title XCMSnExp_mzrt
#'
#' @description Extract m/z, rt from a XCMSnExp object
#'
#' @param XCMSnExp A XCMSnExp object
#' @param method XCMS featureValues parameter for feature values: specifying the 
#'    method to resolve multipeak mappings within the same sample. "medret", report 
#'    the value for the chromatographic peak closest to the feature's median 
#'    retention time.
#' @param value XCMS featureValues parameter: defining which value should be reported
#'    for each feature in each sample. Can be any column of the chromPeaks matrix.
#'    Defaults "into", the integrated peak area is reported
#'    
#' @param mzdigit Number of digits to keep for m/z
#' @param rtdigit Number of digits to keep for rt
#'
#' @return A list of m/z, rt
#'     
#' @export
#' @import xcms

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
