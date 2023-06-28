#' @title process_obiwarp
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param files = NULL,
#' @param para = NULL,
#' @param sample.info = NULL,
#' @param mode = NULL,
#' @param path.result = NULL
#' @param BPPARAM = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' @export
#' @import xcms
#'
process_obiwarp <-
  function( files = NULL,
            para = NULL,
            sample.info = NULL,
            mode = NULL,
            path.result = NULL,
            BPPARAM = NULL
  ){

    if ( file.exists(paste0(path.result, "XCMSset_", mode, ".rds")) ){

      xdata <-
        readRDS( paste0(path.result, "XCMSset_", mode, ".rds"))

    } else {

      raw_data <-
        MSnbase::readMSData( files = files,
                    pdata = new("NAnnotatedDataFrame", sample.info),
                    mode = "onDisk")

      cwp <- CentWaveParam( peakwidth       = c( para$min_peakwidth, para$max_peakwidth ),
                            ppm             = para$ppm,
                            noise           = para$noise,
                            snthresh        = para$snthresh,
                            mzdiff          = -0.001,
                            prefilter       = c( para$prefilter, para$value_of_prefilter ),
                            mzCenterFun     = "wMean",
                            integrate       = 2,
                            fitgauss        = TRUE,
                            verboseColumns  = FALSE )

      owp <- ObiwarpParam( binSize        = 1, ### Check later
                           response       = 1,
                           distFun        = "cor_opt",
                           gapInit        = 0,
                           gapExtend      = 0,
                           factorDiag     = 2,
                           factorGap      = 1,
                           localAlignment = FALSE)

      pdp <- PeakDensityParam( sampleGroups = sample.info$Sample.group,
                               bw           = para$bw,
                               minFraction  = 0.7,
                               minSamples   = 1,
                               maxFeatures  = para$max,
                               binSize      = 0.01 )

      gf <- FillChromPeaksParam( expandMz = 0,
                                 expandRt = 0,
                                 ppm = para$ppm )
      ### Preprocess

      MSnbase::setMSnbaseFastLoad(FALSE)
      xdata1 <- findChromPeaks( raw_data, param = cwp, BPPARAM = BPPARAM )
      xdata2 <- adjustRtime( xdata1, param = owp )
      xdata3 <- groupChromPeaks( xdata2, param = pdp )
      xdata <- fillChromPeaks( xdata3, param = gf, BPPARAM = BPPARAM )

      saveRDS( xdata1, xdata2, xdata3, xdata,
               file = paste0(path.result, "XCMSset_all_", mode, ".rds") )

      saveRDS( xdata,
               file = paste0(path.result, "XCMSset_", mode, ".rds") )
    }

    return(xdata)

  }
