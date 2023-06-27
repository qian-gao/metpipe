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
                            mzdiff          = para$mzdiff,
                            prefilter       = c( para$prefilter, para$value_of_prefilter ),
                            mzCenterFun     = para$mzCenterFun,
                            integrate       = para$integrate,
                            fitgauss        = para$fitgauss,
                            verboseColumns  = FALSE )

      owp <- ObiwarpParam( binSize        = para$profStep,
                           response       = para$response,
                           distFun        = para$distFunc,
                           gapInit        = para$gapInit,
                           gapExtend      = para$gapExtend,
                           factorDiag     = para$factorDiag,
                           factorGap      = para$factorGap,
                           localAlignment = ifelse( para$localAlignment==0,F,T ) )

      pdp <- PeakDensityParam( sampleGroups = sample.info$Sample.group,
                               bw           = para$bw,
                               minFraction  = para$minfrac,
                               minSamples   = para$minsamp,
                               maxFeatures  = para$max,
                               binSize      = para$mzwid )

      gf <- FillChromPeaksParam( expandMz = 0,
                                 expandRt = 0,
                                 ppm = para$ppm )
      ### Preprocess

      MSnbase::setMSnbaseFastLoad(FALSE)
      xdata1 <- findChromPeaks( raw_data, param = cwp, BPPARAM = BPPARAM )
      xdata2 <- adjustRtime( xdata, param = owp )
      xdata3 <- groupChromPeaks( xdata, param = pdp )
      xdata <- fillChromPeaks( xdata, param = gf, BPPARAM = BPPARAM )

      saveRDS( xdata1, xdata2, xdata3, xdata,
               file = paste0(path.result, "XCMSset_all_", mode, ".rds") )

      saveRDS( xdata,
               file = paste0(path.result, "XCMSset_", mode, ".rds") )
    }

    return(xdata)

  }
