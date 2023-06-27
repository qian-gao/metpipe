#' @title process_peak_group
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param files = NULL,
#' @param para = NULL,
#' @param sample.info = NULL,
#' @param mode = NULL,
#' @param path.result = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' @export
#' @import xcms
#' 
process_peak_group <- 
  function( files = NULL,
            para = NULL,
            sample.info = NULL,
            mode = NULL,
            path.result = NULL
  ){
    
    if ( file.exists(paste0(path.result, "XCMSset_", mode, ".rds")) ){
      
      xdata <-
        readRDS( paste0(path.result, "XCMSset_", mode, ".rds"))
      
    } else {
      
      raw_data <- 
        readMSData( files = sample.info$File.path, 
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
      
      pdp.1 <- PeakDensityParam( sampleGroups = sample.info$Sample.group,
                                 bw           = para$bw,            # Standart deviation of the gaussian metapeak that group peaks together.
                                 minFraction  = para$minFraction.1, # To be valid, a group must be found in at least minFraction*n samples
                                 minSamples   = 1,                  # minimum number of samples in at least one sample group in which the peaks have to be detected to be considered a peak group (feature) 
                                 maxFeatures  = 20,                 # Maximum number of groups detected in a single m/z slices.
                                 binSize      = para$binSize        # Range of m/z to be included in a group
      )
      
      pgp <- PeakGroupsParam( smooth      = "loess",
                              span        = para$span,            # 0.4-0.7, increase the span if there're regions with few peaks.
                              minFraction = para$minFraction.ref, # Minimum proportion of samples with reference peaks
                              family      = "gaussian",
                              extraPeaks  = para$extraPeaks,      # Number of "extra" peaks used to define reference peaks
                              subset      = sample.info$Sample.type %>% grep("PO|Sample", .)
      )
      
      pdp.2 <- PeakDensityParam( sampleGroups = sample.info$Sample.group,
                                 bw           = para$bw,            # Standart deviation of the gaussian metapeak that group peaks together.
                                 minFraction  = para$minFraction.2, # To be valid, a group must be found in at least minFraction*n samples
                                 minSamples   = 1,                  # minimum number of samples in at least one sample group in which the peaks have to be detected to be considered a peak group (feature) 
                                 maxFeatures  = 20,                 # Maximum number of groups detected in a single m/z slices.
                                 binSize      = para$binSize        # Range of m/z to be included in a group
      ) 
      
      gf <- FillChromPeaksParam( expandMz = 0, 
                                 expandRt = 0, 
                                 ppm = para$ppm )
      
      ### Preprocess
      
      MSnbase::setMSnbaseFastLoad(FALSE)
      xdata1 <- findChromPeaks( raw_data, param = cwp, BPPARAM = BPPARAM_set )
      xdata2 <- groupChromPeaks( xdata1, param = pdp.1 )
      xdata3 <- adjustRtime( xdata2, param = pgp )
      xdata4 <- groupChromPeaks( xdata3, param = pdp.2 )
      xdata <- fillChromPeaks( xdata4, param = gf, BPPARAM = BPPARAM_set )
      
      save(xdata1, xdata2, xdata3, xdata4, xdata,
           file = paste0(path.result, "XCMSset_all_", mode, ".rds"))
      
      saveRDS( xdata, 
               file = paste0(path.result, "XCMSset_", mode, ".rds") )
      
    }
    
    return(xdata)
    
  }