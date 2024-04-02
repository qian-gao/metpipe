#' @title optim_para
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param files = NULL,
#' @param mode = NULL,
#' @param path.result = NULL
#' @param BPPARAM = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' @export
#'
#'
optim_para <-
  function( files = NULL,
            mode = NULL,
            path.result = NULL,
            BPPARAM = NULL
  ){

    #library(IPO)
    if ( file.exists(paste0(path.result, "XCMS_parameters_obiwarp_", mode, ".rds")) ){

      para <-
        readRDS(paste0(path.result, "XCMS_parameters_obiwarp_", mode, ".rds"))

    } else {

      ### Peak picking
      pp.para <-
        getDefaultXcmsSetStartingParams('centWave')

      pp.para$min_peakwidth <- c(0.1, 0.2)*60   # unit: seconds
      pp.para$max_peakwidth <- c(0.5, 0.8)*60   # unit: seconds
      pp.para$ppm <- 10                 # fluctuation of m/z value (ppm) from scan to scan
      pp.para$snthresh <- 10            # signal/noise ratio threshold
      pp.para$noise <- 500              # each centroid must be greater than the noise value
      pp.para$prefilter <- 3            # a peak must be present in k scans with an intensity greater than l
      pp.para$value_of_prefilter <- 500
      pp.para$mzdiff <- -0.001          # minimum difference in m/z of coeluting peaks(be negative to allow overlap)
      pp.para$integrate <- 2
      pp.para$mzCenterFun <- "wMean"
      pp.para$fitgauss <- TRUE

      result.pp <-
        optimizeXcmsSet( files = files,
                         params = pp.para,
                         plot = F,
                         subdir = NULL,
                         BPPARAM = BPPARAM )

      optim.obj <-
        result.pp$best_settings$xset

      ### Retention time correction and grouping
      rg.para <-
        getDefaultRetGroupStartingParams()

      rg.para$distFunc <- "cor_opt"
      rg.para$gapInit <- c(0.0, 0.4)
      rg.para$gapExtend <- c(2.1, 2.7)
      rg.para$profStep <- c(0.7, 1.0)
      rg.para$plottype <- "none"
      rg.para$response <- 1
      rg.para$factorDiag <- 2
      rg.para$factorGap <- 1
      rg.para$localAlignment <- 0

      rg.para$retcorMethod <- "obiwarp"
      rg.para$bw <- c(10, 30)
      rg.para$minfrac <- 0.7
      rg.para$mzwid <- 0.01
      rg.para$minsamp <- 1
      rg.para$max <- 20

      result.rg <-
        optimizeRetGroup( xset = optim.obj,
                          params = rg.para,
                          plot = F,
                          subdir = NULL )

      ### Save
      para <-
        c( result.pp$best_settings$parameters,
           result.rg$best_settings )


      saveRDS( para,
               file = paste0(path.result, "XCMS_parameters_obiwarp_", mode, ".rds") )
    }

    return(para)

  }
