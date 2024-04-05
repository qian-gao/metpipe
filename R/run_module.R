#' @title run_module
#'
#' @description Wrapper function to run modules
#'
#' @param module Module name to run
#' @param params List of parameters to use
#'
#' @return Rmarkdown files and related output files
#' @examples
#' @export

run_module <- 
  function(module = NULL,
           params = NULL) {
    
    
    if (tolower(module) == "preprocessing_is"){
      params$preprocessing.is <- TRUE
      params$precursor.include <- FALSE
      params$untar <- FALSE
      
      if (is.null(params$mzmine.min.peaks.in.row.pos)){
        params$mzmine.min.peaks.in.row.pos <- floor(nrow(params$sample.info.pos) / 2)
        params$mzmine.min.peaks.in.row.pos.percent <- 0.5
      }
      if (is.null(params$mzmine.min.peaks.in.row.neg)){
        params$mzmine.min.peaks.in.row.neg <- floor(nrow(params$sample.info.neg) / 2)
        params$mzmine.min.peaks.in.row.neg.percent <- 0.5
      }
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_mzmine.Rmd", package="metpipe"),
        #file = "Preprocessing_mzmine.Rmd",
        params = params,
        output = paste0( params$path.result, '01_01_Preprocessing_IS')
        )

    } else if (tolower(module) == "qc_is"){
      params$sample.info.pos.qc <- NULL
      params$sample.info.neg.qc <- NULL
      
      render_rmarkdown(
        file = system.file("rmd", "QC_IS.Rmd", package="metpipe"),
        #file = "QC_IS.Rmd",
        params = params,
        output = paste0( params$path.result, '01_02_QC_IS')
        )
        
    } else if (tolower(module) == "targeted"){
      params$precursor.include <- FALSE
      params$preprocessing.is <- FALSE
      params$untar <- FALSE
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_mzmine.Rmd", package="metpipe"),
        #file = "Preprocessing_mzmine.Rmd",
        params = params,
        output = paste0( params$path.result, '02_Targeted_preprocessing')
        )
      
    } else if (tolower(module) == "untargeted_xcms"){
      params$extract_precursor <- FALSE
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_xcms.Rmd", package="metpipe"),
        #file = "Preprocessing_xcms.Rmd",
        params = params,
        output = paste0( params$path.result, '02_Untargeted_preprocessing_xcms')
      )
      
    } else if (tolower(module) == "untargeted_mzmine"){
      params$precursor.include <- FALSE
      params$preprocessing.is <- FALSE
      params$untar <- TRUE
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_mzmine.Rmd", package="metpipe"),
        #file = "Preprocessing_mzmine.Rmd",
        params = params,
        output = paste0( params$path.result, '02_Untargeted_preprocessing_mzmine')
      )
      
    } else if (tolower(module) == "untargeted_msdial"){
      params$extract_precursor <- FALSE
      params$precursor.include <- FALSE
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_msdial.Rmd", package="metpipe"),
        #file = "Untargeted_preprocessing_msdial.Rmd",
        params = params,
        output = paste0( params$path.result, '02_Untargeted_preprocessing_msdial')
      )
      
    } else if (tolower(module) == "tar_untar_msdial"){
      params$extract_precursor <- TRUE
      params$precursor.include <- TRUE
      params$preprocessing.is <- FALSE
      params$untar <- FALSE
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_msdial.Rmd", package="metpipe"),
        #file = "Untargeted_preprocessing_msdial.Rmd",
        params = params,
        output = paste0( params$path.result, '02_01_Extract_precursor_msdial')
      )
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_mzmine.Rmd", package="metpipe"),
        #file = "Preprocessing_mzmine.Rmd",
        params = params,
        output = paste0( params$path.result, '02_02_Targeted_untargeted_preprocessing')
      )
      
    } else if (tolower(module) == "tar_untar_xcms"){
      params$extract_precursor <- TRUE
      params$precursor.include <- TRUE
      params$preprocessing.is <- FALSE
      params$untar <- FALSE
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_xcms.Rmd", package="metpipe"),
        #file = "Preprocessing_xcms.Rmd",
        params = params,
        output = paste0( params$path.result, '02_01_Extract_precursor_xcms')
      )
      
      render_rmarkdown(
        file = system.file("rmd", "Preprocessing_mzmine.Rmd", package="metpipe"),
        #file = "Preprocessing_mzmine.Rmd",
        params = params,
        output = paste0( params$path.result, '02_02_Targeted_untargeted_preprocessing')
      )
      
    } else if (tolower(module) == "clean"){
      render_rmarkdown(
        file = system.file("rmd", "Clean_peaktable.Rmd", package="metpipe"),
        #file = "Clean_peaktable.Rmd",
        params = params,
        output = paste0( params$path.result, '03_Clean_peaktable')
      )
      
    } else if (tolower(module) == "normalization"){
      render_rmarkdown(
        file = system.file("rmd", "Normalization_comparison.Rmd", package="metpipe"),
        #file = "Normalization_comparison.Rmd",
        params = params,
        output = paste0( params$path.result, '04_Normalization_comparison')
      )
      
    } else if (tolower(module) == "merge"){
      render_rmarkdown(
        file = system.file("rmd", "Merge_and_map_info.Rmd", package="metpipe"),
        #file = "Merge_and_map_info.Rmd",
        params = params,
        output = paste0( params$path.result, '05_Merge_and_map_info')
      )
      
    }
    
  }
    

    