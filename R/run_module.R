run_module <- 
  function(module = NULL,
           params = NULL) {
    
    
    if (tolower(module) == "preprocessing_is"){
      params$preprocessing.is <- TRUE
      params$precursor.include <- FALSE
      
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
        output = paste0( params$path.result, '01_01_Preprocessing_IS'))

    } else if (tolower(module) == "qc_is"){
      params$sample.info.pos.qc <- NULL
      params$sample.info.neg.qc <- NULL
      
      render_rmarkdown(
        file = system.file("rmd", "QC_IS.Rmd", package="metpipe"),
        #file = "QC_IS.Rmd",
        params = params,
        output = paste0( params$path.result, '01_02_QC_IS'))
        
    }
    
  }
    

    