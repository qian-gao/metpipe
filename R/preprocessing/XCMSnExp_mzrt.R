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
