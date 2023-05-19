filter_by_missing <- 
  function(x, 
           method,    #feature or sample
           threshold, #Percentage
           verbose = FALSE
  ) {
    
    if ( verbose ) {
      
      print( "filter_by_missing was created by Qian Gao" )
      print( "qian.gao@sund.ku.dk" )
      print( "2021-06-03" )
      
    }
  
    if ( method == "feature" ) {
      
      missings <- apply( x, 2, function(x) { sum(is.na(x) | x==0) } )
      nsample <- nrow(x)
      feature.keep <- missings < nsample*threshold/100
      
      x.filter <- x[ , feature.keep]
      result <- list(x = x.filter, index = feature.keep)    
      
      print( paste0( "Only keep features having missings less than ", threshold, "% : ", 
                     sum(!result$index), " features have been removed" ),
             quote = FALSE)
      
    } else if ( method == "sample") {
      
      missings <- apply( x, 1, function(x) { sum(is.na(x) | x==0) } )
      nfeature <- ncol(x)
      sample.keep <- missings < nfeature*threshold/100
      
      x.filter <- x[ sample.keep, ]
      result <- list(x = x.filter, index = sample.keep)   
      
      print( paste0( "Only keep sampless having missings less than ", threshold, "% : ", 
                     sum(!result$index), " samples have been removed" ),
             quote = FALSE)
      
    }
    
    result$method <- paste0(method, "_", threshold, "%")
    return(result)
}