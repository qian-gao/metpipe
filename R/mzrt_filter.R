mzrt_filter <-
  function( mzrt = NULL,
            index = NULL){
    
    obj.names <- names(mzrt)
    obj.names <- obj.names[ !grepl( "sample.info", obj.names ) ]
    
    for ( i in 1:length(obj.names) ){
      
      name <- obj.names[i]
        
      if ( !is.null(dim(mzrt[[name]])) ) mzrt[[name]] <- mzrt[[name]][index, ]
      else mzrt[[name]] <- mzrt[[name]][index]
        
      
    }
    
    return(mzrt)
    
  }