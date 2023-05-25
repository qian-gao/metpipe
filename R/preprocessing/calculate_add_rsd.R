calculate_add_rsd <-
  function( data = NULL, 
            type = NULL ){
    
    rsd <-
      calculate_rsd( data = data, 
                     type = type,
                     impute = TRUE)
    
    tm <- rsd$type.mean
    colnames(tm) <- paste0( "mean.", colnames(tm))
    trsd <- rsd$type.rsd
    colnames(trsd) <- paste0( "rsd.", colnames(trsd)) 
    
    summary <- 
      cbind(tm, trsd[, -1]) %>%
      dplyr::rename(Identity = mean.Identity)
    
    return(summary)
    
  }
