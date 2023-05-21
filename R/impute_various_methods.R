impute_various_methods <-
  function(
    x,
    method = NULL,
    verbose = FALSE
  ) {
    
    if ( verbose ) {
      
      print( "impute_various_method was created by Qian Gao" )
      print( "qian.gao@sund.ku.dk" )
      print( "2021-06-03" )
      
    }

    missings_nr <- sum( is.na(x) | x <= 0 )
    
    if (method == 'HF') {
      
      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- 0.5*min(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })
      
    } else if (method == 'LoD') {
      
      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- 0.2*min(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })      

    } else if (method == 'median') {
      
      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- median(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })      
      
    } else if (method == 'min') {
      
      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- min(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })      
      
    } else if (method == 'mean') {
      
      x_imputed <- apply(x, 2, function(x){
                                 miss <- which( is.na(x) | x <= 0 )
                                 if (sum(miss) > 0) {
                                    x[ miss ] <- mean(x[-miss], na.rm = TRUE)
                                 }
                                 return(x)
                               })      
      
    }
    
    x_imputed <- data.frame(x_imputed)
    rownames(x_imputed) <- rownames(x)
    colnames(x_imputed) <- colnames(x)
    
    result <- list( x = x_imputed,
                    method = method)
    
    print(paste(missings_nr, "missing values are found and imputed using method ", method))
    
    return( result )
    }

