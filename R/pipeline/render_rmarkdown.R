render_rmarkdown <-
  function(
    file = NULL,
    params = NULL,
    output = NULL
    
  ){
    
    rmarkdown::render( file,
                       params = params,
                       output_file = paste0( output, "_", Sys.Date(), ".html"))
  
   }