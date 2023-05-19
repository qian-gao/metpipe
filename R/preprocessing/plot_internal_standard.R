plot_internal_standard <- 
  function( 
    x,
    group = NULL,
    alpha = 0.9,
    print = TRUE
  ) {
  
  y <- x[,8]
  
  #y <- (y-mean(y, na.rm = TRUE))/sd(y, na.rm = TRUE)
  
  y <- data.frame(Intensity = y, Order = 1:length(y), Group = group, stringsAsFactors = FALSE)
  
  plot <-
    ggplot2::ggplot(
      data = y, 
      mapping = 
        ggplot2::aes(
          x = Order, 
          y = Intensity, 
          group = group, 
          color = group
        )
      ) +
      ggplot2::geom_point(alpha = alpha) +
      scale_y_continuous(limits = c(-4, 4)) +
      ggplot2::scale_color_brewer( palette = "Set1" ) +
      ggplot2::theme( 
        axis.text.x = ggplot2::element_text( angle = 45, hjust = 1 ) )
    
  if ( print ) {
    
    print( plot )
    
  }
  
  return( plot )
  
}