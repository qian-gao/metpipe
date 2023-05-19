plot_QC_RSD_boxplot<- 
  function( 
    x = NULL,
    type = NULL
  ) {
  
  y <- x
  
  rsd <- 
    lapply( y, 
            function(x){       
              calculate_rsd( data = x,
                             type = type)$type.rsd 
            })
  
  plt.rsd <- 
    data.table::rbindlist(rsd, idcol = "Method") %>%
    select( -Sample) %>%
    pivot_longer( -c("Method", "Identity"), names_to = "Sample.type", values_to = "RSD") %>%
    mutate(RSD = RSD*100)

  plot <-
    ggplot( data = plt.rsd, 
            aes(x = Method, y = RSD) ) +
      geom_boxplot( aes(fill = Sample.type), position = "dodge")
      #geom_violin(alpha = 0.5)
    
  return( plot )
  
  }

plot_QC_RSD_hist<- 
  function( 
    x,
    type,
    pool,
    hist.width = 5,
    hist.height = 15,
    print = TRUE
  ) {
    
    y <- x[ type == pool, ]

    RSD <- apply(y, 2, function(x) {sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100} )
    
    y <- data.frame(RSD)
    
    plot <-
      ggplot2::ggplot(
        data = y, 
        mapping = 
          ggplot2::aes(
            x = RSD
          )
      ) +
      #ggplot2::geom_histogram(binwidth = 0.05, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      ggplot2::geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      scale_x_continuous(limits = c(0, hist.width)) +
      scale_y_continuous(limits = c(0, hist.height)) +
      ggplot2::theme_minimal()
    
    if ( print ) {
      
      print( plot )
      
    }
    
    return( plot )
    
  }

plot_QC_RSD_boxplot_single<- 
  function( 
    x,
    type,
    pool,
    print = TRUE
  ) {
    
    y <- x
    
    QC.RSD <- apply(x[ type == pool, ], 2, function(x) {sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100})
    
    y <- reshape2::melt(QC.RSD)
    
    plot <-
      ggplot2::ggplot(
        data = y, 
        mapping = 
          ggplot2::aes(
            x = "Normalized", 
            y = value
          )
      ) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_violin(alpha = 0.5) +
      ggplot2::xlab("") +
      ggplot2::ylab("RSD") 
    #ggplot2::theme_minimal()
    
    if ( print ) {
      
      print( plot )
      
    }
    
    return( plot )
    
  }