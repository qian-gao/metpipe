plot_forest <-
  function( data = NULL,
            contrast.to.use = NULL,
            contrast.to.sort = NULL,
            coef.to.use = NULL,
            alpha = 0.8,
            yintercept = 0,
            Significance = NULL,
            point.shape = 19,
            top.nr = NULL,
            path.result = NULL,
            fig.width = 2800,
            fig.height = 1800
  ){
  
    if (!is.null(contrast.to.use)){
      
      data.plot <-
        data %>%
        filter(contrast %in% contrast.to.use) %>%
        mutate(contrast = factor(contrast, levels = contrast.to.use))
      
    } else {
      
      data.plot <- data
      
    }
    
    if (!is.null(coef.to.use)){
      
      data.plot <-
        data.plot %>%
        filter(coefficient %in% coef.to.use) %>%
        mutate(coefficient = factor(coefficient, levels = coef.to.use)) %>%
        dplyr::rename(contrast = coefficient)
      
    } else {
      
      data.plot <- data.plot
      
    }    
    
    if (is.null(contrast.to.sort)){
      
      contrast.to.sort <- contrast.to.use     
    }
    
    if (!is.null(top.nr)){
      
      sig.var <-
        data.plot %>%
        filter(contrast %in% contrast.to.sort) %>%
        arrange(adj.p.value) %>%
        select(variable) %>%
        unique() %>%
        filter(row_number() <= top.nr)
    
    } else {
      
      sig.var <- 
        data.plot %>%
        filter(contrast %in% contrast.to.sort) %>%
        select(variable)
      
    }
    
    data.plot <-
      data.plot %>%
      filter(variable %in% sig.var$variable) %>%
      mutate(Significance = case_when(adj.p.value < 0.05 ~ "q < 0.05",
                                      p.value     < 0.05 ~ "p < 0.05",
                                      TRUE               ~ "Not significant"))
    
    data.plot$Significance <- factor( data.plot$Significance,
                                      levels = c("q < 0.05", "p < 0.05", "Not significant"))
    
    p <- 
      ggplot( data = data.plot,
              aes(x = variable, y = estimate, ymin = CI.L, ymax = CI.H, col= Significance)) +
        geom_pointrange(alpha = alpha, show.legend = TRUE, shape = point.shape) +
        geom_hline(yintercept = yintercept, linetype=2) +
        coord_flip() +
        facet_grid(~ contrast, scales = "free_y")
    
    if (!is.null(path.result)){
      
      png( file = paste0(path.result, "/forest_plot.png"), width = fig.width, height = fig.height, res = 300)
      print(p)
      dev.off()
    }

  return(p)

}