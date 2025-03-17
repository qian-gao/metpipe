missing_eval <- 
  function(
    data_cleaned = NULL
    ){
    
    figs <- list()
    
    df <- 
      data_cleaned %>%
      rownames_to_column("Feature") %>% 
      pivot_longer(-Feature, names_to = "Sample", values_to = "var") %>%
      mutate(isna = (is.na(var)|var<=0))
    
    figs$p1 <- ggplot(df, aes(x = Sample, y = Feature, fill = isna)) +
      geom_raster() +
      scale_fill_manual(name = "",
                        values = c('#132b43', '#4fa3e4'),
                        labels = c("Present", "Missing")) +
      labs(title = "Missing values") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    data.plot <- data_cleaned
    miss.sample <- data.frame(Sample = colnames(data.plot),
                              missing = apply(data.plot, 2, function(x){sum(is.na(x)|x<=0)/length(x)*100}))
    
    
    figs$p2 <- ggplot(miss.sample, aes(x = Sample,y = missing)) +
      geom_bar(stat = "identity") +
      labs( x = "Samples",
            y = "Proportion of missing values (%)") +
      theme_bw() +
      theme(axis.text.x = element_blank())
    
    miss.feature <- data.frame(Feature = rownames(data.plot),
                               missing = apply(data.plot, 1, function(x){sum(is.na(x)|x<=0)/length(x)*100}))
    
    figs$p3 <- ggplot(miss.feature, aes(x = Feature,y = missing)) +
      geom_bar(stat = "identity") +
      labs( x = "Features",
            y = "Proportion of missing values (%)") +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            panel.grid.major.x = element_blank())
  
    return(figs)
    
  }