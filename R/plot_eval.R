#' @title plot_eval
#'
#' @description Compare normalization methods using RSD, MAD, and variance summaries.
#'
#' @param x Named list of normalized data matrices/data frames (sample x feature).
#' @param type Sample type vector aligned with rows of each matrix.
#'
#' @return A list of ggplot objects and summary tables for method evaluation.
#' @examples
#' \dontrun{
#' eval_plots <- plot_eval(x = peaks_norm, type = sample_info$Sample.type)
#' }
#' @export
#' @import dplyr tidyr
#' @import ggplot2 ggsci data.table
#' 
plot_eval <-
  function(
    x = NULL,
    type = NULL
  ){

    if (!is.list(x) || length(x) == 0) {
      stop("x must be a non-empty list of normalized matrices/data frames")
    }
    if (is.null(type)) {
      stop("type must be provided")
    }
    
    methods <- names(x)
    
    rsd <- 
      lapply( x, 
              function(x){ 
                
                calculate_rsd( data = x,
                               type = type)$type.rsd
                
              })
    
    plt.rsd.data <- 
      data.table::rbindlist(rsd, idcol = "Method") %>% 
      as.data.frame()
    
    plt.rsd.data <- plt.rsd.data[, !colnames(plt.rsd.data) %in% c("Sample"), drop = FALSE]
      
    plt.rsd.data <- 
      plt.rsd.data %>%
      pivot_longer( -c("Method", "Identity"), names_to = "Sample.type", values_to = "RSD") %>%
      mutate(RSD = RSD*100) %>%
      mutate(Method = factor(Method, levels = methods))
    
    plt.rsd <-
      ggplot( data = plt.rsd.data, 
              aes(x = Method, y = RSD) ) +
      geom_boxplot( aes(fill = Sample.type), #outlier.shape = NA, 
                    position = "dodge") +
      #coord_cartesian(ylim = quantile(plt.rsd.data$RSD, c(0.1, 0.9))) +
      theme_bw() +
      scale_fill_npg()
    
    plt.rsd.m <- 
      plt.rsd.data %>%
      group_by( Method, Sample.type) %>%
      summarize( mean = mean(RSD, na.rm = TRUE))
    
    plt.rsd.mean <-
      plt.rsd.m %>%
      left_join(plt.rsd.m[ plt.rsd.m$Method %in% "raw", ], by = "Sample.type") %>%
      mutate( ratio = mean.x / mean.y * 100) %>%
      dplyr::rename( Method = Method.x) %>%
      mutate(Method = factor(Method, levels = methods))
    
    plt.rsd.mean <-
      ggplot( data = plt.rsd.mean, 
              aes(x = Method, y = ratio) ) +
      geom_bar( stat = "identity",
                aes(fill = Sample.type), position = "dodge") +
      theme_bw() +
      labs( y = "Relative mean CV") +
      scale_fill_npg()
    
    
    stats <- 
      lapply( x, 
              function(x){ 
                
                calculate_rsd( data = log2(x),
                               type = type,
                               other.eval = TRUE)
                
              })
    
    mad <-
      lapply( stats, 
              function(x){ 
                
                x$type.mad
                
              })
    
    plt.mad.data <- 
      data.table::rbindlist(mad, idcol = "Method") %>% 
      as.data.frame()
    
    plt.mad.data <- plt.mad.data[, !colnames(plt.mad.data) %in% c("Sample"), drop = FALSE]
    
    plt.mad.data <- 
      plt.mad.data %>%
      pivot_longer( -c("Method", "Identity"), names_to = "Sample.type", values_to = "MAD") %>%
      mutate(Method = factor(Method, levels = methods))
    
    plt.mad <-
      ggplot( data = plt.mad.data, 
              aes(x = Method, y = MAD) ) +
      geom_boxplot( aes(fill = Sample.type), #outlier.shape = NA, 
                    position = "dodge") +
      #coord_cartesian(ylim = quantile(plt.mad.data$MAD, c(0.1, 0.9))) +
      theme_bw() +
      scale_fill_npg() 
    
    plt.mad.m <- 
      plt.mad.data %>%
      group_by( Method, Sample.type) %>%
      summarize( mean = mean(MAD, na.rm = TRUE))
    
    plt.mad.mean <-
      plt.mad.m %>%
      left_join(plt.mad.m[ plt.mad.m$Method %in% "raw", ], by = "Sample.type") %>%
      mutate( ratio = mean.x / mean.y * 100) %>%
      dplyr::rename( Method = Method.x) %>%
      mutate(Method = factor(Method, levels = methods))
    
    plt.mad.mean <-
      ggplot( data = plt.mad.mean, 
              aes(x = Method, y = ratio) ) +
      geom_bar( stat = "identity",
                aes(fill = Sample.type), position = "dodge") +
      theme_bw() +
      labs( y = "Relative mean MAD") +
      scale_fill_npg()
    
    
    var <-
      lapply( stats, 
              function(x){ 
                
                x$type.var
                
              })
    
    plt.var.data <- 
      data.table::rbindlist(var, idcol = "Method") %>% 
      as.data.frame()
    
    plt.var.data <- plt.var.data[, !colnames(plt.var.data) %in% c("Sample"), drop = FALSE]
    
    plt.var.data <- 
      plt.var.data %>%
      pivot_longer( -c("Method", "Identity"), names_to = "Sample.type", values_to = "VAR") %>%
      mutate(Method = factor(Method, levels = methods))
    
    plt.var <-
      ggplot( data = plt.var.data, 
              aes(x = Method, y = VAR) ) +
      geom_boxplot( aes(fill = Sample.type), #outlier.shape = NA, 
                    position = "dodge") +
      #coord_cartesian(ylim = quantile(plt.var.data$VAR, c(0.1, 0.9))) +
      theme_bw() +
      scale_fill_npg() 
    
    plt.var.m <- 
      plt.var.data %>%
      group_by( Method, Sample.type) %>%
      summarize( sum = sum(VAR, na.rm = TRUE))
    
    plt.var.sum <-
      plt.var.m %>%
      left_join(plt.var.m[ plt.var.m$Method %in% "raw", ], by = "Sample.type") %>%
      mutate( ratio = sum.x / sum.y * 100) %>%
      dplyr::rename( Method = Method.x) %>%
      mutate(Method = factor(Method, levels = methods))
    
    plt.var.sum <-
      ggplot( data = plt.var.sum, 
              aes(x = Method, y = ratio) ) +
      geom_bar( stat = "identity",
                aes(fill = Sample.type), position = "dodge") +
      labs( y = "Relative variance") +
      theme_bw() +
      scale_fill_npg()
    
    plt <-
      list( plt.rsd = plt.rsd,
            plt.rsd.mean = plt.rsd.mean,
            plt.mad = plt.mad,
            plt.mad.mean = plt.mad.mean,
            plt.var = plt.var,
            plt.var.sum = plt.var.sum)
    
    return(plt)
    
  }