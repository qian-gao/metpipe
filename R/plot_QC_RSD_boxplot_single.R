#' @title plot_QC_RSD_boxplot_single
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' 
#' @export
#' @import ggplot2


plot_QC_RSD_boxplot_single <- 
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