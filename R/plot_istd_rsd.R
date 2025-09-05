#' @title plot_istd_rsd
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
#' @export
#' @import ggplot2 ggsci htmltools dplyr tidyr
#'
plot_istd_rsd <-
  function( datalist = NULL){

    data.istd <- datalist$data.istd
    #data <- datalist$data
    sample.info <- 
      datalist$sample.info %>% 
      mutate(Run.order = row_number())
    #feature.info <- datalist$feature.info
    #feature.info.istd <- datalist$feature.info.istd

    data.istd.plot <-
      cbind(sample.info, data.istd)

    plt.int <- htmltools::tagList()
    for (i in colnames(data.istd)) {

      p <-
        ggplot(data.istd.plot,
               aes(x = Run.order, y = get(i), fill = Sample.type, text = Sample)) +
        geom_col() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        labs( x = "Run.order",
              y = "Intensity",
              title = i) +
        scale_fill_npg() 

      plt.int[[i]] <- as_widget(ggplotly(p))

    }

    rsd <-
      calculate_rsd( data = data.istd,
                     type = sample.info$Sample.type,
                     impute = TRUE)

    istd.rsd.plot <-
      rsd$type.rsd %>%
      dplyr::rename( IS = Identity) %>%
      pivot_longer( -IS, names_to = "Sample.type", values_to = "RSD") %>%
      mutate( RSD = RSD*100)


    plt.rsd <-
      ggplot(istd.rsd.plot, aes(x = IS, y = RSD, fill = Sample.type)) +
      geom_bar(stat = "Identity", position="dodge")  +
      scale_x_discrete(limits=rev) +
      theme_bw() +
      labs( #title = "RSD of internal standards",
        x = "Internal standards",
        y = "RSD (%)") +
      coord_flip() +
      scale_fill_npg() 

    plt <- list( plt.int = plt.int,
                 plt.rsd = plt.rsd)

    return(plt)
  }
