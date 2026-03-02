#' @title plot_istd_rsd
#'
#' @description Plot internal-standard intensity trends and RSD by sample type.
#'
#' @param datalist List containing `data.istd` and `sample.info`.
#'
#' @return A list with plotly intensity plots and a ggplot RSD summary.
#' @examples
#' \dontrun{
#' p <- plot_istd_rsd(datalist)
#' }
#' @export
#' @import ggplot2 ggsci htmltools dplyr tidyr
#'
plot_istd_rsd <-
  function( datalist = NULL){

    if (!is.list(datalist) || is.null(datalist$data.istd) || is.null(datalist$sample.info)) {
      stop("datalist must contain data.istd and sample.info")
    }

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
