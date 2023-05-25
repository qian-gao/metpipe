#' @title plot_PCA_Q_T2
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
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
#' @import ggplot2
#' @import plotly
#' @importFrom tidyr gather
#'
plot_PCA_Q_T2 <-
  function(
  x,
  id = NULL,
  highlight = NULL,
  alpha = 0.5,
  plotly.text = rownames(x)
  ){

    # Example
    # id <- sample.info.raw$id
    # highlight <- "19"
    # x <- data.raw

    y <- x

    if ( any( is.na( x ) ) ) {

      y <-
        apply(
          X = y,
          MAR = 2,
          FUN =
            function( x ) {
              tmp <- which( is.na( x ) )
              x[ tmp ] <- median( x = x, na.rm = TRUE )
              return( x )
            }
        ) %>%
        as.data.frame()

      rownames(y) <- rownames(x)
    }

    highlight.index <- as.factor(ifelse(id == highlight, "Highlighted", "Others"))

    # Q residuals vs Hotelling's T2
    pca <- mdatools::pca(y, 3, scale = TRUE)

    Q <-
      data.frame(Sample = rownames(y), pca$res$cal$Q) %>%
        gather(-Sample, key = "Component", value = "Q")

    T2 <-
      data.frame(Sample = rownames(y), pca$res$cal$T2) %>%
        gather(-Sample, key = "Component", value = "T2")

    data.plot <- cbind(Q, T2=T2[,3])

    p1 <-
      ggplot(data.plot[data.plot$Component == "Comp.1",],
                 aes(x = T2, y = Q,  text = plotly.text)) +
        geom_point(aes(color = highlight.index), alpha = alpha) +
        theme(legend.title = element_text("Highlight")) +
        labs(x = "",
             y = "Q residuals",
             color = "Highlight")
    p1 <- style(p1, showlegend = FALSE)

    p2 <-
      ggplot(data.plot[data.plot$Component == "Comp.2",],
                 aes(x = T2, y = Q,  text = plotly.text)) +
        geom_point(aes(color = highlight.index), alpha = alpha) +
        theme(legend.title = element_text("Highlight")) +
        labs(x = "Hotelling's T2",
             y = "",
             color = "Highlight")
    p2 <- style(p2, showlegend = FALSE)

    p3 <-
      ggplot(data.plot[data.plot$Component == "Comp.3",],
                 aes(x = T2, y = Q,  text = plotly.text)) +
        geom_point(aes(color = highlight.index), alpha = alpha) +
        theme(legend.title = element_text("Highlight")) +
        labs(x = "",
             y = "",
             color = "Highlight",
             title = "PC1 - PC3")

    p <- plotly::subplot(p1, p2, p3, margin = 0.01, shareX = TRUE, shareY = TRUE)

    return(p)
  }
