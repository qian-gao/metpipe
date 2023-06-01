#' @title plot_gam
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
#' @importFrom dplyr "%>%" mutate
#' @import ggplot2
#'
plot_gam <-
  function ( mod,
             data = NULL,
             se = FALSE

  ){

    if (is.null(data)){
      data <- mod$model
      names(data) <- c("fit", "x")
    }

    pdf(file = NULL) # Mute display of plot
    pd <- plot(mod)[[1]]
    dev.off()

    data.plot <-
      data.frame( x =   pd$x,
                  fit = pd$fit,
                  se =  pd$se) %>%
      mutate( fit = fit + mod$coefficients[["(Intercept)"]],
              ymin = fit - se,
              ymax = fit + se)

    if (se == TRUE){

      p <-
        ggplot(data = data.plot, aes(x = x, y = fit)) +
        geom_line(color = "#999999", size = 1, alpha = 0.9) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.1) +
        geom_point(data = data, aes(x = x, y = fit),
                   size = 2.5, alpha = 0.5,
                   shape = 21, color = colorspace::darken("#0072B2", 0.3), fill = "#0072B2") +
        theme_bw()

    } else {

      p <-
        ggplot(data = data.plot, aes(x = x, y = fit)) +
        geom_line(color = "#999999", size = 1, alpha = 0.9) +
        #geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.1) +
        geom_point(data = data, aes(x = x, y = fit),
                   size = 2.5, alpha = 0.5,
                   shape = 21, color = colorspace::darken("#0072B2", 0.3), fill = "#0072B2") +
        theme_bw()

    }

    return(p)

  }
