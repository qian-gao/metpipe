#' @title create_qc
#'
#' @description Create QC plots for internal standards across run order.
#'
#' @param sampled_data A data frame containing internal-standard measurements.
#' @param figure_height Plot height in pixels.
#' @param figure_width Plot width in pixels.
#'
#' @return A list of plotly/htmlwidget QC plots.
#'
#' @examples
#' \dontrun{
#' qc_plots <- create_qc(sampled_data)
#' }
#'
#' @export
#' @import dplyr
#' @import plotly ggplot2
#'
create_qc <-
  function(sampled_data = NULL,
           figure_height = NULL,
           figure_width = 600) {

    required_cols <- c(
      "Run.order", "file.rt", "ISTD", "Sample", "rt.dev", "mz",
      "mz.dev", "file.area", "Intensity.dev", "Sample.type",
      "Theoretical.rt", "median.rt", "Theoretical.mz", "median.mz",
      "median.intensity", "RSD.intensity"
    )

    if (!is.data.frame(sampled_data)) {
      stop("sampled_data must be a data.frame")
    }
    if (!all(required_cols %in% names(sampled_data))) {
      stop("sampled_data is missing required QC columns")
    }
    if (is.null(figure_height)) {
      figure_height <- 400
    }

    sample_nr <- max(sampled_data$Run.order, na.rm = TRUE)
    # figure_width <- sample_nr*40
    #
    # if (figure_width < 600) {
    #   figure_width <- 600
    # }

    plt <- htmltools::tagList()
    plt[[1]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Run.order, file.rt, color = ISTD, text = Sample)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Retention Time as a Function of Running Time",
                                       x = "Running order",
                                       y = "RT (min)",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr))#, breaks = integer_breaks(sample_nr))

                                   #, width = figure_width, height = figure_height
    ))

    plt[[2]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Run.order, rt.dev, color = ISTD, text = Sample)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "RT deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: RT (min)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr)) #, breaks = integer_breaks(sample_nr))

                                   #, width = figure_width, height = figure_height
    ))
    #plt[[2]] <- style(plt[[2]], showlegend = FALSE)

    plt[[3]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Run.order, mz, color = ISTD, text = Sample)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "m/z as a Function of Running Time",
                                       x = "Running order",
                                       y = "m/z",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr)) #, breaks = integer_breaks(sample_nr))

                                   #, width = figure_width, height = figure_height
    ))

    plt[[4]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Run.order, mz.dev, color = ISTD, text = Sample)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "m/z deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: m/z (ppm)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr)) #, breaks = integer_breaks(sample_nr))

                                   #, width = figure_width, height = figure_height
    ))
    #plt[[4]] <- style(plt[[4]], showlegend = FALSE)

    plt[[5]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Run.order, file.area, color = ISTD, text = Sample)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Peak area as a Function of Running Time",
                                       x = "Running order",
                                       y = "Peak area",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr)) #, breaks = integer_breaks(sample_nr))

                                   #, width = figure_width, height = figure_height
    ))

    plt[[6]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Run.order, Intensity.dev, color = ISTD, text = Sample)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Peak area deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: Peak area (SD)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr)) #, breaks = integer_breaks(sample_nr))

                                   #, width = figure_width, height = figure_height
    ))
    #plt[[6]] <- style(plt[[6]], showlegend = FALSE)

    summary <-
      sampled_data %>%
      group_by( ISTD, Sample.type) %>%
      summarize( RSD = sd(file.area, na.rm = TRUE) / mean(file.area, na.rm = TRUE)*100 )

    plt[[7]] <- as_widget(ggplotly(ggplot(summary, aes(ISTD, RSD, fill = Sample.type)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_bar(stat = "Identity", position = "dodge") +
                                     scale_x_discrete(limits=rev) +
                                     theme_bw() +
                                     labs( #title = "RSD of internal standards",
                                       x = "Internal standards",
                                       y = "RSD (%)") +
                                     coord_flip()

                                   #, width = 1000
    ))

    summary <-
      sampled_data %>%
      select(ISTD, Theoretical.rt, median.rt, Theoretical.mz, median.mz, median.intensity, RSD.intensity) %>%
      unique()

    plt[[8]] <- as_widget(ggplotly(ggplot(summary, aes(x = Theoretical.rt, y = median.rt, color = ISTD )) +
                                     geom_abline(intercept = 0, slope = 1, color = "gray") +
                                     geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue", alpha = 0.1) +
                                     geom_point(size = 2, aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs( 
                                       x = "Theoretical RT (min)",
                                       y = "Median RT (min)")

                                   #, width = 800
    ))

    return(plt)
  }

integer_breaks <- function(n = 50, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
