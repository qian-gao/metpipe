#' @title create_qc
#'
#' @description Create qc plots for internal standards
#'
#' @param sampled_data A dataframe containing IS in samples
#' @param figure_height Figure height for plot
#' @param figure_width Figure width for plot
#'
#' @return A list qc figures
#'
#' @examples
#'
#' @export
#' @import dplyr
#' @import plotly
#'
create_qc <-
  function(sampled_data = NULL,
           figure_height = NULL,
           figure_width = 600) {

    sample_nr <- max(sampled_data$Order, na.rm = TRUE)
    # figure_width <- sample_nr*40
    #
    # if (figure_width < 600) {
    #   figure_width <- 600
    # }

    plt <- htmltools::tagList()
    plt[[1]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, rt, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Retention Time as a Function of Running Time",
                                       x = "",
                                       y = "RT (min)",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))

    plt[[2]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, rt.dev, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "RT deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: RT (min)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))
    plt[[2]] <- style(plt[[2]], showlegend = FALSE)

    plt[[3]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, mz, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "m/z as a Function of Running Time",
                                       x = "",
                                       y = "m/z",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))

    plt[[4]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, mz.dev, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "m/z deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: m/z (ppm)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))
    plt[[4]] <- style(plt[[4]], showlegend = FALSE)

    plt[[5]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, Intensity, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Peak area as a Function of Running Time",
                                       x = "Running order",
                                       y = "Peak area",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))

    plt[[6]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, Intensity.dev, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Peak area deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: Peak area (%)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))
    plt[[6]] <- style(plt[[6]], showlegend = FALSE)

    summary <-
      sampled_data %>%
      group_by( ISTD, Sample.type) %>%
      summarize( RSD = sd(Intensity, na.rm = TRUE) / mean(Intensity, na.rm = TRUE)*100 )

    plt[[7]] <- as_widget(ggplotly(ggplot(summary, aes(ISTD, RSD, fill = Sample.type)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_bar(stat = "Identity", position = "dodge") +
                                     scale_x_discrete(limits=rev) +
                                     theme_bw() +
                                     labs( #title = "RSD of internal standards",
                                       x = "Internal standards",
                                       y = "RSD (%)") +
                                     coord_flip()

                                   , width = 1000
    ))

    summary <-
      sampled_data %>%
      select(ISTD, Theoretical.rt, median.rt, Theoretical.mz, median.mz, median.intensity, RSD.intensity) %>%
      unique()

    plt[[8]] <- as_widget(ggplotly(ggplot(summary, aes(x = Theoretical.rt, y = median.rt, color = ISTD )) +
                                     geom_abline(intercept = 0, slope = 1, color = "gray") +
                                     geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue", alpha = 0.1) +
                                     geom_point(size = 2, aes(shape = ISTD)) + scale_shape_manual(values = c(1:25))

                                   , width = 800
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
