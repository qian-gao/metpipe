#' @title plot_forest
#'
#' @description plot forest plot
#'
#' @param data = NULL,
#' @param contrast.to.use = NULL,
#' @param contrast.to.sort = NULL,
#' @param coef.to.use = NULL,
#' @param coef.to.sort = NULL,
#' @param alpha = 0.8,
#' @param yintercept = 0,
#' @param Significance = NULL,
#' @param point.shape = 19,
#' @param top.nr = NULL,
#' @param path.result = NULL,
#' @param fig.width = 8,
#' @param fig.height = 6,
#' @param map.contrast = NULL,
#' @param color.manual = c("#4682B4", "#B4464B", "#B4AF46")
#'
#' @return A plot
#' @examples
#' @export
#' @import dplyr ggplot2
#'
plot_forest <-
  function( data = NULL,
            contrast.to.use = NULL,
            contrast.to.sort = NULL,
            coef.to.use = NULL,
            coef.to.sort = NULL,
            alpha = 0.8,
            yintercept = 0,
            Significance = NULL,
            point.shape = 19,
            top.nr = NULL,
            path.result = NULL,
            fig.width = 8,
            fig.height = 6,
            map.contrast = NULL,
            color.manual = c("#4682B4", "#B4464B", "#B4AF46")
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

  if (is.null(coef.to.sort)){

    coef.to.sort <- coef.to.use
  }

  if (!is.null(top.nr)){

    sig.var <-
      data.plot %>%
      filter(contrast %in% c(contrast.to.sort, coef.to.sort)) %>%
      arrange(adj.p.value) %>%
      select(variable) %>%
      unique() %>%
      filter(row_number() <= top.nr)

  } else {

    sig.var <-
      data.plot %>%
      filter(contrast %in% c(contrast.to.sort, coef.to.sort)) %>%
      select(variable)

  }

  if (!is.null(map.contrast)) {
    names(map.contrast) <- contrast.to.use
    data.plot <-
      data.plot %>%
      mutate(contrast = map.contrast[contrast])
  }

  data.plot <-
    data.plot %>%
    filter(variable %in% sig.var$variable) %>%
    mutate(Significance = case_when(adj.p.value < 0.05 ~ "q < 0.05",
                                    p.value     < 0.05 ~ "p < 0.05",
                                    TRUE               ~ "Not significant"))

  data.plot$Significance <- factor( data.plot$Significance,
                                    levels = c("q < 0.05", "p < 0.05", "Not significant"))

  if (!is.null(color.manual)){

    names(color.manual) <- c("Not significant", "p < 0.05", "q < 0.05")

  }

  p <-
    ggplot( data = data.plot,
            aes(x = variable, y = estimate, ymin = CI.L, ymax = CI.H, col= Significance)) +
      geom_pointrange(alpha = alpha, show.legend = TRUE, shape = point.shape) +
      geom_hline(yintercept = yintercept, linetype=2) +
      coord_flip() +
      {if (!is.null(color.manual))
        scale_color_manual( values = color.manual ) } +
      facet_grid(~ contrast, scales = "free_y") +
      theme_bw()


  if (!is.null(path.result)) {
    ggsave( file = paste0(path.result, "/forest_plot.png"), width = fig.width, height = fig.height, dpi = 300)
  }

  return(p)
}
