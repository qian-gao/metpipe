#' @title plot_forest_classes
#'
#' @description plot lipidomics forest plot
#'
#' @param data = NULL,
#' @param y.log = FALSE, # "log" "log2"
#' @param cut.off.1 = 0.05,
#' @param cut.off.2 = 0.001,
#' @param cut.off.label = 1e-7,
#' @param alpha = 0.8,
#' @param format.adjust = TRUE,
#' @param max.overlaps = 10
#'
#' @return A plot
#' @examples
#' @export
#' @import dplyr ggplot2

plot_forest_classes <-
  function(data = NULL,
           y.log = FALSE, # "log" "log2"
           cut.off.1 = 0.05,
           cut.off.2 = 0.001,
           cut.off.label = 1e-7,
           #cut.off.label = NULL,
           alpha = 0.8,
           format.adjust = TRUE,
           max.overlaps = 10
  ){

    if (y.log == "log" | y.log == TRUE){

      data.plot <-
        data %>%
        mutate(estimate = (exp(estimate) - 1)*100,
               CI_L = (exp(CI_L) - 1)*100,
               CI_H = (exp(CI_H) - 1)*100)
      labs.x <- "% "

    } else if (y.log == "log2"){

      data.plot <-
        data %>%
        mutate(estimate = (2^(estimate) - 1)*100,
               CI_L = (2^(CI_L) - 1)*100,
               CI_H = (2^(CI_H) - 1)*100)
      labs.x <- "% "

    } else {

      data.plot <- data
      labs.x <- ""

    }

    if (!"Significance" %in% names(data.plot)){
      data.plot <-
        data.plot %>%
        mutate(Significance = case_when(adj.p.value < cut.off.2 ~ as.character(cut.off.2),
                                        adj.p.value < cut.off.1 ~ as.character(cut.off.1),
                                        TRUE           ~ "None"),
               Label = ifelse(adj.p.value < cut.off.label, as.character(cut.off.label), "None"))
    }

    if (format.adjust){
      format_adjust <-
        data.plot %>%
        group_by(Class, contrast) %>%
        mutate(n = row_number(),
               n.max = n()) %>%
        ungroup()

      format_adjust_1 <-
        format_adjust %>%
        slice(rep(which(n == 1|n == n.max), 1)) %>%
        mutate(Significance = "place.holder",
               dv = case_when( n == 1 ~ Class,
                               n == n.max ~ paste0(Class, "99"),
                               TRUE ~ dv)) %>%
        select(-c(n, n.max))

      format_adjust_2 <-
        format_adjust %>%
        slice(rep(which(n == 1 & n.max <= 2), 2)) %>%
        mutate(Significance = "place.holder",
               dv = case_when( n == n.max ~ paste0(Class, "99"),
                               TRUE ~ dv)) %>%
        select(-c(n, n.max))

      data.plot <-
        rbind(data.plot, format_adjust_1, format_adjust_2)

      cell.height <-
        data.plot %>%
        group_by(contrast, Class) %>%
        add_count(Class) %>%
        ungroup() %>%
        select(Class, n) %>%
        unique() %>%
        mutate(size = n / 10 * 0.4,
               size = ifelse(size < 0.3, 0.3, size),
               size = ifelse(size > 3,   2,   size)) %>%
        arrange(tolower(Class))

    }

    #if (is.null(cut.off.label)) cut.off.label <- cut.off.3

    p <-
      ggplot( data = data.plot,
              aes(x = dv, y = estimate, ymin = CI_L, ymax = CI_H )) +
        geom_point(data = data.plot[data.plot$Significance == "place.holder", ],
                   aes(col= Class), shape = 1, alpha = 0, size = 1.3, show.legend = FALSE) +
        geom_point(data = data.plot[data.plot$Significance == "None", ],
                   aes(col= Class), shape = 1, alpha = alpha, size = 1.3, show.legend = FALSE) +
        geom_point(data = data.plot[data.plot$Significance == as.character(cut.off.1), ],
                   aes(col= Class), alpha = alpha, size = 1.5, show.legend = FALSE) +
        geom_pointrange(data = data.plot[data.plot$Significance %in% c(as.character(cut.off.2)), ],
                        aes(col= Class), alpha = alpha, size = 0.4, fatten = 3, show.legend = FALSE) +
        ggrepel::geom_text_repel(data = data.plot[data.plot$Label == as.character(cut.off.label), ],
                                 aes(label = dv), size = 2,
                                 max.overlaps = max.overlaps) +
        geom_hline(yintercept =0, linetype=2) +
        scale_x_discrete(expand=c(0.1, 0)) +
        theme( #plot.title = element_text(size=16,face="bold"),
               panel.background = element_rect(fill = "grey96"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.spacing = unit(0.1, "lines"),
               strip.background = element_rect(fill=NA),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               #axis.text.x = element_text(face="bold"),
               #axis.title = element_text(size=12,face="bold"),
               strip.text.y = element_text(hjust = 0, angle=0))+
        coord_flip() +
        facet_grid(Class ~ contrast, space = "free_y", scales = "free_y") +
        ggh4x::force_panelsizes(rows = cell.height$size) +
        labs( x = 'Lipid class',
              y = paste0(labs.x, "Change in lipids (95% Confidence Interval)"),
              title = "Change in all lipids between groups")

        # facet_grid(Class ~ Visit, space = "free_y", scales = "free_y") +
        # labs( x = 'Lipid class',
        #       y = "Change in lipids (95% Confidence Interval)",
        #       title = "Change in all lipids over time") +
        # ggh4x::force_panelsizes(rows = c(0.3, 0.3, 0.3, 0.2, 0.3,
        #                                  0.2, 0.2, 0.2, 0.7, 0.2,
        #                                  0.3, 0.7, 0.7, 0.3, 0.3,
        #                                  0.3, 0.3, 0.2, 0.7, 1.0))

  }



