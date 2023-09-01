#' @title plot_PCA
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
#'
plot_PCA <-
  function(
    x,
    group = NULL,
    group.label = "Group",
    plotly.text = rownames(x),
    alpha = 0.25,
    print = FALSE,
    plot.type = "score", # score, loading, loading_arrow, biplot
    ratio = 1,
    color.manual = NULL,
    color.scheme = NULL,
    xlim = NULL,
    ylim = NULL,
    pcs = 1:2,
    text.size = 3,
    circle = TRUE,
    ellipse = TRUE,
    ...
  ) {

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
        )

    }

    result.pca <-
      prcomp(
        x = y,
        center = TRUE,
        scale = TRUE
      )

    if (plot.type == "score") {
      plot <-
        ggbiplot(
          pcobj = result.pca,
          groups = group,
          plotly.text = plotly.text,
          alpha = alpha,
          plot.type = plot.type,
          circle = circle,
          ellipse = ellipse,
          obs.scale = 1,
          var.axes = FALSE,
          var.scale = 1,
          choices = pcs,
          text.size = text.size,
          ...
        )

    } else if(plot.type == "loading") {

      plot <-
        ggbiplot(
          pcobj = result.pca,
          groups = group,
          plotly.text = plotly.text,
          alpha = alpha,
          plot.type = plot.type,
          circle = FALSE,
          ellipse = FALSE,
          obs.scale = 1,
          var.axes = FALSE,
          var.scale = 1,
          choices = pcs,
          text.size = text.size,
          ...
        )

    } else if(plot.type == "loading_arrow") {

      plot <-
        ggbiplot(
          pcobj = result.pca,
          groups = group,
          plotly.text = plotly.text,
          alpha = alpha,
          plot.type = plot.type,
          circle = FALSE,
          ellipse = FALSE,
          obs.scale = 1,
          var.axes = TRUE,
          var.scale = 1,
          choices = pcs,
          text.size = text.size,
          ...
        )

    } else if(plot.type == "biplot") {

      plot <-
        ggbiplot(
          pcobj = result.pca,
          groups = group,
          plotly.text = plotly.text,
          alpha = alpha,
          plot.type = plot.type,
          circle = circle,
          ellipse = ellipse,
          obs.scale = 1,
          var.axes = TRUE,
          var.scale = 1,
          choices = pcs,
          text.size = text.size,
          ...
        )



    }

    plot <-
      plot +
        ggplot2::theme( legend.direction = "vertical",
                        legend.position = "right") +
        coord_fixed( ratio = ratio, xlim = xlim, ylim = ylim ) +
        theme_bw() +
        guides(color = guide_legend(title = group.label))

    if (!is.null(color.manual)) {
      plot <-
        plot +
        scale_color_manual(values = color.manual)
    }

    if (!is.null(color.scheme)) {

      scale.color <- paste0("scale_color_", color.scheme,"()")
      plot <-
        plot +
        eval(parse(text = scale.color))
    }

    if ( print ) {

      print( plot )

    }

    return( plot )

  }

#' @title ggbiplot
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
#' @import ggplot2
#' @import tidyverse
#' @import scales
#' @import grid
#'
ggbiplot <-
  function (pcobj, plotly.text = rownames(pcobj$x),
            plot.type = NULL, choices = 1:2, scale = 1, pc.biplot = TRUE,
            obs.scale = 1 - scale, var.scale = scale, groups = NULL,
            ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3,
            alpha = 1, var.axes = TRUE, circle = FALSE, circle.prob = 0.69,
            varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE,
            text.size = text.size,
            ...)
  {
    library(ggplot2)
    library(tidyverse)
    library(scales)
    library(grid)
    stopifnot(length(choices) == 2)
    if (inherits(pcobj, "prcomp")) {
      nobs.factor <- sqrt(nrow(pcobj$x) - 1)
      d <- pcobj$sdev
      u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
      v <- pcobj$rotation
    }
    else if (inherits(pcobj, "princomp")) {
      nobs.factor <- sqrt(pcobj$n.obs)
      d <- pcobj$sdev
      u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
      v <- pcobj$loadings
    }
    else if (inherits(pcobj, "PCA")) {
      nobs.factor <- sqrt(nrow(pcobj$call$X))
      d <- unlist(sqrt(pcobj$eig)[1])
      u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor),
                 FUN = "*")
      v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord),
                                                    1]), FUN = "/")
    }
    else if (inherits(pcobj, "lda")) {
      nobs.factor <- sqrt(pcobj$N)
      d <- pcobj$svd
      u <- predict(pcobj)$x/nobs.factor
      v <- pcobj$scaling
      d.total <- sum(d^2)
    }
    else {
      stop("Expected a object of class prcomp, princomp, PCA, or lda")
    }
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale,
                                FUN = "*"))
    v <- sweep(v, 2, d^var.scale, FUN = "*")
    df.v <- as.data.frame(v[, choices])
    names(df.u) <- c("xvar", "yvar")
    names(df.v) <- names(df.u)
    if (pc.biplot) {
      df.u <- df.u * nobs.factor
    }
    r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
    v.scale <- rowSums(v^2)
    df.v <- r * df.v/sqrt(max(v.scale))
    if (obs.scale == 0) {
      u.axis.labs <- paste("standardized PC", choices, sep = "")
    }
    else {
      u.axis.labs <- paste("PC", choices, sep = "")
    }
    u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)",
                                              100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
    if (!is.null(labels)) {
      df.u$labels <- labels
    }
    if (!is.null(groups)) {
      df.u$groups <- groups
    }
    if (varname.abbrev) {
      df.v$varname <- abbreviate(rownames(v))
    }
    else {
      df.v$varname <- rownames(v)
    }
    df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
    df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)

    if (plot.type %in% c("score", "biplot")) {
      g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) +
        ylab(u.axis.labs[2]) + coord_equal()

    } else if (plot.type == "loading") {
      g <- ggplot(data = df.v, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) +
        ylab(u.axis.labs[2]) + coord_equal() +
        geom_text(data = df.v, aes(label = varname, x = xvar, y = yvar, hjust="inward", vjust = 1.2),
                  color = "black", size = text.size) +
        geom_vline(xintercept = 0, alpha = 0.5) +
        geom_hline(yintercept = 0, alpha = 0.5)

    } else if (plot.type == "loading_arrow") {
      g <- ggplot() + xlab(u.axis.labs[1]) +
        ylab(u.axis.labs[2]) + coord_equal() +
        geom_point(alpha = 0.1) +
        geom_vline(xintercept = 0, alpha = 0.5) +
        geom_hline(yintercept = 0, alpha = 0.5)
    }


    if (var.axes) {
      if (circle) {
        theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi,
                                                  length = 50))
        circle <- data.frame(xvar = r * cos(theta), yvar = r *
                               sin(theta))
        g <- g + geom_path(data = circle, color = muted("white"),
                           size = 1/2, alpha = 1/3)
      }
      g <- g + geom_segment(data = df.v, aes(x = 0, y = 0,
                                             xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2,
                                                                                                    "picas")), alpha = 0.5, color = "#001d6c")
    }
    if (!is.null(df.u$labels)) {
      if (!is.null(df.u$groups)) {
        g <- g + geom_text(aes(label = labels, color = groups),
                           size = labels.size)
      }
      else {
        g <- g + geom_text(aes(label = labels), size = labels.size)
      }
    }
    else {
      if (!is.null(df.u$groups)) {
        g <- g + geom_point(aes(color = groups,
                                text = plotly.text), alpha = alpha)
      }
      else {
        g <- g + geom_point(alpha = alpha, text = plotly.text)
      }
    }
    if (!is.null(df.u$groups) && ellipse) {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- cbind(cos(theta), sin(theta))
      ell <- plyr::ddply(df.u, "groups", function(x) {
        if (nrow(x) <= 2) {
          return(NULL)
        }
        sigma <- var(cbind(x$xvar, x$yvar))
        mu <- c(mean(x$xvar), mean(x$yvar))
        ed <- sqrt(qchisq(ellipse.prob, df = 2))
        data.frame(sweep(circle %*% chol(sigma) * ed, 2,
                         mu, FUN = "+"), groups = x$groups[1])
      })
      if (ncol(ell) > 2){
        names(ell)[1:2] <- c("xvar", "yvar")
        g <- g + geom_path(data = ell, aes(color = groups, group = groups))
      }
    }
    if (var.axes) {
      g <- g + geom_text(data = df.v, aes(label = varname,
                                          x = xvar, y = yvar, vjust= 1.2, hjust="inward"),
                         color = "black", size = varname.size)
    }
    return(g)
  }
