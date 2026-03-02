#' @title plot_list_pca
#'
#' @description Generate PCA plots for a list of normalized matrices.
#'
#' @param x Named list of matrices/data frames (sample x feature).
#' @param group Optional grouping vector for sample coloring.
#' @param group.label Legend title for grouping variable.
#' @param plotly.text Optional hover labels.
#'
#' @return A named list of PCA ggplot objects.
#' @examples
#' \dontrun{
#' pca_plots <- plot_list_pca(peaks_norm, group = sample_info$Sample.type)
#' }
#' @export
#' @import ggplot2 ggsci

plot_list_pca <-
  function (
    x = NULL,
    group = NULL,
    group.label = NULL,
    plotly.text = NULL
  ){

    if (!is.list(x) || length(x) == 0) {
      stop("x must be a non-empty list")
    }

    plt.pca <-
      lapply(x, function(x) {
        plot_PCA(
          x = x,
          group = group,
          print = FALSE,
          group.label = group.label,
          plotly.text = plotly.text)
      })

    return(plt.pca)

  }
