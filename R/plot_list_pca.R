#' @title plot_list_pca
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

plot_list_pca <-
  function (
    x = NULL,
    group = NULL,
    group.label = NULL,
    plotly.text = NULL
  ){

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
