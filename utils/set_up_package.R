library(roxygen2) # In-Line Documentation for R
library(devtools) # Tools to Make Developing R Packages Easier
library(testthat) # Unit Testing for R
library(usethis)  # Automate Package and Project Setup

### check if name available
# available::available("metpipe", browse = FALSE)

### generate help file in man and new NAMESPACE file
setwd("H:/Documents/CBMR_workflow/packages/metpipe")
devtools::document()

### intall
devtools::install_github("https://github.com/qian-gao/metpipe",
                         auth_token = "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")

### function description

#' @title overview_tab
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
#' @importFrom dplyr "%>%" mutate left_join arrange select

