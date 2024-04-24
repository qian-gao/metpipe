#' @title rt_mapping
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
#' @importFrom dplyr "%>%" rename mutate arrange select

rt_mapping <-
  function( ref.file,
            cor_threshold = 0.5,
            mz_tolerance = 0.01){

    #library(tidyverse)

    ##### Import

    ### Reference files
    sheets <- openxlsx::getSheetNames(ref.file)

    if (length(sheets) == 2){

      ref <-
        openxlsx::read.xlsx(ref.file, sheet = 1) %>%
        dplyr::rename(RT.old = 1,
               RT.new = 2) %>%
        mutate(RT.old = as.numeric(RT.old),
               RT.new = as.numeric(RT.new) )%>%
        arrange(RT.old)

    } else if (length(sheets) == 3){

      ref.1 <-
        openxlsx::read.xlsx(ref.file, sheet = 1) %>%
        dplyr::rename(rt = 1,
               mz = 2,
               identity = 3) %>%
        mutate(rt = as.numeric(rt))

      ref.2 <-
        openxlsx::read.xlsx(ref.file, sheet = 2) %>%
        dplyr::rename(rt = 1,
               mz = 2,
               identity = 3) %>%
        mutate(rt = as.numeric(rt))

      # Extract features with mz difference < mz_tolerance and correlation > cor_threshold
      mz.diff <- outer(ref.1$mz, ref.2$mz, "-")
      mz.logi <- abs(mz.diff) < mz_tolerance

      corr <-
        cor( t(ref.1[, 4:ncol(ref.1)]),
             t(ref.2[, 4:ncol(ref.2)]),
             use = "pairwise.complete.obs",
             method = "pearson")
      corr.logi <- corr > cor_threshold

      pair.chosen <- mz.logi & corr.logi
      pair.chosen[is.na(pair.chosen)] <- FALSE

      index <- which(pair.chosen == TRUE, arr.ind = TRUE)

      ref <-
        t(apply( index,
                 1,
                 function(x){

                   cbind(ref.1$rt[x[1]], ref.2$rt[x[2]], ref.1$identity[x[1]], ref.2$identity[x[2]])

                 })) %>%
        as.data.frame() %>%
        rename(RT.old = 1,
               RT.new = 2,
               Identity.old = 3,
               Identity.new = 4) %>%
        mutate(RT.old = as.numeric(RT.old),
               RT.new = as.numeric(RT.new) )%>%
        arrange(RT.old)

    }

    return(ref)
  }

#' @title rt_mapping_predict
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
#' @importFrom dplyr "%>%" rename mutate arrange select
#'
rt_mapping_predict <-
  function( ref.file,
            ref ){

    #library(tidyverse)

    ### Files to predict
    sheets <- openxlsx::getSheetNames(ref.file)

    if (length(sheets) == 2){

      pred.input <-
        openxlsx::read.xlsx(ref.file, sheet = 2, skipEmptyRows = FALSE) %>%
        rename(RT.to.pred = 1)

    } else if (length(sheets) == 3){

      pred.input <-
        openxlsx::read.xlsx(ref.file, sheet = 3) %>%
        rename(RT.to.pred = 1)

    }

    ##### Fit on matched features
    ref <-
      ref %>%
      arrange(RT.old)

    fit <- pracma::ppfit(ref$RT.old,
                         ref$RT.new,
                         xi = ref$RT.old,
                         method = "cubic") # linear

    ##### Prediction

    ### Impute -99 for missings
    pred.input$RT.to.pred <-
      ifelse(is.na(pred.input$RT.to.pred), -99, pred.input$RT.to.pred)

    pred <- pracma::ppval(fit, pred.input$RT.to.pred)

    ### Out of range part
    out.range <- Hmisc::approxExtrap(ref$RT.old,
                                     ref$RT.new,
                                     pred.input$RT.to.pred,
                                     method = "linear",
                                     rule = 2,
                                     ties = mean,
                                     na.rm = FALSE)

    result <-
      cbind(pred.input,
            RT.predicted = pred,
            RT.out.range = out.range$y) %>%
      # data.frame( RT.to.pred = pred.input$RT.to.pred,
      #             RT.predicted = pred,
      #             RT.out.range = out.range$y) %>%
      mutate( RT.to.pred = if_else(RT.to.pred == -99, as.numeric(NA), RT.to.pred),
              RT.predicted = if_else(is.na(RT.predicted), RT.out.range, RT.predicted),
              RT.predicted = if_else(is.na(RT.to.pred), as.numeric(NA), RT.predicted),
              diff = RT.predicted - RT.to.pred) %>%
      select(-RT.out.range)

    #openxlsx::write.xlsx(new, "Predicted_RT.xlsx")

    return(result)
  }
