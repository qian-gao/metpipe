#' @title calculate_add_rsd
#'
#' @description Calculate mean and rsd of features in a format to add to the
#'              feature info later
#'
#' @param data A dataframe of peaktable, sample x feature
#' @param type A vector indicating sample type
#'
#' @return A dataframe object that contains a summary of features (mean and rsd)
#' @examples
#'
#' @export
#'
calculate_add_rsd <-
  function( data,
            type){

    rsd <-
      calculate_rsd( data = data,
                     type = type,
                     impute = TRUE)

    tm <- rsd$type.mean
    colnames(tm) <- paste0( "mean.", colnames(tm))
    trsd <- rsd$type.rsd
    colnames(trsd) <- paste0( "rsd.", colnames(trsd))

    summary <-
      cbind(tm, trsd[, -1]) %>%
      dplyr::rename(Identity = mean.Identity)

    return(summary)

  }
