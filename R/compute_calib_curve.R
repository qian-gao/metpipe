#' @title compute_calib_curve
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param dat A data set object
#' @param id Scope (e.g., country codes or individual IDs)
#' @param time Time (e.g., time periods are given by years, months, ...)
#'
#' @return A data frame object
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%" mutate left_join group_by ungroup
# Usage
# raw.file <- "202111_Reduction_IS_dilution_test_data/Calibration_lip1_pos_IS_dilution_02032022.xlsx"
# tech.rep <- 2
# concentrations <- c(0, 0.001, 0.025, 0.1, 0.25,
#                     1, 2.5, 10, 40, 160)
#
# models <-
#   compute_calib_curve( raw.file = "202111_Reduction_IS_dilution_test_data/Calibration_lip1_pos_IS_dilution_02032022.xlsx",
#                        tech.rep = 2,
#                        concentrations = c(0, 0.001, 0.025, 0.1, 0.25,
#                                           1, 2.5, 10, 40, 160))

compute_calib_curve <-
  function( raw.file,
            concentration.file,
            dilution.nr,
            k = NULL, # If specify k, it should be a vector, each row indicates one identity and its k
            choose.normalizer = FALSE
    ){

    #library(tidyverse)

    if (!is.data.frame(raw.file)){

      raw <-
        openxlsx::read.xlsx(raw.file, sheet = 1)

    } else {

      raw <- raw.file

    }

    if (!is.data.frame(concentration.file)){

      concentration <- openxlsx::read.xlsx(concentration.file, sheet = 1)

    } else {

      concentration <- concentration.file

    }

    concentration <-
      concentration %>%
      tidyr::pivot_longer(-Identity, names_to = "Sample", values_to = "Concentration")

    data <-
      raw %>%
      dplyr::rename( Identity = 1,
              Normalizer = 2) %>%
      tidyr::pivot_longer( !c(Identity, Normalizer),
                    names_to = "Sample",
                    values_to = "Intensity" ) %>%
      left_join( concentration, by = c("Identity", "Sample")) %>%
      group_by( Identity, Normalizer, Concentration ) %>%
      mutate( Rep = row_number()) %>%
      ungroup()

    mods <- list()
    devis <- data.frame()
    ids <- unique(data$Identity)
    norms <- unique(data$Normalizer)

    if (is.null(k)) {

      k <- matrix(rep(dilution.nr, length(ids)), nrow = length(ids), ncol = length(norms))
      rownames(k) <- ids
      colnames(k) <- norms

    }

    for (i in 1:length(ids)){

      data.i <- data[data$Identity == ids[i], ]

      mod.i <- lapply( norms,
                       function(x, data){
                         d <- data[data$Normalizer == x, ]
                         k.i.x <- k[i, x]

                         mod <- NULL
                         repeatn <- 1
                         while( is.null(mod) && repeatn <= 50 && k.i.x > 0) {
                           # print(repeatn)
                           # print(k.i.x)
                           repeatn <- repeatn + 1
                           try(
                             mod <- mgcv::gam( Intensity ~ s(Concentration, k = k.i.x),
                                               data = d, method = "REML")
                           )
                           k.i.x <- k.i.x - 1
                         }

                         return(mod)
                         # mod <- mgcv::gam( Intensity ~ s(Concentration, k = k.i),
                         #                   data = d, method = "REML")
                       },
                       data = data.i)

      names(mod.i) <- norms

      k.i <- t(sapply(mod.i, function(x){
        length(x$coefficients)
      }))

      k[i, ] <- k.i

      devi <- t(sapply(mod.i, function(x){
                                a <- summary(x)
                                b <- a$dev.expl

                                return(b)
                            })) %>% as.data.frame()

      devis <- rbind(devis, devi)
      mods[[ids[i]]] <- mod.i
    }

    names(mods) <- ids
    rownames(devis) <- ids

    # Output the whole grid of k
    k.w <-
      k %>%
      as.data.frame()

    result <- list( mods = mods,
                    k = k.w,
                    devis = devis)

    if (choose.normalizer == TRUE){

      result <-
        choose_normalizer_from_calibration( mods = result$mods,
                                            devis = result$devis,
                                            k = result$k)

    }

    data <-
      data  %>%
      mutate(x = Concentration,
             fit = Intensity)

    data.split <- split(data, f = data$Identity)
    data.split <- lapply(data.split, function(x){ split(x, f = x$Normalizer) })

    result$data <- data.split
    return(result)

  }

#' @title choose_normalizer_from_calibration
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
#'
choose_normalizer_from_calibration <-
  function( mods = NULL,
            devis = NULL,
            k = NULL,
            selection = NULL # if not null, should be a matrix with same dimension as devis, values are TRUE or FALSE indicating force selection
    ){

    devi.max.ind <-
      sapply(1:nrow(devis),
             function(x){
               sel <- which(selection[x, ] == TRUE)
               if (length(sel) == 0){
                  which.max(devis[x, ])
               } else {
                  sel
               }
         })

    devi.max <-
      sapply(1:nrow(devis), function(x){ devis[x, devi.max.ind[x]]})

    k.choose <-
      sapply(1:nrow(k), function(x){ k[x , devi.max.ind[x]] })

    normalizer.choose <-
      data.frame( Identity = rownames(devis),
                  Normalizer = colnames(devis)[devi.max.ind],
                  k = k.choose,
                  deviance = devi.max)

    mods.choose <-
      apply(normalizer.choose, 1, function(x){ mods[[x["Identity"]]][[x["Normalizer"]]]})

    names(mods.choose) <- normalizer.choose$Identity

    result <- list( mods = mods.choose,
                    normalizer = normalizer.choose)

    return(result)
  }
