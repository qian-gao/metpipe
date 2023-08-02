#' @title select_precursor
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param XCMSnExp = NULL,
#' @param mode = NULL,
#' @param path.result = NULL
#'
#' @return A data frame object that contains a summary of a sample that
#'     can later be converted to a TeX output using \code{overview_print}
#' @examples
#' @export
#' @importFrom dplyr "%>%" mutate select
#'
select_precursor <-
  function( XCMSnExp = NULL,
            #method = NULL,
            mode = NULL,
            path.result = NULL
  ){

    if ( file.exists(paste0(path.result, "precursor_", mode, ".rds")) ){

      precursor <-
        readRDS( paste0(path.result, "precursor_", mode, ".rds"))

    } else {

      mzrt <-
        XCMSnExp_mzrt( XCMSnExp = XCMSnExp,
                       mzdigit = 4,
                       rtdigit = 1,
                       method = "medret",
                       value = "into")

      ### PMD
      pmd <- pmd::globalstd( mzrt,
                             sda = F,
                             ng = NULL)

      pmd.cluster <- pmd::getcluster( pmd,
                                      corcutoff = 0.9)

      precursor <-
        cbind.data.frame( mz = pmd.cluster$mz[ pmd.cluster$stdmassindex2 ],
                          rt = pmd.cluster$rt[ pmd.cluster$stdmassindex2 ],
                          mzrt$data[ pmd.cluster$stdmassindex2, ]) %>%
        mutate( id = row_number()) %>%
        select( id, rt, mz, colnames(mzrt$data))  %>%
        mutate( rt = rt / 60)

      saveRDS( precursor,
               file = paste0(path.result, "precursor_", mode, ".rds") )

    }

    return(precursor)

  }
