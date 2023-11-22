#' @title compute_t
#'
#' @description Compute t test
#'
#' @param data = NULL,
#' @param formula = NULL,
#' @param dv = NULL,
#' @param output.file = NULL,
#' @param map.names = NULL
#'
#' @return A dataframe
#' @examples
#' @export
#' @import tidyverse limma

compute_t <-
  function( data = NULL,
            formula = NULL,
            dv = NULL,
            output.file = NULL,
            map.names = NULL,
            scaling = FALSE
  ){

    iv <- strsplit(formula, "[*+ ~]")[[1]]
    iv <- iv[iv != ""]

    design.data <-
      data.frame(data[, iv, drop = FALSE])

    ind <- apply(!is.na(design.data), 1, all)

    design.data <- design.data[ind, , drop = FALSE]

    design.data <- droplevels(design.data)

    data.test <- data[ind, dv, drop = FALSE]
    if (scaling) data.test <- scale(data.test)
    data.test <- t(data.test)

    if (is.null(map.names)){

      map.names <- rownames(data.test)
      names(map.names) <- map.names

    }

    formula <- as.formula( formula )

    design.matrix <-
      stats::model.matrix( object = formula,
                           data = design.data )



    mFit <-
      limma::lmFit( object = data.test,
                    design = design.matrix )

    fit <- limma::eBayes( fit = mFit )

    coef <- colnames( fit$coefficients )[-1]

    coef.tbl <- list()
    for (i in 1:length(coef)){

      coef.i <- coef[ i ]

      coef.tbl[[ coef.i ]] <-
        limma::topTable( fit,
                         coef = coef.i,
                         n = Inf,
                         sort.by = "none",
                         confint = TRUE) %>%
        tibble::rownames_to_column("Variable") %>%
        dplyr::mutate(Variable = map.names[Variable])

      colnames(coef.tbl[[ coef.i ]]) <- c("variable", "estimate", "CI.L", "CI.H", "AveExpr", "t", "p.value", "adj.p.value", "B")

    }


    coef.tbl.all <-
      data.table::rbindlist(coef.tbl, idcol = "coefficient")

    result <-
      list( model = fit,
            formula = formula,
            coef.tbl = coef.tbl,
            coef.tbl.all = coef.tbl.all)

    if (!is.null(output.file)){

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Coefficient")
      openxlsx::writeData(wb, "Coefficient", coef.tbl.all)

      openxlsx::saveWorkbook( wb,
                              file = output.file,
                              overwrite = TRUE)

    }

    return(result)

  }


