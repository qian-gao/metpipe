#' @title compute_lmm
#'
#' @description Provides an overview table for the time and scope conditions of
#'     a data set
#'
#' @param data = NULL,
#' @param formula = NULL,
#' @param interaction = NULL,
#' @param covariates = NULL,
#' @param random.effect = NULL,
#' @param dv = NULL,
#' @param path.result = NULL,
#' @param map.names = NULL
#'
#' @return A dataframe
#' @examples
#' @export
#' @import tidyverse limma

compute_lmm <-
  function(data = NULL,
           formula = NULL,
           interaction = NULL,
           random.effect = NULL,
           dv = NULL,
           path.result = NULL,
           map.names = NULL
           ){

    library(tidyverse)

    iv <- strsplit(formula, "[*+ ~]")[[1]]
    iv <- iv[iv != ""]

    design.data <-
      data.frame(data[, c(iv, random.effect), drop = FALSE])

    ind <- apply(!is.na(design.data), 1, all)

    design.data <- design.data[ind, , drop = FALSE]

    design.data <- droplevels(design.data)

    data.test <- data[ind, dv, drop = FALSE]
    data.test <- scale(data.test)
    data.test <- t(data.test)

    if (is.null(map.names)){

      map.names <- rownames(data.test)
      names(map.names) <- map.names

    }

    formula <- as.formula(formula)

    design.matrix <-
      stats::model.matrix(object = formula,
                          data = design.data)

    if ( is.null( random.effect ) ) {

      mFit <-
        limma::lmFit(object = data.test,
                     design = design.matrix)

    } else {

      block <-
        factor(x = design.data[, random.effect])

      cFit <-
        limma::duplicateCorrelation(object = data.test,
                                    design = design.matrix,
                                    block = block)

      mFit <-
        limma::lmFit(object = data.test,
                     design = design.matrix,
                     block = block,
                     correlation = cFit$"consensus.correlation" )

    }

    fit <- limma::eBayes(fit = mFit)

    coef <- colnames(fit$coefficients)[-1]

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

    if (length(interaction) > 1){
      dm.contra <- design.matrix
      index.covar <- !grepl( paste0("^", paste(interaction, collapse = "|")),
                             colnames(dm.contra))
      dm.contra[, index.covar] <- 0
      dm.contra <- unique(dm.contra)


      groups <-
        design.data[ rownames(dm.contra), interaction] %>%
        rownames_to_column("Row")

      groups$Name <-
        do.call(paste, c(groups[, interaction], sep = " "))

      groups.comb <-
        data.frame(t(combn(groups$Row, 2))) %>%
        rownames_to_column("No") %>%
        pivot_longer(-No, names_to = "grp", values_to = "Row") %>%
        left_join(groups, by = "Row") %>%
        arrange(No, desc(across(all_of(interaction)))) %>%
        group_by(No) %>%
        mutate(grp = paste0("X", row_number())) %>%
        ungroup() %>%
        select(No, grp, Name, Row) %>%
        pivot_wider(names_from = grp, values_from = c(Name, Row)) %>%
        select(-No)


      contra <-
        apply( groups.comb,
               1,
               function(x){
                 c <- paste0(x["Name_X1"], " - ", x["Name_X2"])
                 d <- dm.contra[x["Row_X1"], ] - dm.contra[x["Row_X2"], ]
                 c(c,d)
               })

      contra.names <- contra[1,]
      contra <-
        matrix( as.numeric(contra[-1,]),
                ncol = nrow(groups.comb))

      ph.tbl <- list()
      for (i in 1:length(contra.names)){

        contrast <- rbind(as.numeric(contra[, i]))
        cfit <- limma::contrasts.fit(mFit, t(contrast))
        efit <- limma::eBayes(cfit)
        ph.tbl[[i]] <-
          limma::topTable( efit,
                           n = Inf,
                           sort.by = "none",
                           confint = TRUE) %>%
          tibble::rownames_to_column("Variable") %>%
          dplyr::mutate(Variable = map.names[Variable])

        colnames(ph.tbl[[i]]) <- c("variable", "estimate", "CI.L", "CI.H", "AveExpr", "t", "p.value", "adj.p.value", "B")

      }

      names(ph.tbl) <- contra.names

      ph.tbl.all <-
        data.table::rbindlist(ph.tbl, idcol = "contrast")

      result <-
        list( model = fit,
              formula = formula,
              coef.tbl = coef.tbl,
              coef.tbl.all = coef.tbl.all,
              ph.tbl = ph.tbl,
              ph.tbl.all = ph.tbl.all)

      if (!is.null(path.result)){

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Coefficient")
        openxlsx::writeData(wb, "Coefficient", coef.tbl.all)
        openxlsx::addWorksheet(wb, "Contrast")
        openxlsx::writeData(wb, "Contrast", ph.tbl.all)

        openxlsx::saveWorkbook( wb,
                                file = paste0(path.result, "/coefficient_table.xlsx"),
                                overwrite = TRUE)

      }

    } else {

      result <-
        list( model = fit,
              formula = formula,
              coef.tbl = coef.tbl,
              coef.tbl.all = coef.tbl.all)

      if (!is.null(path.result)){

        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Coefficient")
        openxlsx::writeData(wb, "Coefficient", coef.tbl.all)

        openxlsx::saveWorkbook( wb,
                                file = paste0(path.result, "/coefficient_table.xlsx"),
                                overwrite = TRUE)

      }
    }

    return(result)
  }


