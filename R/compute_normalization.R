#' @title compute_normalization
#'
#' @description Compute normalization with various methods and export to xlsx
#'
#' @param datalist A list of dataframe containing peaktable, sample info
#'    and feature info
#' @param norm.method Normalization method to use, choose from c("bestis", "low_cv",
#'    "pqn", "nomis", "qspline", "loess", "sum", "median", "combat", "limma")
#' @param path.result Path for export output
#' @param prefix Prefix for names of output files
#' @param batch.wise If normalize within batch only
#' @param po.sample.to.use Pool sample type to use for evaluating normalization
#'    performance
#'
#' @return Normalized data in xlsx files
#' @examples
#'
#' @export
#' @import openxlsx
#'
compute_normalization <-
  function( datalist = NULL,
            norm.method = NULL,
            path.result = NULL,
            prefix = NULL,
            batch.wise = NULL,
            po.sample.to.use = NULL,
            feature.info = NULL
  ){

    data <- datalist$data
    data.istd <- datalist$data.istd
    sample.info <- datalist$sample.info
    feature.info <- datalist$feature.info

    process.list <- list()
    #process.list$data.istd <- datalist$data.istd
    process.list$raw.data <- datalist$data

    print("The following methods were used for normalization:", quote = FALSE)
    print(norm.method)

    ### RSD original
    data.rsd.orig <-
      calculate_rsd( data = data,
                     type = sample.info$Sample.type,
                     names.suffix = "orig")$type.rsd

    if ("bestis" %in% norm.method){

      result.normalization <-
        normalize_with_best_internal_standard(
          x = data,
          istds = data.istd,
          batch = sample.info$Batch,
          batch.wise = batch.wise,
          type = sample.info$Sample.type,
          use.type = po.sample.to.use
        )

      process.list$bestis <- result.normalization$x

      normalizer <-
        data.frame(Metabolite = feature.info$Identity,
                   Normalizer = result.normalization$best.istd)

      data.rsd <-
        calculate_rsd( data = process.list$bestis,
                       type = sample.info$Sample.type,
                       names.suffix = NULL)$type.rsd

      istd.rsd <-
        calculate_rsd( data = data.istd,
                       type = sample.info$Sample.type,
                       names.suffix = NULL)$type.rsd

      normalizer.output <-
        normalizer %>%
        dplyr::left_join(data.rsd.orig, by = c("Metabolite" = "Identity")) %>%
        dplyr::left_join(data.rsd, by = c("Metabolite" = "Identity")) %>%
        dplyr::rename( Identity = Metabolite) %>%
        dplyr::left_join(feature.info, by = "Identity")

      x.output <-
        cbind( Sample.name = sample.info$Sample.name,
               process.list$bestis)

      wb <- createWorkbook()
      addWorksheet(wb, "Normalized.data")
      writeData(wb, "Normalized.data", x.output)
      addWorksheet(wb, "Normalizer")
      writeData(wb, "Normalizer", normalizer.output)
      addWorksheet(wb, "Istd")
      writeData(wb, "Istd", istd.rsd)
      saveWorkbook(wb, file = paste0(path.result, prefix, "normalised_", "bestis", ".xlsx"), overwrite = TRUE )

    }

    if ("low_cv" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          istds = data.istd,
          batch = sample.info$Batch,
          batch.wise = batch.wise,
          method = 'low_cv',
          type = sample.info$"Sample.type",
          use.type = po.sample.to.use,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$low_cv <- result.normalization$x

    }

    if ("pqn" %in% norm.method){

      if ("Control" %in% sample.info$Sample.group){

        result.normalization <-
          normalize_various_methods(
            x = data,
            method = 'pqn',
            type = sample.info$Sample.type,
            reference = sample.info$Sample.group,
            use.reference = "Control",
            data.rsd.orig = data.rsd.orig,
            export.path = path.result,
            prefix = prefix,
            feature.info = feature.info
          )

      } else {

        result.normalization <-
          normalize_various_methods(
            x = data,
            method = 'pqn',
            type = sample.info$Sample.type,
            reference = sample.info$Sample.group,
            use.reference = po.sample.to.use,
            data.rsd.orig = data.rsd.orig,
            export.path = path.result,
            prefix = prefix,
            feature.info = feature.info
          )

      }

      process.list$pqn <- result.normalization$x

    }

    if ("nomis" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          istds = data.istd,
          method = "nomis",
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$nomis <- result.normalization$x

    }

    if ("qspline" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'qspline',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$qspline <- result.normalization$x

    }

    if ("loess" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'loess',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$loess <- result.normalization$x

    }

    if ("combat" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          batch = sample.info$Batch,
          method = "combat",
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$combat <- result.normalization$x

    }

    if ("limma" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          batch = sample.info$Batch,
          method = "limma",
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$limma <- result.normalization$x

    }

    if ("sum" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'sum',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$sum <- result.normalization$x

    }

    if ("median" %in% norm.method){

      result.normalization <-
        normalize_various_methods(
          x = data,
          method = 'median',
          type = sample.info$Sample.type,
          data.rsd.orig = data.rsd.orig,
          export.path = path.result,
          prefix = prefix,
          feature.info = feature.info
        )

      process.list$median <- result.normalization$x

    }

    return(process.list)

  }
