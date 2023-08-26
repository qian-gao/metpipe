#' @title normalize_various_methods
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
#' @import reshape2
#' @import tidyverse
#'
normalize_various_methods <-
  function(
    x,
    istds = NULL,
    method = NULL,
    type = NULL,
    use.type = NULL,
    reference = NULL,
    use.reference = NULL,
    group = NULL,
    verbose = FALSE,
    batch = NULL,
    batch.wise = FALSE,
    sample.rate = 0.33,
    data.rsd.orig = NULL,
    export.path = NULL,
    prefix = "",
    feature.info = NULL

  ) {

    #library(crmn)
    #library(reshape2)
    #library(tidyr)
    #library(dplyr)

    if ( verbose ) {

      print( "normalize_various_method was created by Qian Gao" )
      print( "qian.gao@sund.ku.dk" )
      print( "2021-05-14" )

    }

    x_rownames <- rownames(x)
    x_colnames <- colnames(x)

    if (method == 'unit') {

      x_norm <- t(apply(x, 1, function(x) {x/sqrt(sum(x^2, na.rm = TRUE))}))

    } else if (method == 'sum') {

      sum.median <- median( rowMeans(x, na.rm = TRUE), na.rm = TRUE)
      x_norm <- t(apply(x, 1, function(x) {x/sum(x, na.rm = TRUE) * sum.median }))

    } else if (method == 'median') {

      median.median <- median( apply(x, 1, function(x) median(x, na.rm = TRUE)), na.rm = TRUE)
      x_norm <- t(apply(x, 1, function(x) {x/median(x, na.rm = TRUE) * median.median }))

    } else if (method == 'pqn') {

      x_sum_norm <- t(apply(x, 1, function(x) {x/sum(x, na.rm = TRUE)}))

      if (!is.null(type) && !is.null(use.type)) {

        spectrum.ref <- apply(x[ type == use.type, ], 2, function(x) {median(x, na.rm = TRUE)})

      } else {

        spectrum.ref <- apply(x, 2, function(x) {median(x, na.rm = TRUE)}) # !!! Need to add warning messages
        print("No control group specified, median of all samples are used as reference")
      }

      quotient <- x_sum_norm/spectrum.ref
      probabilistic_q <- apply(quotient, 1, function(x) {median(x, na.rm = TRUE)})
      x_norm <- x_sum_norm/probabilistic_q

    } else if (method == 'nomis') {

      pre_norm <- cbind(istds, x)
      istds_vec <- c(rep(TRUE, ncol(istds)), rep(FALSE, ncol(x)))

      x_norm <- t(crmn::normalize(t(pre_norm), "nomis", standards=istds_vec, lg = TRUE))

    } else if (method == 'loess') {

      mat <- t(x)
      mat_norm <-
        affy_normalize_loess( mat, subset = sample(1:(dim(mat)[1]), min(c(5000, nrow(mat)))),
                             epsilon = 10^-2, maxit = 1, log.it = TRUE, verbose = TRUE,
                             span = 2/3, family.loess = "symmetric")
      x_norm <- t(mat_norm)

    } else if (method == 'qspline') {

      mat <- t(x)
      # Add the choice of target, samples
      target <- apply(x, 1, function(x) {median(x, na.rm = TRUE)} )
      mat_norm <-
        affy_normalize_qspline(mat, target = target, samples = sample.rate,
                          fit.iters = 5, min.offset = 5,
                          spline.method = "natural", smooth = TRUE,
                          spar = 0, p.min = 0, p.max = 1.0,
                          incl.ends = TRUE, converge = FALSE,
                          verbose = TRUE, na.rm = FALSE)
      x_norm <- t(mat_norm)
      rownames(x_norm) <- x_rownames
      colnames(x_norm) <- x_colnames

    } else if (method == 'combat') {

      mat <- log2(t(x))

      if (!is.null(group)){

        mod <- model.matrix(~ group) # Include covariate into correction
        mat_norm <- sva::ComBat(mat, batch, mod)

      } else {

        mat_norm <- sva::ComBat(mat, batch)

      }

      x_norm <- data.frame(2^(t(mat_norm)))

      rownames(x_norm) <- x_rownames
      colnames(x_norm) <- x_colnames

    } else if (method == 'low_cv') {

      if (batch.wise == FALSE){
        batch <- rep("No.batches", nrow(x))
      }

      batch_nr <- unique(batch)
      x_norm <- data.frame()
      minRSD <- data.frame()

      for (i in 1:length(batch_nr)){

        x.i <- x[batch == batch_nr[i], ]
        istds.i <- istds[batch == batch_nr[i], ]

        mat.raw <- x.i[ type == use.type, ]
        mat <- reshape2::melt(cbind(rownames(mat.raw), mat.raw))
        colnames(mat) <- c('Sample', 'Metabolite', 'Intensity.raw')

        mat.raw <- data.frame(mat.raw)
        mat.istds <- data.frame(istds.i[ type == use.type, ])

        # Calculate mean values for each IS
        mat.is.means <- apply(mat.istds, 2, function(x) {mean(x, na.rm = TRUE)})

        # Normalize to each internal Standard
        for (j in 1:length(mat.is.means)) {

          norms <- mat.raw  %>%
            sapply(FUN = function(x) x/mat.istds[ , j]) %>%
            as_data_frame %>%
            gather(Metabolite, Area_norm)

          norms <- norms %>% mutate(Area_norm = Area_norm*mat.is.means[j])

          key <- ncol(mat)
          mat[, key + 1] <- norms$Area_norm
          names(mat)[ key + 1 ] <- colnames(istds.i)[j]

        }

        # Make some BMIS evaluations here based on the Internal Standards

        mat.norm <- reshape2::melt(mat, id = c('Sample', 'Metabolite'))
        RSD <- mat.norm %>%
                 group_by(Metabolite, variable) %>%
                 dplyr::summarise(RSD = sd(value, na.rm = TRUE)/mean(value, na.rm = TRUE))

        # minRSD.i <- RSD %>% # to improve
        #             group_by(Metabolite) %>%
        #             dplyr::summarise(variable, RSD.min = min(RSD, na.rm = TRUE)) %>%
        #             left_join(RSD, by=c('Metabolite', 'RSD.min' = 'RSD')) %>%
        #             select(Metabolite, RSD.min, variable.y) %>%
        #             unique() %>%
        #             ungroup() %>%
        #             rename(Normalizer = variable.y) %>%
        #             mutate(Normalizer = if_else(Normalizer == 'Intensity.raw', 'NA', as.character(Normalizer)),
        #                    Batch = batch_nr[i])

        minRSD.i <-
          RSD %>%
            group_by(Metabolite) %>%
            arrange(Metabolite, RSD) %>%
            filter(row_number() == 1) %>%
            ungroup() %>%
            dplyr::rename(Normalizer = variable) %>%
            mutate(Normalizer = if_else(Normalizer == 'Intensity.raw', 'NA', as.character(Normalizer)),
                   Batch = batch_nr[i])

        # Normalize based on min RSD
        x_norm.i <- x.i
        is.means <- apply(istds.i, 2, function(x) {mean(x, na.rm = TRUE)})

        for (k in 1:nrow(minRSD.i)) {
          met.n <- as.matrix(minRSD.i[k, 'Metabolite'])
          is.n <- as.matrix(minRSD.i[k, 'Normalizer'])
          if (is.n != 'NA'){
            x_norm.i[ , met.n] <- x_norm.i[ , met.n]/istds.i[ , is.n]*is.means[is.n]
          }
        }

        x_norm <- rbind(x_norm, x_norm.i)
        minRSD <- rbind(minRSD, minRSD.i)

        x_norm <- x_norm[rownames(x), ]

      }

      # map.names <- colnames(istds)
      # names(map.names) <- make.names(map.names)

      minRSD <-
        # test <-
        minRSD %>%
        # mutate(Normalizer = if_else( Normalizer == "NA",
        #                             Normalizer,
        #                             map.names[Normalizer]) ) %>%
        pivot_wider(-RSD, names_from = Batch, values_from = Normalizer) %>%
        arrange(match(Metabolite, colnames(x)))


    }  else if (method == 'limma') {

      mat <- log2(t(x))
      mat_norm <-
        limma::removeBatchEffect(mat, batch = batch)

      x_norm <- data.frame(2^(t(mat_norm)))

      rownames(x_norm) <- x_rownames
      colnames(x_norm) <- x_colnames
    }

    if (method == 'low_cv'){

      output <-
        list( x = data.frame(x_norm, check.names = FALSE),
              method = method,
              normalizer = minRSD)

    } else {

      output <-
        list( x = data.frame(x_norm, check.names = FALSE),
              method = method)

    }

    if (is.null(data.rsd.orig)){
      data.rsd.orig <-
        calculate_rsd( data = x,
                       type = type,
                       names.suffix = "rsd.orig")$type.rsd
    }

    data.rsd <-
      calculate_rsd( data = output$x,
                     type = type,
                     names.suffix = "rsd")$type.rsd

    output$rsd <-
      data.rsd.orig %>%
      left_join(data.rsd, by = "Identity")

    if ( "normalizer" %in% names(output)){

      output$normalizer <-
        output$normalizer %>%
        left_join(output$rsd, by = c("Metabolite" = "Identity")) %>%
        dplyr::rename(Identity = Metabolite)

    } else {

      names(output)[names(output) == "rsd"] <- "normalizer"

    }

    if (!is.null(feature.info)) {

      output$normalizer <-
        output$normalizer %>%
        dplyr::left_join(feature.info, by = "Identity")

    }

    if (!is.null(export.path)){

      library(openxlsx)

      if ( !is.null(istds)){

        istd.rsd <-
          calculate_rsd( data = istds,
                         type = type,
                         names.suffix = NULL)$type.rsd

        x.output <-
          output$x %>%
          rownames_to_column("Sample.name")

        wb <- createWorkbook()
        addWorksheet(wb, "Normalized.data")
        writeData(wb, "Normalized.data", x.output)
        addWorksheet(wb, "Normalizer")
        writeData(wb, "Normalizer", output$normalizer)
        addWorksheet(wb, "Istd")
        writeData(wb, "Istd", istd.rsd)
        saveWorkbook(wb, file = paste0(export.path, prefix, "normalised_", method, ".xlsx"), overwrite = TRUE )

      } else {

        x.output <-
          output$x %>%
          rownames_to_column("Sample.name")

        wb <- createWorkbook()
        addWorksheet(wb, "Normalized.data")
        writeData(wb, "Normalized.data", x.output)
        addWorksheet(wb, "Normalizer")
        writeData(wb, "Normalizer", output$normalizer)

        if (!is.null(istds)){

          istd.rsd <-
            calculate_rsd( data = istds,
                           type = type,
                           names.suffix = NULL)$type.rsd

          addWorksheet(wb, "Istd")
          writeData(wb, "Istd", istd.rsd)

        }

        saveWorkbook(wb, file = paste0(export.path, prefix, "normalised_", method, ".xlsx"), overwrite = TRUE )

      }

    }

    return( output )

  }

#' @title affy_normalize_loess
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
#'
affy_normalize_loess <-
  function(mat, subset=sample(1:(dim(mat)[1]), min(c(5000, nrow(mat)))),
           epsilon=10^-2, maxit=1, log.it=TRUE, verbose=TRUE, span=2/3,
           family.loess="symmetric"){

    J <- dim(mat)[2]
    II <- dim(mat)[1]
    if(log.it){
      mat <- log2(mat)
    }

    change <- epsilon +1
    iter <- 0
    w <- c(0, rep(1,length(subset)), 0) ##this way we give 0 weight to the
    ##extremes added so that we can interpolate

    while(iter < maxit){
      iter <- iter + 1
      means <- matrix(0,II,J) ##contains temp of what we substract

      for (j in 1:(J-1)){
        for (k in (j+1):J){
          y <- mat[,j] - mat[,k]
          x <- (mat[,j] + mat[,k]) / 2
          index <- c(order(x)[1], subset, order(-x)[1])
          ##put endpoints in so we can interpolate
          xx <- x[index]
          yy <- y[index]
          aux <-loess(yy~xx, span=span, degree=1, weights=w, family=family.loess)
          aux <- predict(aux, data.frame(xx=x)) / J
          means[, j] <- means[, j] + aux
          means[, k] <- means[, k] - aux
          if (verbose)
            cat("Done with",j,"vs",k,"in iteration",iter,"\n")
        }
      }
      mat <- mat - means
      change <- max(colMeans((means[subset,])^2))

      if(verbose)
        cat(iter, change,"\n")

    }

    if ((change > epsilon) & (maxit > 1))
      warning(paste("No convergence after", maxit, "iterations.\n"))

    if(log.it) {
      return(2^mat)
    } else
      return(mat)
  }

#' @title affy_normalize_qspline
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
#'
affy_normalize_qspline <- function(x,
                                   target        = NULL,
                                   samples       = NULL,
                                   fit.iters     = 5,
                                   min.offset    = 5,
                                   spline.method = "natural", # c("fmm", "natural", "periodic")
                                   smooth        = TRUE,
                                   spar          = 0,     # smoothing parameter
                                   p.min         = 0,
                                   p.max         = 1.0,
                                   incl.ends     = TRUE,
                                   converge      = FALSE,
                                   verbose       = TRUE,
                                   na.rm         = FALSE
){

  if (is.null(target))
    target <- exp(apply(log(x), 1, mean))

  x.n <- dim(x)[1]
  m   <- dim(x)[2]

  if (is.null(samples))
    samples <- max(round(x.n/1000), 100)
  else
    if (samples < 1)
      samples <- round(samples * x.n)

  p <- (1:samples) / samples
  p <- p[ which(p <= p.max) & which(p >= p.min) ]
  samples <- length(p)

  k <- fit.iters

  if (na.rm==TRUE)
    y.n <- sum(!is.na(target))
  else
    y.n <- length(target)

  py.inds  <- as.integer(p * y.n)
  y.offset <- round(py.inds[1]/fit.iters)

  if (y.offset <= min.offset) {
    y.offset <- min.offset;
    k <- round(py.inds[1]/min.offset)
  }

  if (k <= 1) {
    warning("'k' found is non-sense. using default 'fit.iter'")
    k <- fit.iters
  }

  y.offset <- c(0, array(y.offset, (k-1)))
  y.order <- order(target)

  fx <- matrix(0, x.n,m)
  if(verbose==TRUE)
    print(paste("samples=",samples, "k=", k, "first=", py.inds[1]))

  for (i in 1:m) {
    # to handel NA values for each array
    if (na.rm==TRUE)
      x.valid <- which(!is.na(x[,i]))
    else
      x.valid <- 1:x.n

    x.n <- length(x.valid)
    px.inds  <- as.integer(p * x.n)
    x.offset <- round(px.inds[1]/fit.iters)

    if (x.offset<=min.offset) {
      x.offset <- min.offset;
      k <- min(round(px.inds[1]/min.offset), k)
    }

    x.offset <- c(0, array(x.offset, (k-1)))
    x.order  <- order(x[,i]) # NA's at the end (?)

    y.inds   <- py.inds ## must be reset each iteration
    x.inds   <- px.inds

    for (j in 1:k) {
      y.inds <- y.inds - y.offset[j]
      x.inds <- x.inds - x.offset[j]
      ty.inds <- y.inds
      tx.inds <- x.inds
      if (verbose==TRUE)
        print(paste("sampling(array=", i, "iter=", j, "off=",
                    x.inds[1], -x.offset[j], y.inds[1], -y.offset[j], ")"))

      if (converge==TRUE) {
        ty.inds <- as.integer(c(1, y.inds))
        tx.inds <- as.integer(c(1, x.inds))

        if (j > 1) {
          ty.inds <- c(ty.inds, y.n)
          tx.inds <- c(tx.inds, x.n)
        }
      }
      qy <- target[y.order[ty.inds]]
      qx <-  x[x.order[tx.inds],i]

      if (smooth==TRUE) {
        sspl <- smooth.spline(qx, qy, spar=spar)
        qx <- sspl$x
        qy <- sspl$y
      }

      fcn <- splinefun(qx, qy, method=spline.method)
      fx[x.valid,i] <- fx[x.valid,i] + fcn(x[x.valid,i])/k
    }

    if (na.rm==TRUE) {
      invalid <- which(is.na(x[,i]))
      fx[invalid,i] <- NA
    }
  }
  return(fx)
}
