#' @title normalize_various_methods
#'
#' @description Apply a selected normalization method to metabolomics intensity data.
#'
#' @param x Input intensity matrix/data frame (sample x feature).
#' @param istds Optional internal-standard matrix/data frame.
#' @param method Normalization method name.
#' @param type Optional sample-type vector.
#' @param use.type Optional sample type used as reference in selected methods.
#' @param reference Optional reference vector for PQN-style methods.
#' @param use.reference Optional reference label to use within `reference`.
#' @param group Optional covariate used in batch-correction models.
#' @param batch Optional batch vector.
#' @param batch.wise Logical; perform selected methods per batch.
#' @param sample.rate Quantile sampling rate for qspline.
#' @param data.rsd.orig Optional precomputed baseline RSD table.
#' @param export.path Deprecated/optional export path.
#' @param prefix Deprecated/optional file prefix.
#' @param feature.info Optional feature metadata to join into outputs.
#'
#' @return A list containing normalized data and evaluation tables.
#' @examples
#' \dontrun{
#' out <- normalize_various_methods(x = data, method = "median")
#' }
#' @export
#' @import reshape2 dplyr tidyr
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
    batch = NULL,
    batch.wise = FALSE,
    sample.rate = 0.33,
    data.rsd.orig = NULL,
    export.path = NULL,
    prefix = "",
    feature.info = NULL

  ) {

    allowed_methods <- c("unit", "sum", "median", "pqn", "nomis", "loess", "qspline", "combat", "low_cv", "bestis", "limma")
    if (is.null(method) || length(method) != 1 || !method %in% allowed_methods) {
      stop("`method` must be one of: ", paste(allowed_methods, collapse = ", "))
    }

    if (!is.matrix(x) && !is.data.frame(x)) {
      stop("`x` must be a matrix or data.frame")
    }
    if (nrow(x) == 0 || ncol(x) == 0) {
      stop("`x` must have at least one row and one column")
    }

    x <- as.data.frame(x, check.names = FALSE)
    x[] <- lapply(x, as.numeric)

    if (!is.logical(batch.wise) || length(batch.wise) != 1 || is.na(batch.wise)) {
      stop("`batch.wise` must be TRUE or FALSE")
    }

    if (!is.numeric(sample.rate) || length(sample.rate) != 1 || is.na(sample.rate) || sample.rate <= 0 || sample.rate > 1) {
      stop("`sample.rate` must be a numeric scalar in (0, 1]")
    }

    if (method %in% c("combat", "limma") && is.null(batch)) {
      stop("`batch` is required for method='", method, "'")
    }

    if (method %in% c("nomis", "low_cv", "bestis") && is.null(istds)) {
      stop("`istds` is required for method='", method, "'")
    }

    if (method == "bestis" && isTRUE(batch.wise) && is.null(batch)) {
      stop("`batch` is required for method='bestis' when `batch.wise=TRUE`")
    }

    if (method == "pqn" && !is.null(use.type) && is.null(type)) {
      stop("`type` must be provided when `use.type` is set for method='pqn'")
    }

    if (!is.null(type) && length(type) != nrow(x)) {
      stop("`type` must have the same length as nrow(x)")
    }

    if (!is.null(batch) && length(batch) != nrow(x)) {
      stop("`batch` must have the same length as nrow(x)")
    }

    if (!is.null(istds)) {
      istds <- as.data.frame(istds, check.names = FALSE)
      if (nrow(istds) != nrow(x)) {
        stop("`istds` must have the same number of rows as `x`")
      }
      istds[] <- lapply(istds, as.numeric)
    }

    sanitize_norm_matrix <- function(mat, rn, cn) {
      mat <- as.data.frame(mat, check.names = FALSE)
      mat[] <- lapply(mat, as.numeric)
      mat_mat <- as.matrix(mat)
      mat_mat[!is.finite(mat_mat)] <- NA_real_
      mat <- data.frame(mat_mat, check.names = FALSE)
      rownames(mat) <- rn
      colnames(mat) <- cn
      mat
    }

    x_rownames <- rownames(x)
    x_colnames <- colnames(x)

    if (method == 'unit') {

      x_norm <- t(apply(x, 1, function(row_i) {
        den <- sqrt(sum(row_i^2, na.rm = TRUE))
        if (!is.finite(den) || den == 0) {
          rep(NA_real_, length(row_i))
        } else {
          row_i / den
        }
      }))

    } else if (method == 'sum') {

      sum.median <- median( apply(x, 1, function(x) sum(x, na.rm = TRUE)), na.rm = TRUE)
      x_norm <- t(apply(x, 1, function(row_i) {
        den <- sum(row_i, na.rm = TRUE)
        if (!is.finite(den) || den == 0) {
          rep(NA_real_, length(row_i))
        } else {
          row_i/den * sum.median
        }
      }))

    } else if (method == 'median') {

      median.median <- median( apply(x, 1, function(x) median(x, na.rm = TRUE)), na.rm = TRUE)
      x_norm <- t(apply(x, 1, function(row_i) {
        den <- median(row_i, na.rm = TRUE)
        if (!is.finite(den) || den == 0) {
          rep(NA_real_, length(row_i))
        } else {
          row_i/den * median.median
        }
      }))

    } else if (method == 'pqn') {

      x_sum_norm <- t(apply(x, 1, function(row_i) {
        den <- sum(row_i, na.rm = TRUE)
        if (!is.finite(den) || den == 0) {
          rep(NA_real_, length(row_i))
        } else {
          row_i/den
        }
      }))

      if (!is.null(type) && !is.null(use.type)) {
        ref_idx <- type == use.type
        if (!any(ref_idx, na.rm = TRUE)) {
          stop("No samples match `use.type` for method='pqn'")
        }
        spectrum.ref <- apply(x[ref_idx, , drop = FALSE], 2, function(x) {median(x, na.rm = TRUE)})

      } else {

        spectrum.ref <- apply(x, 2, function(x) {median(x, na.rm = TRUE)}) # !!! Need to add warning messages
        print("No control group specified, median of all samples are used as reference")
      }

      spectrum.ref[!is.finite(spectrum.ref) | spectrum.ref == 0] <- NA_real_
      quotient <- x_sum_norm/spectrum.ref
      probabilistic_q <- apply(quotient, 1, function(x) {median(x, na.rm = TRUE)})
      probabilistic_q[!is.finite(probabilistic_q) | probabilistic_q == 0] <- NA_real_
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
        mat_norm <- sva_ComBat(mat, batch, mod)

      } else {

        mat_norm <- sva_ComBat(mat, batch)

      }

      x_norm <- data.frame(2^(t(mat_norm)))

      rownames(x_norm) <- x_rownames
      colnames(x_norm) <- x_colnames

    } else if (method == 'low_cv') {

      if (batch.wise == FALSE){
        batch <- rep("No.batches", nrow(x))
      }

      type_all <- type
      if (is.null(type_all)) {
        type_all <- rep(NA_character_, nrow(x))
      }

      batch_nr <- unique(batch)
      x_norm <- data.frame()
      minRSD <- data.frame()

      for (i in seq_along(batch_nr)){

        batch_idx <- batch == batch_nr[i]
        x.i <- x[batch_idx, , drop = FALSE]
        istds.i <- istds[batch_idx, , drop = FALSE]
        type.i <- type_all[batch_idx]

        if (!is.null(use.type) && !any(type.i == use.type, na.rm = TRUE)) {
          stop("No samples match `use.type` within batch ", batch_nr[i], " for method='low_cv'")
        }

        mat.raw <- x.i[type.i == use.type, , drop = FALSE]
        mat <- reshape2::melt(cbind(rownames(mat.raw), mat.raw))
        colnames(mat) <- c('Sample', 'Metabolite', 'Intensity.raw')

        mat.raw <- data.frame(mat.raw)
        mat.istds <- data.frame(istds.i[type.i == use.type, , drop = FALSE])

        # Calculate mean values for each IS
        mat.is.means <- apply(mat.istds, 2, function(x) {mean(x, na.rm = TRUE)})

        # Normalize to each internal Standard
        for (j in seq_along(mat.is.means)) {

          norms <- mat.raw  %>%
            sapply(FUN = function(x) x/mat.istds[ , j]) %>%
            as.data.frame() %>%
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


    } else if (method == 'bestis') {

      batch_bestis <- batch
      if (!isTRUE(batch.wise) || is.null(batch_bestis)) {
        batch_bestis <- rep("No.batches", nrow(x))
      }

      bestis_out <- normalize_with_best_internal_standard(
        x = x,
        istds = istds,
        batch = batch_bestis,
        batch.wise = batch.wise,
        type = type,
        use.type = use.type
      )

      x_norm <- bestis_out$x

      best_istd_tbl <- data.frame(
        Metabolite = rownames(bestis_out$best.istd),
        bestis_out$best.istd,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

    }  else if (method == 'limma') {

      mat <- log2(t(x))
      mat_norm <-
        limma::removeBatchEffect(mat, batch = batch)

      x_norm <- data.frame(2^(t(mat_norm)))

      rownames(x_norm) <- x_rownames
      colnames(x_norm) <- x_colnames
    }

    x_norm <- sanitize_norm_matrix(x_norm, x_rownames, x_colnames)

    if (method == 'low_cv'){

      output <-
        list( x = data.frame(x_norm, check.names = FALSE),
              method = method,
              normalizer = minRSD)

    } else if (method == 'bestis') {

      output <-
        list( x = data.frame(x_norm, check.names = FALSE),
              method = method,
              normalizer = best_istd_tbl)

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

    if ( !is.null(istds)){

      output$istd.rsd <-
        calculate_rsd( data = istds,
                       type = type,
                       names.suffix = NULL)$type.rsd
    }

    return( output )

  }

#' @title affy_normalize_loess
#'
#' @description Perform iterative loess-based normalization on matrix-like data.
#'
#' @param mat Numeric matrix (feature x sample).
#' @param subset Feature indices used for fitting.
#' @param epsilon Convergence tolerance.
#' @param maxit Maximum number of iterations.
#' @param log.it Logical; normalize in log2 space.
#' @param verbose Logical; print iteration progress.
#' @param span Loess span.
#' @param family.loess Loess family.
#'
#' @return Normalized matrix.
#' @examples
#' \dontrun{ affy_normalize_loess(mat) }
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

    if ((change > epsilon) && (maxit > 1))
      warning(paste("No convergence after", maxit, "iterations.\n"))

    if(log.it) {
      return(2^mat)
    } else
      return(mat)
  }

#' @title affy_normalize_qspline
#'
#' @description Quantile-spline normalization adapted from affy-style workflows.
#'
#' @param x Numeric matrix (feature x sample).
#' @param target Optional target profile.
#' @param samples Number/fraction of quantile samples.
#' @param fit.iters Number of fitting iterations.
#' @param min.offset Minimum offset for fitting windows.
#' @param spline.method Spline method.
#' @param smooth Logical; smooth sampled quantiles.
#' @param spar Smoothing parameter.
#' @param p.min,p.max Quantile range to use.
#' @param incl.ends Logical; include end points.
#' @param converge Logical; use convergent fitting strategy.
#' @param verbose Logical; print progress.
#' @param na.rm Logical; ignore missing values during fitting.
#'
#' @return Normalized matrix.
#' @examples
#' \dontrun{ affy_normalize_qspline(x) }
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

#' @title sva_ComBat
#'
#' @description ComBat batch correction implementation used by normalization helpers.
#'
#' @param dat Data matrix (feature x sample).
#' @param batch Batch labels.
#' @param mod Optional model matrix of covariates.
#' @param par.prior Logical; use parametric priors.
#' @param prior.plots Logical; generate prior plots.
#' @param mean.only Logical; apply mean-only adjustment.
#' @param ref.batch Optional reference batch label.
#' @param BPPARAM BiocParallel backend parameter.
#'
#' @return Batch-corrected matrix.
#' @examples
#' \dontrun{ sva_ComBat(dat, batch) }
#' 
sva_ComBat <-
function (dat, batch, mod = NULL, par.prior = TRUE, prior.plots = FALSE,
          mean.only = FALSE, ref.batch = NULL, BPPARAM = bpparam("SerialParam"))
{
  if (length(dim(batch)) > 1) {
    stop("This version of ComBat only allows one batch variable")
  }
  dat <- as.matrix(dat)
  batch <- as.factor(batch)
  zero.rows.lst <- lapply(levels(batch), function(batch_level) {
    if (sum(batch == batch_level) > 1) {
      return(which(apply(dat[, batch == batch_level],
                         1, function(x) {
                           var(x) == 0
                         })))
    }
    else {
      return(which(rep(1, 3) == 2))
    }
  })
  zero.rows <- Reduce(union, zero.rows.lst)
  keep.rows <- setdiff(1:nrow(dat), zero.rows)
  if (length(zero.rows) > 0) {
    cat(sprintf("Found %d genes with uniform expression within a single batch (all zeros); these will not be adjusted for batch.\n",
                length(zero.rows)))
    dat.orig <- dat
    dat <- dat[keep.rows, ]
  }
  if (any(table(batch) == 1)) {
    mean.only = TRUE
  }
  if (mean.only == TRUE) {
    message("Using the 'mean only' version of ComBat")
  }
  batchmod <- model.matrix(~-1 + batch)
  if (!is.null(ref.batch)) {
    if (!(ref.batch %in% levels(batch))) {
      stop("reference level ref.batch is not one of the levels of the batch variable")
    }
    message("Using batch =", ref.batch, "as a reference batch (this batch won't change)")
    ref <- which(levels(as.factor(batch)) == ref.batch)
    batchmod[, ref] <- 1
  }
  else {
    ref <- NULL
  }
  message("Found", nlevels(batch), "batches")
  n.batch <- nlevels(batch)
  batches <- list()
  for (i in 1:n.batch) {
    batches[[i]] <- which(batch == levels(batch)[i])
  }
  n.batches <- sapply(batches, length)
  if (any(n.batches == 1)) {
    mean.only = TRUE
    message("Note: one batch has only one sample, setting mean.only=TRUE")
  }
  n.array <- sum(n.batches)
  design <- cbind(batchmod, mod)
  check <- apply(design, 2, function(x) all(x == 1))
  if (!is.null(ref)) {
    check[ref] <- FALSE
  }
  design <- as.matrix(design[, !check])
  message("Adjusting for", ncol(design) - ncol(batchmod),
          "covariate(s) or covariate level(s)")
  if (qr(design)$rank < ncol(design)) {
    if (ncol(design) == (n.batch + 1)) {
      stop("The covariate is confounded with batch! Remove the covariate and rerun ComBat")
    }
    if (ncol(design) > (n.batch + 1)) {
      if ((qr(design[, -c(1:n.batch)])$rank < ncol(design[,
                                                          -c(1:n.batch)]))) {
        stop("The covariates are confounded! Please remove one or more of the covariates so the design is not confounded")
      }
      else {
        stop("At least one covariate is confounded with batch! Please remove confounded covariates and rerun ComBat")
      }
    }
  }
  NAs <- any(is.na(dat))
  if (NAs) {
    message(c("Found", sum(is.na(dat)), "Missing Data Values"),
            sep = " ")
  }
  message("Standardizing Data across genes")
  if (!NAs) {
    B.hat <- solve(crossprod(design), tcrossprod(t(design),
                                                 as.matrix(dat)))
  }
  else {
    B.hat <- apply(dat, 1, Beta.NA, design)
  }
  if (!is.null(ref.batch)) {
    grand.mean <- t(B.hat[ref, ])
  }
  else {
    grand.mean <- crossprod(n.batches/n.array, B.hat[1:n.batch,
    ])
  }
  if (!NAs) {
    if (!is.null(ref.batch)) {
      ref.dat <- dat[, batches[[ref]]]
      var.pooled <- ((ref.dat - t(design[batches[[ref]],
      ] %*% B.hat))^2) %*% rep(1/n.batches[ref], n.batches[ref])
    }
    else {
      var.pooled <- ((dat - t(design %*% B.hat))^2) %*%
        rep(1/n.array, n.array)
    }
  }
  else {
    if (!is.null(ref.batch)) {
      ref.dat <- dat[, batches[[ref]]]
      var.pooled <- rowVars(ref.dat - t(design[batches[[ref]],
      ] %*% B.hat), na.rm = TRUE)
    }
    else {
      var.pooled <- rowVars(dat - t(design %*% B.hat),
                            na.rm = TRUE)
    }
  }
  stand.mean <- t(grand.mean) %*% t(rep(1, n.array))
  if (!is.null(design)) {
    tmp <- design
    tmp[, c(1:n.batch)] <- 0
    stand.mean <- stand.mean + t(tmp %*% B.hat)
  }
  s.data <- (dat - stand.mean)/(sqrt(var.pooled) %*% t(rep(1,
                                                           n.array)))
  message("Fitting L/S model and finding priors")
  batch.design <- design[, 1:n.batch]
  if (!NAs) {
    gamma.hat <- solve(crossprod(batch.design), tcrossprod(t(batch.design),
                                                           as.matrix(s.data)))
  }
  else {
    gamma.hat <- apply(s.data, 1, Beta.NA, batch.design)
  }
  delta.hat <- NULL
  for (i in batches) {
    if (mean.only == TRUE) {
      delta.hat <- rbind(delta.hat, rep(1, nrow(s.data)))
    }
    else {
      delta.hat <- rbind(delta.hat, rowVars(s.data[, i],
                                            na.rm = TRUE))
    }
  }
  gamma.bar <- rowMeans(gamma.hat)
  t2 <- rowVars(gamma.hat)
  a.prior <- apply(delta.hat, 1, aprior)
  b.prior <- apply(delta.hat, 1, bprior)
  if (prior.plots && par.prior) {
    old_pars <- par(no.readonly = TRUE)
    on.exit(par(old_pars))
    par(mfrow = c(2, 2))
    tmp <- density(gamma.hat[1, ])
    plot(tmp, type = "l", main = expression(paste("Density Plot of First Batch ",
                                                  hat(gamma))))
    xx <- seq(min(tmp$x), max(tmp$x), length = 100)
    lines(xx, dnorm(xx, gamma.bar[1], sqrt(t2[1])), col = 2)
    qqnorm(gamma.hat[1, ], main = expression(paste("Normal Q-Q Plot of First Batch ",
                                                   hat(gamma))))
    qqline(gamma.hat[1, ], col = 2)
    tmp <- density(delta.hat[1, ])
    xx <- seq(min(tmp$x), max(tmp$x), length = 100)
    tmp1 <- list(x = xx, y = dinvgamma(xx, a.prior[1], b.prior[1]))
    plot(tmp, typ = "l", ylim = c(0, max(tmp$y, tmp1$y)),
         main = expression(paste("Density Plot of First Batch ",
                                 hat(delta))))
    lines(tmp1, col = 2)
    invgam <- 1/qgamma(1 - ppoints(ncol(delta.hat)), a.prior[1],
                       b.prior[1])
    qqplot(invgam, delta.hat[1, ], main = expression(paste("Inverse Gamma Q-Q Plot of First Batch ",
                                                           hat(delta))), ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
    lines(c(0, max(invgam)), c(0, max(invgam)), col = 2)
  }
  gamma.star <- delta.star <- matrix(NA, nrow = n.batch, ncol = nrow(s.data))
  if (par.prior) {
    message("Finding parametric adjustments")
    results <- bplapply(1:n.batch, function(i) {
      if (mean.only) {
        gamma.star <- postmean(gamma.hat[i, ], gamma.bar[i],
                               1, 1, t2[i])
        delta.star <- rep(1, nrow(s.data))
      }
      else {
        temp <- it.sol(s.data[, batches[[i]]], gamma.hat[i,
        ], delta.hat[i, ], gamma.bar[i], t2[i], a.prior[i],
        b.prior[i])
        gamma.star <- temp[1, ]
        delta.star <- temp[2, ]
      }
      list(gamma.star = gamma.star, delta.star = delta.star)
    }, BPPARAM = BPPARAM)
    for (i in 1:n.batch) {
      gamma.star[i, ] <- results[[i]]$gamma.star
      delta.star[i, ] <- results[[i]]$delta.star
    }
  }
  else {
    message("Finding nonparametric adjustments")
    results <- bplapply(1:n.batch, function(i) {
      if (mean.only) {
        delta.hat[i, ] = 1
      }
      temp <- int.eprior(as.matrix(s.data[, batches[[i]]]),
                         gamma.hat[i, ], delta.hat[i, ])
      list(gamma.star = temp[1, ], delta.star = temp[2,
      ])
    }, BPPARAM = BPPARAM)
    for (i in 1:n.batch) {
      gamma.star[i, ] <- results[[i]]$gamma.star
      delta.star[i, ] <- results[[i]]$delta.star
    }
  }
  if (!is.null(ref.batch)) {
    gamma.star[ref, ] <- 0
    delta.star[ref, ] <- 1
  }
  message("Adjusting the Data\n")
  bayesdata <- s.data
  j <- 1
  for (i in batches) {
    bayesdata[, i] <- (bayesdata[, i] - t(batch.design[i,
    ] %*% gamma.star))/(sqrt(delta.star[j, ]) %*% t(rep(1,
                                                        n.batches[j])))
    j <- j + 1
  }
  bayesdata <- (bayesdata * (sqrt(var.pooled) %*% t(rep(1,
                                                        n.array)))) + stand.mean
  if (!is.null(ref.batch)) {
    bayesdata[, batches[[ref]]] <- dat[, batches[[ref]]]
  }
  if (length(zero.rows) > 0) {
    dat.orig[keep.rows, ] <- bayesdata
    bayesdata <- dat.orig
  }
  return(bayesdata)
}

