#' @title normalize_serrf
#'
#' @description SERRF (Systematic Error Removal using Random Forest) normalization.
#'   For each feature and batch, a random forest is trained on QC samples using
#'   the most correlated features as predictors, and the predicted systematic
#'   variation is removed from QC and non-QC samples. Adapted from the original
#'   SERRF implementation (Fan et al., Anal. Chem. 2019).
#'
#' @param x Numeric matrix/data frame (sample x feature).
#' @param type Sample-type vector (length `nrow(x)`).
#' @param use.type Sample type used as QC (training) samples, e.g. "PO".
#' @param batch Optional batch vector (length `nrow(x)`). If NULL, one batch is assumed.
#' @param num Number of correlated features used as predictors.
#' @param seed Random seed for reproducibility (NULL to skip seeding).
#'
#' @return Normalized matrix (sample x feature) with the same dimnames as `x`.
#' @keywords internal
#' @examples
#' \dontrun{ normalize_serrf(x, type = meta$Sample.type, use.type = "PO", batch = meta$Sample.batch) }
#'
normalize_serrf <- function(x, type, use.type, batch = NULL, num = 10, seed = 1) {

  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("Package 'ranger' is required for method='serrf'")
  }

  x <- as.matrix(x)
  storage.mode(x) <- "numeric"
  x_rownames <- rownames(x)
  x_colnames <- colnames(x)

  if (is.null(batch)) {
    batch <- rep("No.batches", nrow(x))
  }
  batch <- as.character(batch)
  type <- as.character(type)
  type[is.na(type)] <- "NA"

  qc.idx <- type == use.type
  if (!any(qc.idx)) {
    stop("No samples match `use.type` for method='serrf'")
  }
  if (all(qc.idx)) {
    stop("method='serrf' requires at least one non-QC sample")
  }
  if (ncol(x) < 2) {
    stop("method='serrf' requires at least two features")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # feature x sample
  e <- t(x)
  num <- min(num, nrow(e) - 1)

  serrf_core <- function(train, target, num, batch., sampleType.) {

    batch. <- factor(batch.)
    is.qc <- sampleType. == "qc"
    all <- cbind(train, target)

    # Replace zeros and missing values within each batch
    for (j in seq_len(nrow(all))) {
      for (current_batch in levels(batch.)) {
        in.b <- batch. %in% current_batch
        v <- all[j, in.b]
        if (all(is.na(v))) next
        m <- min(v[!is.na(v)])
        zero <- !is.na(v) & v == 0
        v[zero] <- rnorm(sum(zero), mean = m + 1, sd = 0.1 * (m + .1))
        m <- min(v[!is.na(v)])
        na <- is.na(v)
        v[na] <- rnorm(sum(na), mean = 0.5 * m + 1, sd = 0.1 * (m + .1))
        all[j, in.b] <- v
      }
    }

    # Feature-feature correlations within each batch (QC and target separately)
    corrs_train <- list()
    corrs_target <- list()
    for (current_batch in levels(batch.)) {
      tr <- train[, batch.[is.qc] %in% current_batch, drop = FALSE]
      tg <- target[, batch.[!is.qc] %in% current_batch, drop = FALSE]
      # Batches without enough QCs or without targets are left uncorrected
      if (ncol(tr) < 2 || ncol(tg) < 1) next
      train_scale <- t(apply(tr, 1, scale))
      target_scale <- scale(tg)
      corrs_train[[current_batch]] <- suppressWarnings(cor(t(train_scale), method = "spearman"))
      corrs_target[[current_batch]] <- suppressWarnings(cor(t(target_scale), method = "spearman"))
    }

    pred <- vapply(seq_len(nrow(all)), function(j) {

      normalized <- all[j, ]

      for (current_batch in levels(batch.)) {

        if (is.null(corrs_train[[current_batch]])) next

        in.b <- batch. %in% current_batch
        e_current_batch <- all[, in.b, drop = FALSE]
        corr_train <- corrs_train[[current_batch]]
        corr_target <- corrs_target[[current_batch]]

        corr_train_order <- order(abs(corr_train[, j]), decreasing = TRUE)
        corr_target_order <- order(abs(corr_target[, j]), decreasing = TRUE)

        sel_var <- c()
        l <- num
        while (length(sel_var) < num) {
          sel_var <- intersect(corr_train_order[1:l], corr_target_order[1:l])
          sel_var <- sel_var[!sel_var == j]
          l <- l + 1
        }

        qc.b <- sampleType.[in.b] == "qc"
        train_data_y <- as.numeric(scale(e_current_batch[j, qc.b], scale = FALSE))
        train_data_x <- apply(e_current_batch[sel_var, qc.b, drop = FALSE], 1, scale)

        tx <- e_current_batch[sel_var, !qc.b, drop = FALSE]
        if (ncol(tx) == 1) {
          test_data_x <- t(scale(tx))
        } else {
          test_data_x <- apply(tx, 1, scale)
        }

        train_NA_index <- apply(train_data_x, 2, function(x) sum(is.na(x)) > 0)
        train_data_x <- train_data_x[, !train_NA_index, drop = FALSE]
        test_data_x <- test_data_x[, !train_NA_index, drop = FALSE]

        good_column <-
          apply(train_data_x, 2, function(x) sum(is.na(x)) == 0) &
          apply(test_data_x, 2, function(x) sum(is.na(x)) == 0)
        train_data_x <- train_data_x[, good_column, drop = FALSE]
        test_data_x <- test_data_x[, good_column, drop = FALSE]

        train_data <- data.frame(y = train_data_y, train_data_x)

        if (ncol(train_data) == 1) { # no usable predictors
          normalized[in.b] <- e_current_batch[j, ]
        } else {
          colnames(train_data) <- c("y", paste0("V", 1:(ncol(train_data) - 1)))
          model <- ranger::ranger(y ~ ., data = train_data)

          test_data <- data.frame(test_data_x)
          colnames(test_data) <- colnames(train_data)[-1]

          pred_train <- predict(model, data = train_data)$predictions
          pred_test <- predict(model, data = test_data)$predictions

          norm <- e_current_batch[j, ]

          norm[qc.b] <- e_current_batch[j, qc.b] /
            ((pred_train + mean(e_current_batch[j, qc.b], na.rm = TRUE)) / mean(all[j, is.qc], na.rm = TRUE))

          norm[!qc.b] <- e_current_batch[j, !qc.b] /
            ((pred_test + mean(e_current_batch[j, !qc.b], na.rm = TRUE)) / median(all[j, !is.qc], na.rm = TRUE))
          neg <- which(norm[!qc.b] < 0)
          norm[!qc.b][neg] <- e_current_batch[j, !qc.b][neg]

          norm[qc.b] <- norm[qc.b] / (median(norm[qc.b], na.rm = TRUE) / median(all[j, is.qc], na.rm = TRUE))
          norm[!qc.b] <- norm[!qc.b] / (median(norm[!qc.b], na.rm = TRUE) / median(all[j, !is.qc], na.rm = TRUE))
          norm[!is.finite(norm)] <- rnorm(sum(!is.finite(norm)), sd = sd(norm[is.finite(norm)], na.rm = TRUE) * 0.01)

          out <- boxplot.stats(norm, coef = 3)$out
          is.out <- which(norm[!qc.b] %in% out)
          norm[!qc.b][is.out] <-
            (e_current_batch[j, !qc.b] -
               ((pred_test + mean(e_current_batch[j, !qc.b], na.rm = TRUE)) - median(all[j, !is.qc], na.rm = TRUE)))[is.out]
          neg <- which(norm[!qc.b] < 0)
          norm[!qc.b][neg] <- e_current_batch[j, !qc.b][neg]

          normalized[in.b] <- norm
        }
      }

      normalized
    }, numeric(ncol(all)))

    normed <- t(pred)

    fix_values <- function(mat) {
      for (i in seq_len(nrow(mat))) {
        ok <- !is.na(mat[i, ])
        if (any(!ok) && any(ok)) {
          mat[i, !ok] <- rnorm(sum(!ok), mean = min(mat[i, ok]), sd = sd(mat[i, ok]) * 0.1)
        }
        pos <- which(mat[i, ] > 0)
        neg <- which(mat[i, ] < 0)
        if (length(neg) > 0 && length(pos) > 0) {
          mat[i, neg] <- runif(1) * min(mat[i, pos])
        }
      }
      mat
    }

    list(normed_train = fix_values(normed[, is.qc, drop = FALSE]),
         normed_target = fix_values(normed[, !is.qc, drop = FALSE]))
  }

  e_qc <- e[, qc.idx, drop = FALSE]
  e_norm <- e

  # "Sample" first (its run also provides the normalized QCs), then other types
  target.types <- unique(type[!qc.idx])
  target.types <- c(intersect("Sample", target.types), setdiff(target.types, "Sample"))

  for (k in seq_along(target.types)) {
    tt.idx <- type == target.types[k]
    res <- serrf_core(
      train = e_qc,
      target = e[, tt.idx, drop = FALSE],
      num = num,
      batch. = c(batch[qc.idx], batch[tt.idx]),
      sampleType. = rep(c("qc", "sample"), c(sum(qc.idx), sum(tt.idx)))
    )
    e_norm[, tt.idx] <- res$normed_target
    if (k == 1) {
      e_norm[, qc.idx] <- res$normed_train
    }
  }

  x_norm <- t(e_norm)
  rownames(x_norm) <- x_rownames
  colnames(x_norm) <- x_colnames
  x_norm
}
