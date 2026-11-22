#' @title impute_missing
#'
#' @description Missing imputation with various methods.
#'
#' @param x Input data frame for imputation, sample x feature.
#' @param method Method for imputation, available methods: HF, half minimum; LoD,
#'               limit of detection, 1/5 of minimum; min, minimum; median; mean;
#'               knn, k nearest neighbor. If knn is chosen, then the number of
#'               nearest neighbors k, missing threshold missing_thres and group.info
#'               (optional) are used. group.info (optional) is a vector indicating
#'               the group information of the samples. knn works in the way that
#'               only the feature with missing rate <= missing_thres in a group
#'               are be imputed with knn group-wise. The rest are imputed with LoD.
#' @param k The number of nearest neighbors.
#' @param missing_thres Missing threshold for deciding if knn is applied.
#' @param group.info Optional, if provides, knn is applied group-wise.
#'
#' @return Imputed data frame.
#' @examples impute_various_methods( data,
#'                                   method = "LoD")
#' @importFrom VIM kNN
#' @importFrom laeken weightedMean
#' @export

impute_missing <-
  function(
    x,
    method = NULL,
    k = 5,
    missing_thres = 0.2,
    group.info = NULL
  ) {

    if (!is.data.frame(x) && !is.matrix(x)) {
      stop("x must be a data.frame or matrix")
    }

    valid_methods <- c(NULL, "HF", "LoD", "median", "min", "mean", "knn")
    if (!is.null(method) && !method %in% valid_methods) {
      stop("Unsupported method: ", method)
    }

    fill_missing <- function(v, fill_fun, scale = 1) {
      miss <- which(is.na(v) | v <= 0)
      if (length(miss) == 0) return(v)
      observed <- v[setdiff(seq_along(v), miss)]
      if (length(observed) == 0) {
        v[miss] <- 0
      } else {
        v[miss] <- scale * fill_fun(observed, na.rm = TRUE)
      }
      v
    }
    
    missings_nr <- sum( is.na(x) | x <= 0 )
    
    if (is.null(method)) {
      
      x_imputed <- x
      method <- "no imputation"
      
    } else if (method == 'HF') {
      
      x_imputed <- apply(x, 2, fill_missing, fill_fun = min, scale = 0.5)
      
    } else if (method == 'LoD') {
      
      x_imputed <- apply(x, 2, fill_missing, fill_fun = min, scale = 0.2)
      
    } else if (method == 'median') {
      
      x_imputed <- apply(x, 2, fill_missing, fill_fun = median, scale = 1)
      
    } else if (method == 'min') {
      
      x_imputed <- apply(x, 2, fill_missing, fill_fun = min, scale = 1)
      
    } else if (method == 'mean') {
      
      x_imputed <- apply(x, 2, fill_missing, fill_fun = mean, scale = 1)
      
    } else if (method == "knn") {
      # if missing < 20% for each group, apply knn, otherwise HF

      if (is.null(group.info)) {
        group.info <- rep("Group 0", nrow(x))
      }
      if (length(group.info) != nrow(x)) {
        stop("group.info must have the same length as nrow(x)")
      }
      
      grps <- unique(group.info)
      
      
      # Impute each group's low-missingness block with kNN and write it back
      # in place, so x_imputed keeps every row and column of x. Features that
      # are not imputed here are filled with HF below.
      x_imputed <- as.matrix(x)
      for (i in grps){
        row.index <- which(group.info == i)
        data.i <- x_imputed[row.index, , drop = FALSE]
        is_miss <- is.na(data.i) | data.i <= 0
        col.index <- which(colSums(is_miss) <= missing_thres * nrow(data.i))
        if (length(col.index) == 0 || length(row.index) < 2) next

        block <- data.i[, col.index, drop = FALSE]
        block[is_miss[, col.index, drop = FALSE]] <- NA
        block <- as.data.frame(block, stringsAsFactors = FALSE)
        names(block) <- paste0("V", seq_len(ncol(block)))   # kNN needs syntactic names

        imp <- kNN(block, variable = names(block),
                   k = k, numFun = weightedMean, weightDist = TRUE, imp_var = FALSE)

        x_imputed[row.index, col.index] <- as.matrix(imp)
      }

      x_imputed <- apply(x_imputed, 2, fill_missing, fill_fun = min, scale = 0.5)
    }
    
    x_imputed <- data.frame(x_imputed)
    rownames(x_imputed) <- rownames(x)
    colnames(x_imputed) <- colnames(x)
    
    result <- list( x = x_imputed,
                    method = method)
    
    print(paste(missings_nr, "missing values are found and imputed using method: ", method))
    
    return( result )
  }

