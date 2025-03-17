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
    
    missings_nr <- sum( is.na(x) | x <= 0 )
    
    if (is.null(method)) {
      
      x_imputed <- x
      method <- "no imputation"
      
    } else if (method == 'HF') {
      
      x_imputed <- apply(x, 2, function(x){
        miss <- which( is.na(x) | x <= 0 )
        if (sum(miss) > 0) {
          x[ miss ] <- 0.5*min(x[-miss], na.rm = TRUE)
        }
        return(x)
      })
      
    } else if (method == 'LoD') {
      
      x_imputed <- apply(x, 2, function(x){
        miss <- which( is.na(x) | x <= 0 )
        if (sum(miss) > 0) {
          x[ miss ] <- 0.2*min(x[-miss], na.rm = TRUE)
        }
        return(x)
      })
      
    } else if (method == 'median') {
      
      x_imputed <- apply(x, 2, function(x){
        miss <- which( is.na(x) | x <= 0 )
        if (sum(miss) > 0) {
          x[ miss ] <- median(x[-miss], na.rm = TRUE)
        }
        return(x)
      })
      
    } else if (method == 'min') {
      
      x_imputed <- apply(x, 2, function(x){
        miss <- which( is.na(x) | x <= 0 )
        if (sum(miss) > 0) {
          x[ miss ] <- min(x[-miss], na.rm = TRUE)
        }
        return(x)
      })
      
    } else if (method == 'mean') {
      
      x_imputed <- apply(x, 2, function(x){
        miss <- which( is.na(x) | x <= 0 )
        if (sum(miss) > 0) {
          x[ miss ] <- mean(x[-miss], na.rm = TRUE)
        }
        return(x)
      })
      
    } else if (method == "knn") {
      # if missing < 20% for each group, apply knn, otherwise LoD
      
      if (!is.null(group.info)){
        grps <- unique(group.info)
      } else {
        grps <- "Group 0"
      }
      
      
      x_imputed <- x
      for (i in grps){
        row.index <- group.info == i
        data.i <- x[row.index, , drop = FALSE]
        nrow <- nrow(data.i)
        
        col.index <- apply(data.i, 2, function(x) {sum(is.na(x) | x <= 0) <= missing_thres * nrow})
        
        x_imputed[row.index, col.index] <-
          kNN(x_imputed[row.index, col.index, drop = FALSE],
              variable = colnames(x_imputed[row.index, col.index, drop = FALSE]),
              k = k, numFun = weightedMean, weightDist=TRUE, imp_var = FALSE)
        
      }
      
      x_imputed <-
        apply(x_imputed, 2,
              function(x){
                miss <- which( is.na(x) | x <= 0 )
                if (sum(miss) > 0) {
                  x[ miss ] <- 0.2*min(x[-miss], na.rm = TRUE)
                }
                return(x)
              })
    }
    
    x_imputed <- data.frame(x_imputed)
    rownames(x_imputed) <- rownames(x)
    colnames(x_imputed) <- colnames(x)
    
    result <- list( x = x_imputed,
                    method = method)
    
    print(paste(missings_nr, "missing values are found and imputed using method: ", method))
    
    return( result )
  }

