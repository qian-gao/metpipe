normalize_various_methods <-
  function(
    x,
    istds = NULL,
    method = NULL,
    type = NULL,
    use.type = NULL,
    group = NULL,
    verbose = FALSE,
    batch = NULL,
    batch.wise = FALSE,
    sample.rate = 0.33
  ) {
    
    #library(crmn)
    #library(affy)
    library(reshape2)
    #library(tidyr)
    library(dplyr)
    
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
      
      x_norm <- t(apply(x, 1, function(x) {x/sum(x, na.rm = TRUE)}))
      
    } else if (method == 'median') {
      
      x_norm <- t(apply(x, 1, function(x) {x/median(x, na.rm = TRUE)}))
      
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
        affy::normalize.loess( mat, subset = sample(1:(dim(mat)[1]), min(c(5000, nrow(mat)))), 
                             epsilon = 10^-2, maxit = 1, log.it = TRUE, verbose = TRUE, 
                             span = 2/3, family.loess = "symmetric")
      x_norm <- t(mat_norm)
      
    } else if (method == 'qspline') {
      
      mat <- t(x)
      # Add the choice of target, samples
      target <- apply(x, 1, function(x) {median(x, na.rm = TRUE)} )
      mat_norm <-    
        affy::normalize.qspline(mat, target = target, samples = sample.rate,
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
        
        mat.norm <- melt(mat, id = c('Sample', 'Metabolite'))
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
            rename(Normalizer = variable) %>%
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
     
      minRSD <-
        minRSD %>%
        pivot_wider(-RSD, names_from = Batch, values_from = Normalizer)
      
      
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
    
    data.rsd <- 
      calculate_rsd( data = data,
                     type = sample.info$Sample.type,
                     names.suffix = method)$type.rsd
    
    output$rsd <- data.rsd
    
    return( output )
    
  }

# metabolomics_normalise <-
#   function(inputdata, 
#            method=c("median", "mean", "sum", "ref", "is", "nomis", "ccmn", "ruv2"),
#            refvec=NULL, ncomp=NULL, k=NULL, nc=NULL, saveoutput=FALSE, 
#            outputname=NULL)
#   {
#     # Match the method
#     method <- match.arg(method)
#     
#     # If method is not one of the above listed, then stop
#     if (!is.element(method, 
#                     c("median", "mean", "sum", "ref", "is", "nomis", "ccmn", "ruv2"))
#     ) {
#       stop("Invalid normalization method")
#     }
#     
#     # Reference vector should be a vector
#     if (!is.null(refvec)) {
#       if (class(refvec) %in% c("data.frame", "list", "matrix")) {
#         stop("Reference should be a vector")
#       }
#     }
#     
#     # If there is no refvec is given, get them to enter the reference vector
#     if ((method == "ref"| method == "is") & is.null(refvec)) {
#       stop("Please enter the reference vector")
#     }
#     # If no k, get them to enter it 
#     if (method == "ruv" & is.null(k)) {
#       stop("Please enter the number of unwanted variation factors")
#     }
#     # If there is no nc, get them to enter it
#     if (method == "ruv2" & is.null(nc)) {
#       stop(
#         paste("Please enter a logical vector indicating",
#               "the non-changing metabolites"
#         )
#       )
#     }
#     
#     
#     # If there is no nc, get them to enter it for ccmn
#     if ((method == "ccmn"|method == "nomis") & is.null(nc)) {
#       stop(
#         paste("Please enter a logical vector indicating",
#               "the internal standards"
#         )
#       )
#     }
#     
#     if (method == "ccmn"){
#       warning(paste("The ccmn method uses the grouping structure in the normalisation method, 
#                       therefore, should not be used for those unsupervised 
#                       methods where the groups must be treated as unknown."))      
#     }
#     
#     if (method == "ruv2"){
#       warning("The ruv2 method generates a matrix of unwanted 
#                 variation using LinearModelFit(). For identifying 
#                 differentially abundant metabolites, use LinearModelFit() 
#                 directly with ruv2=TRUE")     
#     }
#     
#     # Get groups information
#     Group <- inputdata[, 1]
#     
#     # Get normalisation vector according to the method and inputs, and
#     # remove groups and internal standard for data processing
#     
#     # Remove groups for processing
#     #pre_norm <- inputdata[, -1]
#     pre_norm <- inputdata
#     # Median vector
#     if (method == "median") {
#       norm_vector <- apply(pre_norm, 1, median, na.rm=TRUE)
#       # Mean vector
#     } else if (method == "mean") {
#       norm_vector <- rowMeans(pre_norm, na.rm=TRUE)
#       # Sum vector
#     } else if (method == "sum") {
#       norm_vector <- rowSums(pre_norm, na.rm=TRUE)
#     } else if (method == "ref" | method == "is") {
#       norm_vector <- refvec
#     }
#     
#     # Prepare an empty matrix
#     #norm_data <- matrix(NA, nrow=nrow(pre_norm), ncol=length(pre_norm))
#     #rownames(norm_data) <- rownames(pre_norm)
#     #colnames(norm_data) <- colnames(pre_norm)
#     
#     if (!is.null(nc)){
#       ncvec<-logical(ncol(pre_norm))
#       ncvec[nc]<-TRUE      
#     }
#     
#     if (method == "ccmn") {
#       norm_data <- t(normalize(t(pre_norm), "crmn", 
#                                factor=model.matrix(~-1 + Group), standards=ncvec, ncomp=ncomp, 
#                                lg=FALSE)
#       )
#     } else if (method == "nomis") {
#       norm_data <- t(crmn::normalize(t(pre_norm), "nomis", standards=ncvec, 
#                                lg=FALSE)
#       )
#     } else if (method == "ruv2") {
#       norm_data<-LinearModelFit(datamat=data.matrix(pre_norm),
#                                 factormat=model.matrix(~-1 + Group), 
#                                 ruv2=TRUE,
#                                 k=k, nc=nc)$uvmat
#     } else {
#       norm_data <- sweep(pre_norm, 1, norm_vector, "-")
#     }
#     
#     # Reattach groups information
#     outdata <- data.frame(Group, norm_data)
#     #Edit column names
#     outdata <- editcolnames(outdata)
#     
#     # Generate the output matrix in .csv format
#     if (saveoutput) {
#       write.csv(outdata,
#                 if (!is.null(outputname)) {
#                   paste(c(outputname, ".csv"), collapse="")
#                 } else {
#                   paste(c("normalized_", method, ".csv"), collapse="")
#                 }
#       )
#     }
#     
#     output <- list()
#     output$output <- outdata
#     output$groups <- Group
#     output$samples <- row.names(inputdata)
#     
#     return(structure(output, class="metabdata"))
#   }