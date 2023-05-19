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
    prefix = ""
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

# calculate_rsd <-
#   function( data = NULL, 
#             type = NULL,
#             impute = FALSE,
#             names.suffix = NULL
#   ){
#     
#     ### Impute missing
#     if (impute){
#       hm <- min(data, na.rm = T) / 2
#       data[ is.na(data) ] <- hm
#     }
#     
#     ### Calculate
#     
#     map.names <- colnames(data)
#     names(map.names) <- make.names(map.names)
#     
#     rsd <- 
#       data.frame( Sample.type = type, data) %>%
#       pivot_longer( -Sample.type, names_to = "Identity", values_to = "Intensity") %>%
#       group_by( Identity, Sample.type) %>%
#       summarise( mean = mean(Intensity, na.rm = TRUE),
#                  sd = sd(Intensity, na.rm = TRUE),
#                  rsd = sd/mean ) %>%
#       ungroup() %>%
#       mutate( names.suf = ifelse( is.null(names.suffix), "", names.suffix),
#               Sample.type = ifelse( names.suf == "", 
#                                     Sample.type,
#                                     paste0(Sample.type, ".", names.suffix)) ) %>%
#       pivot_wider(names_from = Sample.type, values_from = c(mean, sd, rsd), 
#                   names_glue = "{.value}.{Sample.type}") %>%
#       arrange(match(Identity, colnames(map.names))) %>%
#       mutate(Identity = map.names[Identity])
#     
#     
#     mean <- 
#       rsd %>%
#       select(Identity, starts_with("mean.")) %>%
#       rename_all(~stringr::str_replace(.,"^mean.",""))
#     
#     sd <- 
#       rsd %>%
#       select(Identity, starts_with("sd.")) %>%
#       rename_all(~stringr::str_replace(.,"^sd.",""))
#     
#     rsd <- 
#       rsd %>%
#       select(Identity, starts_with("rsd.")) %>%
#       rename_all(~stringr::str_replace(.,"^rsd.",""))
#     
#     output <-
#       list( type.mean = mean,
#             type.sd   = sd,
#             type.rsd  = rsd )
#     
#     ### old
#     
#     # data = NULL, 
#     # type = NULL,
#     # impute = FALSE,
#     # suffix = "",
#     # BPPARAM = BiocParallel::bpparam()
#     #
#     # mean <- 
#     #   BiocParallel::bpaggregate( data, 
#     #                              by = list(type), 
#     #                              FUN = base::mean, 
#     #                              BPPARAM = BPPARAM)
#     # 
#     # group <- paste0(mean[, 1], suffix)
#     # sd <- 
#     #   BiocParallel::bpaggregate( data, 
#     #                              by = list(type), 
#     #                              FUN = stats::sd, 
#     #                              BPPARAM = BPPARAM)
#     # 
#     # rsd <- sd[, -1]/mean[, -1]
#     # 
#     # mean <- t(mean[, -1])
#     # colnames(mean) <- group
#     # 
#     # sd <- t(sd[, -1])
#     # colnames(sd) <- group
#     # 
#     # rsd <- t(rsd)
#     # colnames(rsd) <- group
#     # 
#     # output <- 
#     #   list( type.mean = mean,
#     #         type.sd   = sd,
#     #         type.rsd  = rsd )
#     
#     return(output)
#     
#   }
