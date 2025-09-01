#' run_merge_amd_map
#'
#' @param datalist 
#' @param final.norm 
#' @param eval.sample.to.use 
#' @param sample.type.keep 
#'
#' @return
#' @export
#'
#' @examples
run_merge_amd_map <- 
  function(
    datalist = NULL,
    final.norm = NULL,
    eval.sample.to.use = NULL,
    sample.type.keep = NULL,
    db_file = NULL
  ){
    
    sample.type.keep <- c("Sample", sample.type.keep)
    
    meta.pos <- datalist$pos$meta_norm
    peaks.pos <- cbind(Sample = meta.pos[, "Sample"], datalist$pos$peaks_norm[[final.norm]])
    
    meta.neg <- datalist$neg$meta_norm
    peaks.neg <- cbind(Sample = meta.neg[, "Sample"], datalist$neg$peaks_norm[[final.norm]])
    
    if (!is.null(datalist$pos) & !is.null(datalist$neg)){
      
      features.pos <- 
        datalist$pos$features_norm %>% 
        mutate(Polarity = "pos")
      
      features.neg <- 
        datalist$neg$features_norm %>% 
        mutate(Polarity = "neg")
      
      feature.info.temp <-
        rbind(features.pos,
              features.neg)
      
      # Remove repeating features
      feature.info <-
        feature.info.temp %>%
        group_by(Identity_raw) %>%
        arrange(Identity_raw, Polarity, desc(get(paste0("mean.", eval.sample.to.use)))) %>%
        filter(row_number() == 1) %>%
        mutate(n = n(),
               Identity_clean = ifelse(n > 1, 
                                       paste0(Identity_raw, "-iso", row_number()),
                                       Identity_raw)) %>% 
        ungroup() %>% 
        select(-n) %>% 
        relocate(Identity_clean, .before = Identity) %>% 
        group_by(Polarity, iin_id) %>% 
        arrange(identity_source, desc(get(paste0("mean.", eval.sample.to.use)))) %>% 
        filter(is.na(iin_id) | row_number() == 1) %>% 
        ungroup() %>% 
        arrange(identity_source, Identity_clean)
      
      datatable.keep <-
        peaks.pos[, c("Sample", feature.info$Identity[feature.info$Polarity == "pos"])] %>%
        inner_join(peaks.neg[, c("Sample", feature.info$Identity[feature.info$Polarity == "neg"])], 
                   by = "Sample")
      
      sample.info <-
        meta.pos[match(datatable.keep$Sample, meta.pos$Sample), ] %>%
        select(-c("File.name")) 
      
    } else if (!is.null(datalist$pos) & is.null(datalist$neg)) {
      
      features.pos <- 
        datalist$pos$features_norm %>% 
        mutate(Polarity = "pos")
      
      feature.info.temp <- features.pos
      
      # Remove repeating features
      feature.info <-
        feature.info.temp %>%
        group_by(Identity_raw) %>%
        arrange(Identity_raw, Polarity, desc(get(paste0("mean.", eval.sample.to.use)))) %>%
        filter(row_number() == 1) %>%
        mutate(n = n(),
               Identity_clean = ifelse(n > 1, 
                                       paste0(Identity_raw, "-iso", row_number()),
                                       Identity_raw)) %>% 
        ungroup() %>% 
        select(-n) %>% 
        relocate(Identity_clean, .before = Identity) %>% 
        group_by(Polarity, iin_id) %>% 
        arrange(identity_source, desc(get(paste0("mean.", eval.sample.to.use)))) %>% 
        filter(is.na(iin_id) | row_number() == 1) %>% 
        ungroup() %>% 
        arrange(identity_source, Identity_clean)
      
      datatable.keep <- peaks.pos[, c("Sample", feature.info$Identity)]
      
      sample.info <-
        meta.pos[match(datatable.keep$Sample, meta.pos$Sample), ] %>%
        select(-c("File.name")) 
      
    } else if (is.null(datalist$pos) & !is.null(datalist$neg)) {
      
      features.neg <- 
        datalist$neg$features_norm %>% 
        mutate(Polarity = "neg")
      
      feature.info.temp <- features.neg
    
      # Remove repeating features
      feature.info <-
        feature.info.temp %>%
        group_by(Identity_raw) %>%
        arrange(Identity_raw, Polarity, desc(get(paste0("mean.", eval.sample.to.use)))) %>%
        filter(row_number() == 1) %>%
        mutate(n = n(),
               Identity_clean = ifelse(n > 1, 
                                       paste0(Identity_raw, "-iso", row_number()),
                                       Identity_raw)) %>% 
        ungroup() %>% 
        select(-n) %>% 
        relocate(Identity_clean, .before = Identity) %>% 
        group_by(Polarity, iin_id) %>% 
        arrange(identity_source, desc(get(paste0("mean.", eval.sample.to.use)))) %>% 
        filter(is.na(iin_id) | row_number() == 1) %>% 
        ungroup() %>% 
        arrange(identity_source, Identity_clean)
      
      datatable.keep <- peaks.neg[, c("Sample", feature.info$Identity)]
      
      sample.info <-
        meta.neg[match(datatable.keep$Sample, meta.neg$Sample), ] %>%
        select(-c("File.name"))   
    }
    
    feature.info.seq <- 
      data.frame(Identity = names(datatable.keep)[-1]) %>% 
      left_join(feature.info, by = "Identity")
    
    feature.info.seq <- 
      map_metabolite_info(input_file = feature.info.seq,
                          name_col = 4,
                          sep = ';',
                          db_file = db_file) %>% 
      select(-Metabolite_name_original)
    
    map.names <- feature.info.seq$Metabolite_name
    names(map.names) <- feature.info.seq$Identity
    colnames(datatable.keep)[-1] <- map.names[colnames(datatable.keep)[-1]]
    
    keep.index <- sample.info$Sample.type %in% sample.type.keep
    
    sample.info <- sample.info[keep.index, ]
    datatable <- cbind(sample.info, datatable.keep[keep.index, -1])
    
    
    print(paste0("For repeating features, the one with higher mean intensity in ", eval.sample.to.use, " was kept."), quote = FALSE)
    print(paste0(nrow(feature.info.temp) - nrow(feature.info), " features have been removed"), quote = FALSE)
    
    datalist$datatable <- datatable
    datalist$sample.info <- sample.info
    datalist$feature.info <- feature.info.seq
    
    return(datalist)

}







