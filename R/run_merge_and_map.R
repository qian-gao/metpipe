#' Merge positive/negative modes and map metabolite names
#'
#' Combines normalized outputs across ionization modes, removes duplicated
#' annotations by intensity/retention-time logic, and maps feature identities
#' to metabolite names via [map_metabolite_info()].
#'
#' @param datalist A `metpipe_datalist` (or compatible list) containing
#'   normalized `pos` and/or `neg` mode outputs.
#' @param final.norm Name of normalized matrix to use from `peaks_norm`.
#' @param eval.sample.to.use Sample type used to rank duplicate feature identities.
#' @param sample.type.keep Additional sample types to retain besides `"Sample"`.
#' @param db_file Optional database file used by [map_metabolite_info()].
#'
#' @return Updated `metpipe_datalist` with merged `datatable`,
#'   `sample.info`, and `feature.info`.
#' @export
#'
#' @examples
run_merge_and_map <- 
  function(
    datalist = NULL,
    final.norm = NULL,
    eval.sample.to.use = NULL,
    sample.type.keep = NULL,
    db_file = NULL
  ){

    datalist <- as_metpipe_datalist(datalist, stage = "imported")

    has_pos <- has_mode(datalist, "pos")
    has_neg <- has_mode(datalist, "neg")

    if (is.null(final.norm) || !nzchar(final.norm)) {
      stop("final.norm must be provided (e.g. a name in peaks_norm)")
    }

    if (is.null(eval.sample.to.use) || !nzchar(eval.sample.to.use)) {
      stop("eval.sample.to.use must be provided")
    }

    get_peaks_norm <- function(mode_data, mode_name) {
      peaks <- mode_data$peaks_norm[[final.norm]]
      if (is.null(peaks)) {
        stop("Normalization '", final.norm, "' was not found in datalist$", mode_name, "$peaks_norm")
      }
      peaks
    }
    
    sample.type.keep <- unique(c("Sample", sample.type.keep))
    
    if (has_pos && has_neg){
      pos_data <- get_mode(datalist, "pos")
      neg_data <- get_mode(datalist, "neg")

      meta.pos <- pos_data$meta_norm
      peaks.pos <- cbind(Sample = meta.pos[, "Sample"], get_peaks_norm(pos_data, "pos"))

      meta.neg <- neg_data$meta_norm
      peaks.neg <- cbind(Sample = meta.neg[, "Sample"], get_peaks_norm(neg_data, "neg"))
      
      features.pos <- 
        pos_data$features_norm %>% 
        mutate(Polarity = "pos",
               identifier = paste0("pos_", Feature_ID))
      
      features.neg <- 
        neg_data$features_norm %>% 
        mutate(Polarity = "neg",
               identifier = paste0("neg_", Feature_ID))
      
      feature.info.temp <-
        rbind(features.pos,
              features.neg)
      
      colnames(peaks.pos) <- c("Sample", features.pos$identifier)
      colnames(peaks.neg) <- c("Sample", features.neg$identifier)
      
      # Remove repeating features
      feature.info <-
        feature.info.temp %>%
        group_by(Identity_raw) %>%
        arrange(Identity_raw, desc(get(paste0("mean.", eval.sample.to.use)))) %>%
        # mutate(first_rt = first(rt),
        #        second_rt = nth(rt, 2),
        #        diff_to_first_rt = abs(rt - first_rt) > 0.2,
        #        diff_to_second_rt = abs(rt - second_rt) > 0.2) %>% 
        # filter(row_number() == 1 | (row_number() == 2 & diff_to_first_rt == TRUE) | 
        #          (row_number() == 3 & diff_to_first_rt == TRUE & diff_to_second_rt == TRUE)) %>% 
        filter(row_number() == 1) %>% 
        ungroup() %>% 
        # dplyr::select(-c(first_rt, diff_to_first_rt, second_rt, diff_to_second_rt)) %>% 
        group_by(Identity_raw) %>%
        mutate(n = n(),
               Identity = ifelse(n > 1,
                                       paste0(Identity_raw, "-iso", row_number()),
                                       Identity_raw),
               Identity = gsub("low score: ", "", Identity)) %>%
        ungroup() %>%
        dplyr::select(-n)
      
      if ("iin_id" %in% names(feature.info)){
        feature.info <- 
          feature.info %>% 
          group_by(Polarity, iin_id) %>% 
          arrange(identity_source, desc(get(paste0("mean.", eval.sample.to.use)))) %>% 
          filter(is.na(iin_id) | row_number() == 1) %>% 
          ungroup() %>% 
          arrange(identity_source, Identity_raw)
      } else {
        feature.info <- 
          feature.info %>% 
          arrange(identity_source, Identity_raw)      
      }
        
      datatable.keep <-
        peaks.pos[, c("Sample", feature.info$identifier[feature.info$Polarity == "pos"])] %>%
        inner_join(peaks.neg[, c("Sample", feature.info$identifier[feature.info$Polarity == "neg"])], 
                   by = "Sample")
      
      datatable.keep <- datatable.keep[, c("Sample", feature.info$identifier)]
      colnames(datatable.keep) <- c("Sample", feature.info$Identity)
      
      sample.info <-
        meta.pos[match(datatable.keep$Sample, meta.pos$Sample), ] %>%
        dplyr::select(-c("File.name")) 
      
      feature.info <- 
        feature.info %>% 
        dplyr::select(-identifier)
      
    } else if (has_pos && !has_neg) {
      pos_data <- get_mode(datalist, "pos")

      meta.pos <- pos_data$meta_norm
      peaks.pos <- cbind(Sample = meta.pos[, "Sample"], get_peaks_norm(pos_data, "pos"))
      
      features.pos <- 
        pos_data$features_norm %>% 
        mutate(Polarity = "pos")
      
      feature.info.temp <- features.pos
      
      # Remove repeating features
      feature.info <-
        feature.info.temp %>%
        group_by(Identity_raw) %>%
        arrange(Identity_raw, desc(get(paste0("mean.", eval.sample.to.use)))) %>%
        # mutate(first_rt = first(rt),
        #        second_rt = nth(rt, 2),
        #        diff_to_first_rt = abs(rt - first_rt) > 0.2,
        #        diff_to_second_rt = abs(rt - second_rt) > 0.2) %>% 
        # filter(row_number() == 1 | (row_number() == 2 & diff_to_first_rt == TRUE) | 
        #          (row_number() == 3 & diff_to_first_rt == TRUE & diff_to_second_rt == TRUE)) %>% 
        filter(row_number() == 1) %>% 
        ungroup() %>% 
        #dplyr::select(-c(first_rt, diff_to_first_rt, second_rt, diff_to_second_rt)) %>% 
        group_by(Identity_raw) %>%
        mutate(n = n(),
               Identity = ifelse(n > 1,
                                 paste0(Identity_raw, "-iso", row_number()),
                                 Identity_raw),
               Identity = gsub("low score: ", "", Identity)) %>%
        ungroup() %>%
        dplyr::select(-n)
      
      if ("iin_id" %in% names(feature.info)){
        feature.info <- 
          feature.info %>% 
          group_by(Polarity, iin_id) %>% 
          arrange(identity_source, desc(get(paste0("mean.", eval.sample.to.use)))) %>% 
          filter(is.na(iin_id) | row_number() == 1) %>% 
          ungroup() %>% 
          arrange(identity_source, Identity_raw)
        
      } else {
        feature.info <- 
          feature.info %>% 
          arrange(identity_source, Identity_raw)        
      }
      
      datatable.keep <- peaks.pos[, c("Sample", feature.info$Identity)]
      
      sample.info <-
        meta.pos[match(datatable.keep$Sample, meta.pos$Sample), ] %>%
        dplyr::select(-c("File.name")) 
      
    } else if (!has_pos && has_neg) {
      neg_data <- get_mode(datalist, "neg")

      meta.neg <- neg_data$meta_norm
      peaks.neg <- cbind(Sample = meta.neg[, "Sample"], get_peaks_norm(neg_data, "neg"))
      
      features.neg <- 
        neg_data$features_norm %>% 
        mutate(Polarity = "neg")
      
      feature.info.temp <- features.neg
    
      # Remove repeating features
      feature.info <-
        feature.info.temp %>%
        group_by(Identity_raw) %>%
        arrange(Identity_raw, desc(get(paste0("mean.", eval.sample.to.use)))) %>%
        # mutate(first_rt = first(rt),
        #        second_rt = nth(rt, 2),
        #        diff_to_first_rt = abs(rt - first_rt) > 0.2,
        #        diff_to_second_rt = abs(rt - second_rt) > 0.2) %>% 
        # filter(row_number() == 1 | (row_number() == 2 & diff_to_first_rt == TRUE) | 
        #          (row_number() == 3 & diff_to_first_rt == TRUE & diff_to_second_rt == TRUE)) %>% 
        filter(row_number() == 1) %>% 
        ungroup() %>% 
        #dplyr::select(-c(first_rt, diff_to_first_rt, second_rt, diff_to_second_rt)) %>% 
        group_by(Identity_raw) %>%
        mutate(n = n(),
               Identity = ifelse(n > 1,
                                 paste0(Identity_raw, "-iso", row_number()),
                                 Identity_raw),
               Identity = gsub("low score: ", "", Identity)) %>%
        ungroup() %>%
        dplyr::select(-n)
      
      if ("iin_id" %in% names(feature.info)){
        feature.info <- 
          feature.info %>% 
          group_by(Polarity, iin_id) %>% 
          arrange(identity_source, desc(get(paste0("mean.", eval.sample.to.use)))) %>% 
          filter(is.na(iin_id) | row_number() == 1) %>% 
          ungroup() %>% 
          arrange(identity_source, Identity_raw)
        
      } else {
        feature.info <- 
          feature.info %>% 
          arrange(identity_source, Identity_raw)        
      }
      
      datatable.keep <- peaks.neg[, c("Sample", feature.info$Identity)]
      
      sample.info <-
        meta.neg[match(datatable.keep$Sample, meta.neg$Sample), ] %>%
        dplyr::select(-c("File.name"))   
    }
    
    feature.info.seq <- 
      data.frame(Identity = names(datatable.keep)[-1]) %>% 
      left_join(feature.info, by = "Identity") %>% 
      dplyr::rename(Formula = formula) %>% 
      mutate(name_search = gsub("low score: ", "", Identity_raw))
    
    feature.info.seq <- 
      map_metabolite_info(input_file = feature.info.seq,
                          name_col = match("name_search", names(feature.info.seq)),
                          sep = ';',
                          db_file = db_file) %>% 
      dplyr::select(-Metabolite_name_original) %>% 
      relocate(c(Identity, Identity_raw), .after = Metabolite_name) %>% 
      group_by(Identity_raw) %>% 
      mutate(n = n(),
             Metabolite_name = ifelse(n > 1,
                                     paste0(Metabolite_name, "-iso", row_number()),
                                     Metabolite_name)) %>%
      ungroup() %>%
      dplyr::select(-n)
    
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
    
    validate_metpipe_datalist(datalist, stage = "merged")
    return(datalist)

}






