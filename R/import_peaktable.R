#' @title import_peaktable
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
#' @import dplyr 
#' 
import_peaktable <- 
  function(
      peaktable = NULL,
      meta = NULL,
      rt_col_nr = NULL,
      mz_col_nr = NULL,
      identity_col_nr = NULL,
      sample_col_nr = NULL,
      find_is = FALSE,
      add_lipid_info = FALSE,
      keep.lipid.orig = NULL
  ){

    datalist <- list()
    
    file <- openxlsx::read.xlsx(peaktable)
    peaks <- 
      file[, c(sample_col_nr:ncol(file))] %>% 
      mutate(across(where(is.character), as.numeric))
    features <- 
      file[, c(rt_col_nr, mz_col_nr, identity_col_nr, 
             setdiff( 1:(sample_col_nr-1),
                      c(rt_col_nr, mz_col_nr, identity_col_nr)) )]
      
    colnames(features)[1:3] <- c("rt", "mz", "Identity_raw")
    
    features <- 
      features %>% 
      mutate( Feature_type = ifelse(is.na(Identity_raw)| Identity_raw == "" |Identity_raw == "NA", 
                                    "Unknown", "Known"),
              Identity_raw = ifelse(Feature_type == "Unknown", 
                                     paste0("Unknown_MZ_", round(mz, 3), "_RT_", round(rt, 3)),
                                     Identity_raw),
              Feature_ID = paste0("F", row_number())) %>% 
      group_by(Identity_raw) %>% 
      mutate(n = n(),
             Identity = ifelse( n > 1, 
                                paste0(Identity_raw, "-iso", row_number()),
                                Identity_raw)) %>% 
      ungroup() %>% 
      select(-n) %>% 
      relocate(Identity, .before = Identity_raw)
    
    if (find_is){
      features <- 
        features %>% 
        mutate(Feature_type = ifelse( Feature_type == "Known" & 
                                        (grepl("^[0-9][0-9]\\.", substr(Identity_raw, 1, 3)) | grepl("-D[0-9]+$", Identity_raw)),
                                      "IS",
                                      Feature_type))
    }
    
    if (add_lipid_info){
      features <- 
        add_lipid_info(features,
                       keep.lipid.orig = keep.lipid.orig)
        
    }
    
    if (!is.null(meta)){
    
      file <- 
        readxl::read_excel(meta) %>%
        mutate(across(where(is.character), stringr::str_trim)) %>% 
        group_by(Sample) %>% 
        mutate(n = n(),
               Sample = ifelse(n > 1, 
                               paste0(Sample, "-", row_number()),
                               Sample)) %>%
        ungroup() %>% 
        select(-n)
        
      
    } else {
      
      # file <- 
      #   data.frame(
      #     File.name = colnames(peaks),
      #     Sample = colnames(peaks),
      #     Sample.type = "Sample",
      #     Sample.batch = "No batch"
      #   )
      
      File.name <- colnames(peaks)
      Run.order <- as.numeric(gsub(".d", "", str_extract(File.name, "[0-9]+.d$")))
      file <- 
        data.frame(
          File.name,
          Sample = File.name,
          Run.order) %>% 
        mutate(Sample.type = case_when(grepl("_PO[0-9]+_", File.name) ~ "PO",
                                       grepl("_NIST[0-9]+_", File.name) ~ "NIST",
                                       grepl("_BL[0-9]+_", File.name) ~ "BL",
                                       grepl("_CP[0-9]+_", File.name) ~ "CP",
                                       TRUE ~ "Sample"),
               Sample.seq = row_number()) %>% 
        arrange(Run.order) %>% 
        mutate(Sample.batch = paste0("Batch ", row_number() %/% 100 + 1)) %>% 
        arrange(Sample.seq)
      
    }
    
    rownames(peaks) <- features$Feature_ID
    colnames(peaks) <- file$Sample
    
    datalist$peaks <- peaks
    datalist$peaks_raw <- peaks
    datalist$features <- features
    datalist$features_raw <- features    
    datalist$meta <- file
    datalist$meta_raw <- file
    
    return(datalist)
}
