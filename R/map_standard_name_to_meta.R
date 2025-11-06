#' Map file names to meta
#'
#' @param raw_files 
#'
#' @returns
#' @export
#' 
#' @import stringr dplyr
#' @examples
map_standard_name_to_meta <- 
  function(raw_files,
           qc_types = c("BL", "NIST", "PO", "sol", "CP", "IQ", "BPL", "MMix"),
           pasef_as_dda = FALSE){
    
    File.name <- basename(raw_files)
    Run.order <- as.numeric(gsub(".d|.mzML", "", str_extract(File.name, "[0-9]+.d$|[0-9]+.mzML$")))
    
    file <-
      data.frame(
        File.name,
        stringr::str_split_fixed(
          string = File.name,
          pattern = "_",
          n = Inf
        )[, c(1:7)],
        Run.order,
        stringsAsFactors = FALSE
      )
    
    colnames(file) <-
      c("File.name", "Project", "Method", "Date", "Run.seq", "Sample", "Extract.rep", "Tech.rep" , "Run.order")
    
    if (pasef_as_dda){
      
      file <- 
        file %>% 
        mutate(Sample.type = case_when(grepl("PASEF|DDA", Method) ~ "DDA",
                                       Sample %in% qc_types ~ Sample,
                                       TRUE ~ "Sample"),
               #Sample.seq = row_number(),
               Sample = case_when(Sample.type %in% qc_types ~ paste(Sample, Extract.rep, Tech.rep, sep = "_"),
                                  Sample.type == "DDA" ~ paste("DDA", Sample, Extract.rep, Tech.rep, sep = "_"),
                                  TRUE ~ Sample),
               Run.order = dense_rank(Run.order)) %>% 
        arrange(Run.order) %>% 
        mutate(Sample.batch = paste0("Batch ", row_number() %/% 100 + 1)) %>% 
        #arrange(Sample.seq) %>% 
        group_by(Sample) %>% 
        mutate(n = n(),
               Sample = ifelse(n > 1, 
                               paste0(Sample, "_", Tech.rep),
                               Sample)) %>%
        ungroup() %>% 
        group_by(Sample) %>% 
        mutate(n = n(),
               Sample = ifelse(n > 1, 
                               paste0(Sample, "-", row_number()),
                               Sample)) %>%
        ungroup() %>% 
        select(-n)
      
    } else {
      
      file <- 
        file %>% 
        mutate(Sample.type = case_when(grepl("DDA", Method) ~ "DDA",
                                       Sample %in% qc_types ~ Sample,
                                       TRUE ~ "Sample"),
               #Sample.seq = row_number(),
               Sample = case_when(Sample.type %in% qc_types ~ paste(Sample, Extract.rep, Tech.rep, sep = "_"),
                                  Sample.type == "DDA" ~ paste("DDA", Sample, Extract.rep, Tech.rep, sep = "_"),
                                  TRUE ~ Sample),
               Run.order = dense_rank(Run.order)) %>% 
        arrange(Run.order) %>% 
        mutate(Sample.batch = paste0("Batch ", row_number() %/% 100 + 1)) %>% 
        #arrange(Sample.seq) %>% 
        group_by(Sample) %>% 
        mutate(n = n(),
               Sample = ifelse(n > 1, 
                               paste0(Sample, "_", Tech.rep),
                               Sample)) %>%
        ungroup() %>% 
        group_by(Sample) %>% 
        mutate(n = n(),
               Sample = ifelse(n > 1, 
                               paste0(Sample, "-", row_number()),
                               Sample)) %>%
        ungroup() %>% 
        select(-n) 
    }
    
    return(file)
    
  }
