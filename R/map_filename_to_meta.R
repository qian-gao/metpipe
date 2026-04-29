#' Map file names to meta
#'
#' Parse raw file names into metadata fields.
#'
#' @param raw_files Character vector of raw data file paths or names.
#'
#' @returns A metadata data frame with sample type, run order, and batch columns.
#' @export
#' 
#' @import stringr dplyr
#' @examples
map_filename_to_meta <- 
  function(raw_files){

    if (is.null(raw_files) || length(raw_files) == 0) {
      stop("raw_files must be a non-empty character vector")
    }
    
    File.name <- basename(raw_files)
    Run.order <- as.numeric(gsub(".d", "", str_extract(File.name, "[0-9]+.d$")))
    
    file <-
      data.frame(
        File.name,
        stringr::str_split_fixed(
          string = File.name,
          pattern = "_",
          n = Inf
        )[, c(1:3, 5, 6, 7)],
        Run.order,
        stringsAsFactors = FALSE
      )
    
    colnames(file) <-
      c("File.name", "Project", "Method", "Date", "Sample", "Extract.rep", "Tech.rep" , "Run.order")
    
    file <- 
      file %>% 
      mutate(Sample.type = case_when(grepl("_PO[0-9]+_|_PO_|_POP[0-9]+_", File.name) ~ "PO",
                                     grepl("_NIST[0-9]+_|_NIST_|_NISTP[0-9]+_", File.name) ~ "NIST",
                                     grepl("_BL[0-9]+_|_BL_|_BLP[0-9]+_", File.name) ~ "BL",
                                     grepl("_CP[0-9]+_|_CP_|_CPP[0-9]+_", File.name) ~ "CP",
                                     grepl("_POJ[0-9]+_|_POJ_|_POJP[0-9]+_", File.name) ~ "Sample",
                                     grepl("_POK[0-9]+_|_POK_|_POKP[0-9]+_", File.name) ~ "POK",  
                                     grepl("_sol[0-9]+_|_sol_", File.name) ~ "sol", 
                                     TRUE ~ "Sample"),
             Sample.seq = row_number(),
             Sample = paste0(Sample, "_", Extract.rep),
             Run.order = dense_rank(Run.order)) %>% 
      arrange(Run.order) %>% 
      mutate(Sample.batch = paste0("Batch ", row_number() %/% 100 + 1)) %>% 
      arrange(Sample.seq) %>% 
      group_by(Sample) %>% 
      mutate(n = n(),
             Sample = ifelse(n > 1, 
                             paste0(Sample, "-", row_number()),
                             Sample)) %>%
      ungroup() %>% 
      dplyr::select(-n) 
    
    return(file)
    
  }
