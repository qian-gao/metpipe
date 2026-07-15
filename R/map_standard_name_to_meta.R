#' Map file names to meta
#'
#' Parse standardized raw file names into metadata fields.
#'
#' @param raw_files Character vector of raw data file paths or names.
#' @param qc_types Character vector of QC sample labels recognized in names.
#' @param pasef_as_dda Logical; treat PASEF acquisitions as DDA.
#' @param batch_size Number of samples per batch.
#' 
#' @returns A metadata data frame with sample type, run order, and batch columns.
#' @export
#' 
#' @import stringr dplyr
#' @examples
map_standard_name_to_meta <- 
  function(raw_files,
           qc_types = c("BL", "NIST", "PO", "sol", "CP", "IQ", "BPL", "MMix"),
           pasef_as_dda = FALSE,
           batch_size = 100){

    if (is.null(raw_files) || length(raw_files) == 0) {
      stop("raw_files must be a non-empty character vector")
    }
    
    File.name <- basename(raw_files)

    if (grepl(".raw$", File.name[1], ignore.case = TRUE)) {
      
      Run.order <- 1:length(File.name)

      file <-
        data.frame(
          File.name,
          stringr::str_split_fixed(
            string = gsub(".raw", "", File.name),
            pattern = "-",
            n = Inf
          )[, c(1:7)],
          Run.order,
          stringsAsFactors = FALSE
        )

      colnames(file) <-
        c("File.name", "Project", "Method", "Date", "Run.seq", "Sample", "Extract.rep", "Tech.rep" , "Run.order")
      
      file <-
        file %>% 
        mutate(Run.order = Run.seq)

    } else {

      Run.order <- as.numeric(gsub(".d|.mzML", "", str_extract(File.name, "[0-9]+.d$|[0-9]+.mzML$")))

      file <-
        data.frame(
          File.name,
          stringr::str_split_fixed(
            string = gsub(".d|.mzML", "", File.name),
            pattern = "_",
            n = Inf
          )[, c(1:7)],
          Run.order,
          stringsAsFactors = FALSE
        )

      colnames(file) <-
        c("File.name", "Project", "Method", "Date", "Run.seq", "Sample", "Extract.rep", "Tech.rep" , "Run.order")

    }
    
    if (pasef_as_dda){
      
      file <- 
        file %>% 
        mutate(Sample.type = case_when(grepl("PASEF|DDA", Method) ~ "DDA",
                                       Sample %in% qc_types ~ Sample,
                                       TRUE ~ "Sample"))

    } else if (sum(grepl("DDA", file$Method)) == nrow(file)) {
      
      file <- 
        file %>% 
        mutate(Sample.type = case_when(Sample %in% qc_types ~ Sample,
                                       TRUE ~ "Sample"))
 
    } else {

      file <- 
        file %>% 
        mutate(Sample.type = case_when(grepl("DDA", Method) ~ "DDA",
                                       Sample %in% qc_types ~ Sample,
                                       TRUE ~ "Sample"))

    }
    
    file <-
      file %>% 
      mutate(Sample = case_when(Sample.type %in% qc_types ~ paste(Sample, Extract.rep, Tech.rep, sep = "_"),
                                  Sample.type == "DDA" ~ paste("DDA", Sample, Extract.rep, Tech.rep, sep = "_"),
                                  TRUE ~ Sample),
             Run.order = dense_rank(Run.order)) %>% 
        arrange(Run.order) %>% 
        mutate(Sample.batch = paste0("Batch ", row_number() %/% batch_size + 1)) %>% 
        group_by(Sample) %>% 
        mutate(n = n(),
               Sample = ifelse(n > 1, 
                               paste0(Sample, "-", row_number()),
                               Sample)) %>%
        ungroup() %>% 
        dplyr::select(-n)
    
    return(file)
    
  }
