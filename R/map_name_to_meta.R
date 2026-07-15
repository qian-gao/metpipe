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
map_name_to_meta <- 
  function(raw_files,
           qc_types = c("BL", "NIST", "PO", "sol", "CP", "IQ", "BPL", "MMix"),
           #pasef_as_dda = FALSE,
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
          Project = "",
          Method = "",
          Date = "",
          Run.seq = Run.order,
          Sample = "",
          Extract.rep = "",
          Tech.rep = "",
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
          Project = "",
          Method = "",
          Date = "",
          Run.seq = Run.order,
          Sample = "",
          Extract.rep = "",
          Tech.rep = "",
          Run.order,
          stringsAsFactors = FALSE
        )

      colnames(file) <-
        c("File.name", "Project", "Method", "Date", "Run.seq", "Sample", "Extract.rep", "Tech.rep" , "Run.order")

    }
    
    
    file$Sample.type <- sapply(file$File.name, function(x) {
      match <- qc_types[str_detect(x, qc_types)]
      if (length(match) > 0) match[1] else "Sample"
    })
    
    file <-
      file %>% 
      mutate(Run.order = dense_rank(Run.order)) %>% 
      arrange(Run.order) %>% 
      mutate(Sample.batch = paste0("Batch ", row_number() %/% batch_size + 1))
    
    return(file)
    
  }
