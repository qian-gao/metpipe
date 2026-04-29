#' Import a metabolomics peak table and construct a mode-level datalist
#'
#' Reads a peak table file (typically `.xlsx`), splits feature metadata and
#' sample-intensity matrix, harmonizes identities, and attaches sample metadata.
#'
#' @param peaktable Path to peak table file.
#' @param meta Optional sample metadata as a file path or data frame.
#' @param rt_col_nr Column index for retention time.
#' @param mz_col_nr Column index for m/z.
#' @param identity_col_nr Column index for raw feature identity.
#' @param sample_col_nr First sample-intensity column index.
#' @param find_is Logical; detect isotope-labelled internal standards by name.
#' @param add_lipid_info Logical; parse lipid annotations via [add_lipid_info()].
#' @param keep.lipid.orig Logical; keep original lipid names when parsing.
#' @param standardized.name Logical; infer metadata from standardized file names.
#'
#' @return A list with `peaks`, `features`, and `meta` (plus raw copies).
#' @examples
#' \dontrun{
#' dat <- import_peaktable(
#'   peaktable = "peaktable_pos.xlsx",
#'   rt_col_nr = 1,
#'   mz_col_nr = 2,
#'   identity_col_nr = 3,
#'   sample_col_nr = 8
#' )
#' }
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
      keep.lipid.orig = NULL,
      standardized.name = FALSE
  ){

    if (is.null(peaktable) || !nzchar(peaktable) || !file.exists(peaktable)) {
      stop("peaktable must be an existing file path")
    }

    required_cols <- c(rt_col_nr, mz_col_nr, identity_col_nr, sample_col_nr)
    if (any(is.null(required_cols))) {
      stop("rt_col_nr, mz_col_nr, identity_col_nr, and sample_col_nr must be provided")
    }

    datalist <- list()
    
    file <- openxlsx::read.xlsx(peaktable)

    if (sample_col_nr > ncol(file)) {
      stop("sample_col_nr exceeds number of columns in peaktable")
    }

    idx_check <- c(rt_col_nr, mz_col_nr, identity_col_nr)
    if (any(idx_check > ncol(file))) {
      stop("One or more feature column indices exceed number of columns in peaktable")
    }
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
      mutate( Feature_type = ifelse(is.na(Identity_raw)| Identity_raw == "" |Identity_raw == "NA" |grepl("Unknown|no MS2|RIKEN", Identity_raw), 
                                    "Unknown", "Known"),
              Identity_raw = ifelse(Feature_type == "Unknown", 
                                     paste0("Unknown_MZ_", round(mz, 3), "_RT_", round(rt, 3)),
                                     Identity_raw),
              Feature_ID = paste0("F", row_number())) %>% 
      group_by(Identity_raw) %>% 
      mutate(n = n(),
             Identity = ifelse( n > 1, 
                                paste0(Identity_raw, "-iso", row_number()),
                                Identity_raw),
             Identity = gsub("low score: ", "", Identity)) %>% 
      ungroup() %>% 
      dplyr::select(-n) %>% 
      relocate(Identity, .before = Identity_raw)
    
    if (find_is){
      features <- 
        features %>% 
        mutate(Feature_type = ifelse( Feature_type == "Known" & 
                                        #(grepl("^[0-9][0-9]\\.|-D[0-9]+$", substr(Identity_raw, 1, 3)) | grepl("-D[0-9]+$", Identity_raw)),
                                        (grepl("-13C[0-9]+", Identity_raw) | grepl("-13C$", Identity_raw) | 
                                         grepl("-D[0-9]+$", Identity_raw) | grepl("-15N$", Identity_raw)),
                                      "IS",
                                      Feature_type))
    }
    
    if (add_lipid_info){
      features <- 
        add_lipid_info(features,
                       keep.lipid.orig = keep.lipid.orig)
        
    }
    
    if (!is.null(meta)){
      if (!is.data.frame(meta)){
        file <- 
          readxl::read_excel(meta)
      } else {
        file <- meta
      }
        file <- 
          file %>%
          mutate(across(where(is.character), stringr::str_trim)) %>% 
          group_by(Sample) %>% 
          mutate(n = n(),
                 Sample = ifelse(n > 1, 
                                 paste0(Sample, "-", row_number()),
                                 Sample)) %>%
          ungroup() %>% 
          dplyr::select(-n)
        
      
    } else if (standardized.name) {
      
      File.name <- colnames(peaks)
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
        mutate(Sample.type = case_when(grepl("_PO[0-9]+_|_PO_", File.name) ~ "PO",
                                       grepl("_NIST[0-9]+_|_NIST_", File.name) ~ "NIST",
                                       grepl("_BL[0-9]+_|_BL_", File.name) ~ "BL",
                                       grepl("_CP[0-9]+_|_CP_", File.name) ~ "CP",
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
          Sample = NA,
          Run.order) %>% 
        mutate(Sample.type = case_when(grepl("_PO[0-9]+_|_PO_", File.name) ~ "PO",
                                       grepl("_NIST[0-9]+_|_NIST_", File.name) ~ "NIST",
                                       grepl("_BL[0-9]+_|_BL_", File.name) ~ "BL",
                                       grepl("_CP[0-9]+_|_CP_", File.name) ~ "CP",
                                       TRUE ~ "Sample"),
               Sample.seq = row_number(),
               Run.order = dense_rank(Run.order)) %>% 
        arrange(Run.order) %>% 
        mutate(Sample.batch = paste0("Batch ", row_number() %/% 100 + 1),
               Sample = File.name) %>% 
        arrange(Sample.seq)
      
    }
    
    # Adjust sequence
    peak_colnames <- make.names(colnames(peaks))
    samples <- make.names(file$File.name)
    seq <- match(samples, peak_colnames)
    peaks <- peaks[, seq]
    
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
