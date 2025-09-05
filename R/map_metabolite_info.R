#' map_metabolite_info
#'
#' @param input_file A feature table in dataframe, xlsx or csv format. contains at least one
#'                   columns: metabolite name
#' @param name_col Position of the column containing metabolite names
#' @param sep Separator in input_file if it is csv format
#' @param db_file Database file to use for mapping metabolite info in csv format
#' @param db_sep Separator in db_file in csv format
#' @param path_output The path to store output .xlsx file
#'
#' @return A feature table with metabolite information
#' @importFrom magrittr %>%
#' @import dplyr stringi curl
#' @export
#' 
map_metabolite_info <- 
  function(input_file = NULL,
           name_col = 6,
           sep = ';',
           db_file = NULL,
           db_sep = ',',
           path_output = NULL){
    
    
    if (is.data.frame(input_file)){
      raw <- input_file
    } else if (substr(input_file, nchar(input_file)-4, nchar(input_file)) == '.xlsx') {
      raw <- readxl::read_excel(input_file)
    } else{
      raw <- read.table(input_file, header = TRUE, sep = sep)
    }
    
    names(raw)[name_col] <- "Metabolite_name_original"
    
    input_names <- 
      raw$Metabolite_name_original[!is.na(raw$Metabolite_name_original)] %>% 
      unique()
    
    mets = stringi::stri_join_list(list(input_names), sep="\n")
    h <- curl::new_handle()
    curl::handle_setform(h,  metabolite_name = mets)
    
    # Run the RefMet request on the Metabolomics Workbench server to convert 
    # metabolites name to standardized names
    req <- curl::curl_fetch_memory("https://www.metabolomicsworkbench.org/databases/refmet/name_to_refmet_new_min.php", handle = h)
    
    refmet <- read.table(text = rawToChar(req$content), header = TRUE, na.strings = "-", stringsAsFactors = FALSE, quote = "", comment.char = "", sep="\t");
    
    met_info <- refmet %>% unique()
    
    report <- 
      raw %>% 
      left_join(met_info[, 1:2], by = c("Metabolite_name_original" = "Input.name")) %>% 
      rename(Metabolite_name = Standardized.name) %>% 
      mutate(Metabolite_name = ifelse(is.na(Metabolite_name), Metabolite_name_original, Metabolite_name)) %>% 
      relocate(Metabolite_name, .before = Metabolite_name_original)
    
    # Load database
    db <- read.table(db_file, sep = db_sep, header = TRUE)
    
    # Map metabolites information
    output <- 
      report %>% 
      left_join(db, by = c('Metabolite_name' = 'refmet_name'))
    
    if (!is.null(path_output)){
      openxlsx::write.xlsx(output, file = paste0(path_output, "/feature_table_metabolite_info.xlsx"))
    }
    
    return(output)
  }
