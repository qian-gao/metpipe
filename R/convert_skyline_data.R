#' @title convert_skyline_data
#'
#' @description Convert peaktable exported from skyline to needed format
#' 
#' @param file File of skyline exported peaktable
#' @param mode 'pos' or 'neg' indicating positive or negative mdoe
#' @param path.result Path to output folder
#' 
#' @return IS and Peaks table
#' 
#' @export
#' 

convert_skyline_data <- 
  function(
    file = NULL,
    mode = NULL,
    path.result = NULL
  ){
    raw <- 
      read.csv(file, sep = ",") %>% 
      mutate(rt = 1,
             Replicate.Name = paste0(Replicate.Name, ".mzML"),
             Area = as.numeric(Area),
             Area = if_else(is.na(Area), 0, Area)) %>% 
      filter(Fragment.Ion == "precursor") %>% 
      filter(Peak.Rank %in% c("1", "2")) %>% 
      select(-c("Precursor.Adduct", "Precursor.Charge", "Fragment.Ion", "Product.Adduct", 
                "Product.Charge", "Background", "Peak.Rank")) %>%
      dplyr::rename("Feature m/z" = "Product.Mz",
                    "Feature RT" = "Retention.Time",
                    "Peak area" = "Area") %>% 
      pivot_wider(names_from = Replicate.Name, 
                  values_from = c("Feature m/z", "Feature RT", "Peak area"),
                  names_glue = "{Replicate.Name} {.value}") %>% 
      group_by(Molecule) %>% 
      filter(row_number() == 1) %>%
      dplyr::rename("row retention time" = "rt",
                    "row m/z" = "Precursor.Mz",
                    "row identity (main ID)" = "Molecule") %>% 
      relocate(c("row m/z", "row retention time", "row identity (main ID)"), .after = "Molecule.List.Name")
    
    raw_is <- 
      raw %>% 
      filter(Molecule.List.Name == "IS") %>% 
      select(-Molecule.List.Name) %>% 
      mutate(ID = row_number()) %>% 
      relocate(ID, .before = "row m/z")
    
    raw_peak <- 
      raw %>% 
      select(-Molecule.List.Name) %>% 
      mutate(ID = row_number()) %>% 
      relocate(ID, .before = "row m/z")
    
    keep_names <- !grepl(" Feature m/z| Feature RT", names(raw_peak))
    raw_peak <- raw_peak[, keep_names]
    
    dir.create(paste0(path.result, "/01_01_Preprocessing_IS/"))
    write.table(raw_is, sep = ";", row.names = FALSE,
                file = paste0(path.result, "01_01_Preprocessing_IS/peaktable_targeted_IS_", mode, ".csv"))
    
    dir.create(paste0(path.result, "/02_Preprocessing/"))
    write.table(raw_peak, sep = ";", row.names = FALSE,
                file = paste0(path.result, "02_Preprocessing/peaktable_", mode, ".csv"))
    
    result <- list(
      raw_is = raw_is,
      raw_peak = raw_peak
    )
    
    return(result)
  }