# Usage
# feature_info = "Test_input.xlsx"
# library_file = "LCMS_library.xlsx"
# 
# result <-
#   match_library( feature_info,
#                  rt_tolerance = 0.1,
#                  mz_tolerance = 0.01,
#                  library_file)

match_library <-
  function( feature_info = NULL,
            rt_tolerance = 0.1,
            mz_tolerance = 0.01,
            library_file = NULL,
            verbose = FALSE ) {
    
    library(tidyverse)
    
    if ( verbose ) {
      
      print( "match_library was created by Qian Gao" )
      print( "qian.gao@sund.ku.dk" )
      
    }
    
    if (!is.data.frame(feature_info)){
      x <-
        openxlsx::read.xlsx(feature_info, sheet = 1) %>%
        dplyr::rename(id = 1, rt = 2, mz = 3) %>%
        mutate(rt = as.numeric(rt),
               mz = as.numeric(mz),
               identifier = row_number())
    } else {
      x <-
        feature_info %>%
        dplyr::rename(id = 1, rt = 2, mz = 3) %>%
        mutate(rt = as.numeric(rt),
               mz = as.numeric(mz),
               identifier = row_number())
    }

    if (!is.data.frame(library_file)){
      y <-
        openxlsx::read.xlsx(library_file, sheet = 1) %>%
        dplyr::rename(Library.mz = 1, Library.rt = 2, Library.name = 3) %>%
        mutate(Library.rt = as.numeric(Library.rt),
               Library.mz = as.numeric(Library.mz))
    } else {
      y <-
        library_file %>%
        mutate(Library.rt = as.numeric(Library.rt),
               Library.mz = as.numeric(Library.mz))
    }
    
    rt_diff <- outer(x$rt, y$Library.rt, "-") %>% as.data.frame()
    mz_diff <- outer(x$mz, y$Library.mz, "-") %>% as.data.frame()
    
    match.ind <- 
      abs(rt_diff) < rt_tolerance & abs(mz_diff) < mz_tolerance & !is.na(rt_diff) & !is.na(mz_diff)
    
    match.from.y <- 
      data.table::rbindlist(
        apply( data.frame(id = 1:nrow(match.ind)), 
               1,
               function(x) { y[match.ind[x, ], ] })
        , idcol = "identifier")
    
    result <-
      x %>%
        left_join(match.from.y, by = "identifier") %>%
        select(-identifier)
    
    return(result) 
  }
