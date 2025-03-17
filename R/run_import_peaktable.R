run_import_peaktable <- 
  function(
    peaktable_pos = NULL,
    peaktable_neg = NULL,
    
    rt_col_nr = NULL,
    mz_col_nr = NULL,
    identity_col_nr = NULL,
    sample_col_nr = NULL,
    
    meta_file_pos = NULL,
    meta_file_neg = NULL,
    
    find_is = FALSE,
    add_lipid_info = FALSE,
    keep.lipid.orig = NULL
  ){
    
    datalist <- list()
    
    if (!is.null(peaktable_pos)){
      datalist$pos <- 
        import_peaktable(
          peaktable = peaktable_pos,
          meta = meta_file_pos,
          rt_col_nr = rt_col_nr,
          mz_col_nr = mz_col_nr,
          identity_col_nr = identity_col_nr,
          sample_col_nr = sample_col_nr,
          find_is = find_is,
          add_lipid_info = add_lipid_info,
          keep.lipid.orig = keep.lipid.orig
        )
    } else {
      datalist$pos <- NULL
    }
    
    if (!is.null(peaktable_neg)){
      datalist$neg <- 
        import_peaktable(
          peaktable = peaktable_neg,
          meta = meta_file_neg,
          rt_col_nr = rt_col_nr,
          mz_col_nr = mz_col_nr,
          identity_col_nr = identity_col_nr,
          sample_col_nr = sample_col_nr,
          find_is = find_is,
          add_lipid_info = add_lipid_info,
          keep.lipid.orig = keep.lipid.orig
        )
    } else {
      datalist$neg <- NULL
    }
    
    return(datalist)
  }