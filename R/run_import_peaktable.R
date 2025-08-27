#' run_import_peaktable
#'
#' @param peaktable_pos 
#' @param peaktable_neg 
#' @param rt_col_nr 
#' @param mz_col_nr 
#' @param identity_col_nr 
#' @param sample_col_nr 
#' @param meta_file_pos 
#' @param meta_file_neg 
#' @param find_is 
#' @param add_lipid_info 
#' @param keep.lipid.orig 
#'
#' @return
#' @export
#'
#' @examples
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
    keep.lipid.orig = NULL,
    standerdized.name = FALSE
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
          keep.lipid.orig = keep.lipid.orig,
          standerdized.name = standerdized.name
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
          keep.lipid.orig = keep.lipid.orig,
          standerdized.name = standerdized.name
        )
    } else {
      datalist$neg <- NULL
    }
    
    return(datalist)
  }