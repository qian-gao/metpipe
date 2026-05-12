#' Import peak tables for positive/negative modes
#'
#' Wrapper around [import_peaktable()] that loads one or both ionization modes
#' and returns a combined `datalist` object used by downstream workflow steps.
#'
#' @param peaktable_pos Path to positive-mode peak table file.
#' @param peaktable_neg Path to negative-mode peak table file.
#' @param rt_col_nr Column index of retention time in the peak table.
#' @param mz_col_nr Column index of m/z in the peak table.
#' @param identity_col_nr Column index of feature identity annotation.
#' @param sample_col_nr First sample-intensity column index.
#' @param meta_file_pos Optional metadata file/data frame for positive mode.
#' @param meta_file_neg Optional metadata file/data frame for negative mode.
#' @param find_is Logical; detect isotope-labelled internal standards by name.
#' @param add_lipid_info Logical; enrich annotations with lipid class parsing.
#' @param keep.lipid.orig Logical; keep original lipid names when parsing.
#' @param standardized.name Logical; infer metadata fields from standardized file names.
#' @param standerdized.name Deprecated alias for `standardized.name`.
#' @param batch_size Number of samples per batch.
#' @return A `metpipe_datalist` with `pos` and/or `neg` elements,
#'   each returned by [import_peaktable()].
#' @export
#'
#' @examples
#' \dontrun{
#' datalist <- run_import_peaktable(
#'   peaktable_pos = "pos.xlsx",
#'   peaktable_neg = "neg.xlsx",
#'   rt_col_nr = 1,
#'   mz_col_nr = 2,
#'   identity_col_nr = 3,
#'   sample_col_nr = 8
#' )
#' }
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
    standerdized.name = NULL,
    batch_size = 100
  ){

    if (!is.null(standerdized.name)) {
      warning("'standerdized.name' is deprecated; use 'standardized.name'", call. = FALSE)
      standardized.name <- standerdized.name
    }

    has_file <- function(path) {
      !is.null(path) && nzchar(path) && file.exists(path)
    }
    
    datalist <- list(pos = NULL, neg = NULL)
    
    if (has_file(peaktable_pos)){
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
          standardized.name = standardized.name,
          batch_size = batch_size
        )
    } else {
      datalist$pos <- NULL
    }
    
    if (has_file(peaktable_neg)){
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
          standardized.name = standardized.name,
          batch_size = batch_size
        )
    } else {
      datalist$neg <- NULL
    }

    if (is.null(datalist$pos) && is.null(datalist$neg)) {
      stop("No valid peak table was found. Provide existing file path(s) via peaktable_pos and/or peaktable_neg.")
    }

    return(as_metpipe_datalist(datalist, stage = "imported"))
  }