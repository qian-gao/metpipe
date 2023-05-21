#' @title generate_mzmine3_xml
#'
#' @description Generate MZmine xml file
#'
#' @param template = NULL,
#' @param params = NULL,
#' @param output_file = NULL
#'
#' @return A list of summaries for all types of samples
#'
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%" filter

generate_mzmine3_xml <-
  function(
    template = NULL,
    params = NULL,
    output_file = NULL
  ){

    params$RAW_FILES <-
      paste( paste0( "<file>",
                     list.files( params$P_RAWPATH, pattern = ".mzML",
                                 recursive = TRUE, full.names = TRUE, include.dirs = TRUE),
                     "</file>" ),
             collapse = "" )

    modules <- c("start", "MSDKmzMLImportModule")

    if (!is.null(params$P_MD_NOISE_LEVEL)) modules <- c(modules, "MassDetectionModule")

    if (!is.null(params$P_CB_METHOD)){

      if (params$P_CB_METHOD == "Chromatogram builder") modules <- c(modules, "ChromatogramBuilder")
      else if (params$P_CB_METHOD == "ADAP") modules <- c(modules, "ADAPChromatogramBuilder")
    }

    if (!is.null(params$P_DECONVOLUTION)) modules <- c(modules, "Deconvolution")

    if (!is.null(params$P_TF_FEATURE_LIST)) modules <- c(modules, "TargetedFeatureDetectionModule")

    if (!is.null(params$P_DEISO_ABS_TOLERANCE)) modules <- c(modules, "IsotopeGrouper")

    if (!is.null(params$P_PF_DURATION_MIN)) params$P_PF_DURATION <- 'true' else params$P_PF_DURATION <- 'false'
    if (!is.null(params$P_PF_AREA_MIN)) params$P_PF_AREA <- 'true' else params$P_PF_AREA <- 'false'
    if (!is.null(params$P_PF_HEIGHT_MIN)) params$P_PF_HEIGHT <- 'true' else params$P_PF_HEIGHT <- 'false'
    if (!is.null(params$P_PF_DATA_POINTS_MIN)) params$P_PF_DATA_POINTS <- 'true' else params$P_PF_DATA_POINTS <- 'false'
    if (!is.null(params$P_PF_FWHM_MIN)) params$P_PF_FWHM <- 'true' else params$P_PF_FWHM <- 'false'
    if (!is.null(params$P_PF_TAILING_MIN)) params$P_PF_TAILING <- 'true' else params$P_PF_TAILING <- 'false'
    if (!is.null(params$P_PF_ASYMMETRY_MIN)) params$P_PF_ASYMMETRY <- 'true' else params$P_PF_ASYMMETRY <- 'false'

    if ('true' %in% c(params$P_PF_DURATION, params$P_PF_AREA, params$P_PF_HEIGHT, params$P_PF_DATA_POINTS, params$P_PF_FWHM, params$P_PF_TAILING, params$P_PF_ASYMMETRY) ) modules <- c(modules, "FeatureFilterModule")

    if (!is.null(params$P_SM_FILTER_WIDTH)) modules <- c(modules, "Smoothing")

    if (!is.null(params$P_JA_MZ_ABS_TOLERANCE)) modules <- c(modules, "JoinAlignerModule")

    if (!is.null(params$P_DF_MZ_ABS_TOLERANCE)) modules <- c(modules, "DuplicateFilterModule")

    steps <-
      template %>%
      filter( module %in% modules)

    param_list <- params
    names(param_list) <- paste0("\\$\\{", names(param_list), "\\}")

    output <-
      data.frame(
        step = stringr::str_replace_all(steps$step, unlist(param_list)) )

    modules <- c()

    # params$P_MIN_PEAK_IN_ROW <- params$P_RF2_MIN_PEAK_IN_ROW
    # params$P_MIN_PEAK_IN_ISO <- params$P_RF2_MIN_PEAK_IN_ISO

    if (!is.null(params$P_MIN_PEAK_IN_ROW)) params$P_PEAK_IN_ROW <- 'true' else params$P_PEAK_IN_ROW <- 'false'
    if (!is.null(params$P_MIN_PEAK_IN_ISO)) params$P_PEAK_IN_ISO <- 'true' else params$P_PEAK_IN_ISO <- 'false'

    if ('true' %in% c(params$P_PEAK_IN_ROW, params$P_PEAK_IN_ISO) ) modules <- c(modules, "RowsFilterModule")

    if (!is.null(params$P_GF_INTENSITY_TOLERANCE)) modules <- c(modules, "PeakFinderModule")

    if (!is.null(params$P_DB_DATABASE)) modules <- c(modules, "CustomDBSearch")

    if (!is.null(params$P_EX_CSV)) modules <- c(modules, "LegacyCSVExportModule")

    if (!is.null(params$P_EX_MZTAB)) modules <- c(modules, "MzTabExportModule")

    modules <- c(modules, "end")

    steps <-
      template %>%
      filter( module %in% modules)

    param_list <- params
    names(param_list) <- paste0("\\$\\{", names(param_list), "\\}")

    output2 <-
      data.frame(
        step = stringr::str_replace_all(steps$step, unlist(param_list)) )

    output.steps <- rbind(output, output2)

    write.table(output.steps, output_file,
                row.names = FALSE, col.names = FALSE,
                quote = FALSE)

  }
