#' @title prepare_mzmine_xml
#'
#' @description General MZmine xml file
#'
#' @param type The type of preprocessing the xml will do. "is" or "peak" for preprocessing
#'        internal standards or all metabolites.
#' @param path_mzML Path to the folder containing mzML files
#' @param path_output Path to the output folder
#' @param mode Polarity, 'pos' or 'neg'
#' @param path_lib Path to the library file
#' @param mzmine_mz_tol m/z tolerance for feature detection
#' @param mzmine_mz_tol_bigger m/z tolerance for alignment
#' @param mzmine_rt_tol Retention time tolerance for feature detection
#' @param mzmine_rt_tol_bigger Retention time tolerance for alignment
#' @param mzmine_min_peaks_in_row Minimum number of samples containing the peak
#' @param mzmine_min_peaks_in_row_percent Minimum percentage of samples containing the peak
#'
#' @return A xml file for MZMine processing
#' @export

prepare_mzmine_xml <-
  function(
    type = NULL,
    path_mzML = NULL,
    path_output = NULL,
    mode = NULL,
    path_lib = NULL,
    mzmine_mz_tol = NULL,
    mzmine_mz_tol_bigger = NULL,
    mzmine_rt_tol = NULL,
    mzmine_rt_tol_bigger = NULL,
    mzmine_min_peaks_in_row = NULL,
    mzmine_min_peaks_in_row_percent = NULL

  ){

    ##### MZMine settings #####

    params_mzmine <- list()

    ### Mass detection ###
    params_mzmine$P_MD_MASS_DETECTOR='Centroid'
    params_mzmine$P_MD_NOISE_LEVEL=500

    ### Targeted feature detection ###
    ## e.g.
    ## P_TF_SEPARATOR=';'
    ## P_TF_FEATURE_LIST='/home/projects/ku_00007/data/Database/database.csv'
    ## Percentage parameter P_TF_INTENSITY_TOLERANCE. e.g. 50% should be inputted as 0.5

    params_mzmine$P_TF_FEATURE_LIST=path_lib
    params_mzmine$P_TF_SEPARATOR=';'
    params_mzmine$P_TF_INTENSITY_TOLERANCE=0.2
    params_mzmine$P_TF_NOISE_LEVEL=500
    params_mzmine$P_TF_MZ_ABS_TOLERANCE=mzmine_mz_tol
    params_mzmine$P_TF_MZ_PPM_TOLERANCE=0.0
    params_mzmine$P_TF_RT_ABS_TOLERANCE=mzmine_rt_tol

    ### Peak filter (optional) ###
    ## It is possible to use some of the filters without the rest. If chosen, both MIN and MAX should be filled out
    params_mzmine$P_PF_DURATION_MIN=NULL
    params_mzmine$P_PF_DURATION_MAX=NULL
    params_mzmine$P_PF_AREA_MIN=NULL
    params_mzmine$P_PF_AREA_MAX=NULL
    params_mzmine$P_PF_HEIGHT_MIN=1000
    params_mzmine$P_PF_HEIGHT_MAX=1E9
    params_mzmine$P_PF_DATA_POINTS_MIN=5
    params_mzmine$P_PF_DATA_POINTS_MAX=100
    params_mzmine$P_PF_FWHM_MIN=NULL
    params_mzmine$P_PF_FWHM_MAX=NULL
    params_mzmine$P_PF_TAILING_MIN=NULL
    params_mzmine$P_PF_TAILING_MAX=NULL
    params_mzmine$P_PF_ASYMMETRY_MIN=NULL
    params_mzmine$P_PF_ASYMMETRY_MAX=NULL

    ### Feature list rows filter 1 (optional) ###
    ## It is possible to use only one of the filter
    params_mzmine$P_RF1_MIN_PEAK_IN_ROW=NULL
    params_mzmine$P_RF1_MIN_PEAK_IN_ISO=NULL

    # Smoothing (optional)
    params_mzmine$P_SM_FILTER_WIDTH=NULL

    ### Sort feature lists (implemented automatically) ###

    ### Join aligner ###
    params_mzmine$P_JA_MZ_ABS_TOLERANCE=mzmine_mz_tol_bigger
    params_mzmine$P_JA_MZ_PPM_TOLERANCE=0.0
    params_mzmine$P_JA_RT_ABS_TOLERANCE=mzmine_rt_tol_bigger
    params_mzmine$P_JA_MZ_WEIGHT=2
    params_mzmine$P_JA_RT_WEIGHT=1

    ### Duplicate filter (optional) ###
    params_mzmine$P_DF_MZ_ABS_TOLERANCE=mzmine_mz_tol
    params_mzmine$P_DF_MZ_PPM_TOLERANCE=0.0
    params_mzmine$P_DF_RT_ABS_TOLERANCE=mzmine_rt_tol_bigger

    ### Feature list rows filter 2 (optional) ###
    ## It is possible to use only one of the filter
    params_mzmine$P_MIN_PEAK_IN_ROW=mzmine_min_peaks_in_row
    params_mzmine$P_MIN_PEAK_IN_ROW_PERCENT=mzmine_min_peaks_in_row_percent
    params_mzmine$P_MIN_PEAK_IN_ISO=NULL

    ### Peak finder (optional) ###
    ## Percentage parameter P_GF_INTENSITY_TOLERANCE. e.g. 50% should be inputted as 0.5
    params_mzmine$P_GF_INTENSITY_TOLERANCE=0.2
    params_mzmine$P_GF_MZ_ABS_TOLERANCE=mzmine_mz_tol
    params_mzmine$P_GF_MZ_PPM_TOLERANCE=0.0
    params_mzmine$P_GF_RT_ABS_TOLERANCE=mzmine_rt_tol_bigger

    ### Custom database searh (optional) ###
    ## e.g.
    ## P_DB_SEPARATOR=';'
    ## P_DB_DATABASE='/home/projects/ku_00007/data/database.csv'

    params_mzmine$P_DB_DATABASE=NULL
    params_mzmine$P_DB_SEPARATOR=NULL
    params_mzmine$P_DB_MZ_ABS_TOLERANCE=NULL
    params_mzmine$P_DB_MZ_PPM_TOLERANCE=NULL
    params_mzmine$P_DB_RT_ABS_TOLERANCE=NULL

    ### Raw data path ###
    ## e.g. P_RAWPATH='/home/projects/ku_00007/data/metabolomics'
    params_mzmine$P_RAWPATH=path_mzML

    ### Export to csv ###
    ## e.g.
    ## P_EX_FILE_ELEMENT='Peak area' or 'Peak height'
    ## P_EX_SEPARATOR=';'
    ## P_EX_CSV='/home/projects/ku_00007/people/xxxx/peak_list.csv'

    params_mzmine$P_EX_FILE_ELEMENT='Peak area'
    params_mzmine$P_EX_SEPARATOR=';'

    if (type == "is"){
      params_mzmine$P_EX_CSV=paste0(path_output, "peaktable_targeted_IS_", mode, ".csv")
      params_mzmine$P_EX_MZTAB=paste0(path_output, "peaktable_targeted_IS_", mode, ".mztab")
    } else {
      params_mzmine$P_EX_CSV=paste0(path_output, "peaktable_", mode, ".csv")
      params_mzmine$P_EX_MZTAB=paste0(path_output, "peaktable_", mode, ".mztab")
    }

    ### Generate MZMine xml file ###

    template <- readRDS(system.file("template", "mzmine3.template.rds", package="metpipe"))

    if (type == "is"){

      template.use <- template$is

      generate_mzmine3_xml(
        template = template.use,
        params = params_mzmine,
        output_file = paste0(path_output, "MZMine_parameters_IS_", mode, ".xml") )

    } else {

      template.use <- template$peak

      generate_mzmine3_xml(
        template = template.use,
        params = params_mzmine,
        output_file = paste0(path_output, "MZMine_parameters_", mode, ".xml") )

    }
  }
