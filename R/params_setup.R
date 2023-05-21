#' @title params_setup
#'
#' @description Set up preprocessing parameters
#'
#' @param params A list of parameters, set as NULL if nothing has been set up yet
#' @param run.pipeline If run whole pipeline or not, TRUE/FALSE
#' @param path.mzmine Path of MZmine
#' @param BPPARAM_set Parallel preprocessing setup in XCMS
#' @param qc.sample.type QC sample types, default is c( "sol", "BL", "CP", "IQ",
#'    "NIST", "CAL", "Met", "M-Mix", "SPO", "PO", "PO50", "PO12.5", "PO25", "PO100", "BPO" )
#' @param calibration.sample.type Calibration sample types, default is c( "CA00",
#'    "CA01", "CA02","CA03", "CA04", "CA05", "CA06", "CA07", "CA08", "CA09" )
#' @param path.mzml Path to mzML files
#' @param standard.name If standardized sample names are used, TRUE/FALSE
#' @param path.meta.pos Path to metadata file for samples from positive mode
#' @param path.meta.neg Path to metadata file for samples from negative mode
#' @param meta.match.col Column name to match sample and metadata, "Sample" for
#'    standard.name = TRUE, "Sample.name" for standard.name = FALSE
#' @param rt_tolerance Retention time tolerance for matching library
#' @param mz_tolerance m/z for matching library
#' @param type.use.to.optimize Sample type used for optimizing XCMS parameters, default is "PO"
#' @param po.sample.to.use Sample type used for evaluating normalization, default is "PO"
#' @param method Choose from c("LIP", "RP", "HILIC")
#' @param path.result Path to result folder
#' @param sample.pattern Sample pattern of data files, default is ".mzML"
# Put "No_data" if not preprocessing the mode
#' @param path.mzml.pos Path to mzML positive mode files, put "No_data" if not preprocessing the mode
#' @param path.mzml.neg Path to mzML negative mode files, put "No_data" if not preprocessing the mode
#' @param path.lib Path to in house library xlsx/RData
#' @param path.lib.is Path to in house library for internal standards xlsx/RData
#'
#' @return A list of parameters
#'
#' @examples
#' @export

params_setup <-
  function(
    params = NULL,
    run.pipeline = TRUE,

    # system
    path.mzmine = "C:/Users/Public/Documents/MZmine-3.4.27/MZmine_console.exe",
    BPPARAM_set = NULL,

    # standard names
    qc.sample.type = c( "sol", "BL", "CP", "IQ", "NIST", "CAL", "Met", "M-Mix",
                        "SPO", "PO", "PO50", "PO12.5", "PO25", "PO100", "BPO" ),
    calibration.sample.type = c( "CA00", "CA01", "CA02","CA03", "CA04",
                                 "CA05", "CA06", "CA07", "CA08", "CA09" ),

    # mzML files
    path.mzml = NULL,
    standard.name = TRUE,

    # metadata
    path.meta.pos = NULL,
    path.meta.neg = NULL,
    meta.match.col = "Sample",

    # library match
    rt_tolerance = 0.3,
    mz_tolerance = 0.01,

    # Pool samples to use for evaluation
    type.use.to.optimize = "PO",
    po.sample.to.use = "PO",

    ### Only modify if not standard workflow
    method = NULL,
    path.result = NULL,
    sample.pattern = ".mzML",

    # Sample path
    # Put "No_data" if not preprocessing the mode
    path.mzml.pos = NULL,
    path.mzml.neg = NULL,

    # Library path
    path.lib = NULL,
    path.lib.is = NULL

  ){

    if (is.null(params)) params <- list()

    params$path.mzmine = path.mzmine
    params$run.pipeline = run.pipeline

    # system
    params$path.mzmine = path.mzmine
    if (is.null(BPPARAM_set)){

      params$BPPARAM_set = switch( Sys.info()["sysname"],
                                   Windows = BiocParallel::SnowParam(max(1, min(4, parallel::detectCores()-1)), progressbar = TRUE),
                                   BiocParallel::MulticoreParam(max(1, min(4, parallel::detectCores()-1)), progressbar = TRUE))

    } else {
      params$BPPARAM_set = BPPARAM_set
    }

    # standard names
    params$qc.sample.type = qc.sample.type
    params$calibration.sample.type = calibration.sample.type

    # mzML files
    params$path.mzml = path.mzml
    params$standard.name = standard.name

    # metadata
    params$path.meta.pos = path.meta.pos

    if (is.null(path.meta.neg)){
      params$path.meta.neg = path.meta.pos
    } else {
      params$path.meta.neg = path.meta.neg
    }

    params$meta.match.col = meta.match.col

    # library match
    params$rt_tolerance = rt_tolerance
    params$mz_tolerance = mz_tolerance

    # Pool samples to use for evaluation
    params$type.use.to.optimize = type.use.to.optimize
    params$po.sample.to.use = po.sample.to.use

    ### Only modify if not standard workflow

    if (is.null(method)) {
      params$method = strsplit(strsplit(path.mzml, "mzML/")[[1]][2], "_")[[1]][2]
    } else {
      params$method = method
    }

    if (is.null(path.result)) {
      params$path.result = gsub("mzML", "peaktable", path.mzml)
    } else {
      params$path.result = path.result
    }

    params$sample.pattern = sample.pattern

    # Sample path
    if (is.null(path.mzml.pos)) {
      params$path.mzml.pos = paste0(path.mzml, "pos/")
    } else {
      params$path.mzml.pos = path.mzml.pos
    }

    if (is.null(path.mzml.neg)) {
      params$path.mzml.neg = paste0(path.mzml, "neg/")
    } else {
      params$path.mzml.neg = path.mzml.neg
    }

    # Library path
    params$path.lib = path.lib
    params$path.lib.is = path.lib.is

    # Path setup
    if (!dir.exists(params$path.result)) dir.create(params$path.result)

    # Generate sample info
    if ( params$path.mzml.pos != "No_data" ){

      sample.info.pos <-
        extract_sample_info( path = params$path.mzml.pos,
                             path.meta = params$path.meta.pos,
                             meta.match.col = params$meta.match.col,
                             sample.pattern = params$sample.pattern,
                             standard.name = params$standard.name,
                             qc.sample.type = params$qc.sample.type,
                             calibration.sample.type = params$calibration.sample.type,
                             export.rds = paste0(params$path.result, "sample_info_pos.rds"),
                             export.xlsx = paste0(params$path.result, "sample_info_pos.xlsx"))

    }

    if ( params$path.mzml.neg != "No_data" ){

      sample.info.neg <-
        extract_sample_info( path = params$path.mzml.neg,
                             path.meta = params$path.meta.neg,
                             meta.match.col = params$meta.match.col,
                             sample.pattern = params$sample.pattern,
                             standard.name = params$standard.name,
                             qc.sample.type = params$qc.sample.type,
                             calibration.sample.type = params$calibration.sample.type,
                             export.rds = paste0(params$path.result, "sample_info_neg.rds"),
                             export.xlsx = paste0(params$path.result, "sample_info_neg.xlsx"))

    }

    params$sample.info.pos <- sample.info.pos
    params$sample.info.neg <- sample.info.neg

    params$run.pos <- exists("sample.info.pos")
    params$run.neg <- exists("sample.info.neg")

    if ( grepl(".xlsx", path.lib) ){

      lib <- list()

      lib[[params$method]]$pos <- openxlsx::read.xlsx( path.lib, "POS" )
      lib[[params$method]]$neg <- openxlsx::read.xlsx( path.lib, "NEG" )

    } else {

      load(path.lib)

    }

    if ( grepl(".xlsx", path.lib.is) ){

      lib.is <- list()

      lib.is[[params$method]]$pos <- openxlsx::read.xlsx( path.lib.is, "POS" )
      lib.is[[params$method]]$neg <- openxlsx::read.xlsx( path.lib.is, "NEG" )

    } else {

      load(path.lib.is)

    }

    params$lib <- lib
    params$lib.is <- lib.is

    return(params)
  }
