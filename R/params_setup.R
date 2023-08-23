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

    # system
    path.mzmine = NULL,
    path.msdial = NULL,

    BPPARAM_set = NULL,

    # standard names
    qc.sample.type = c( "sol", "BL", "CP", "IQ", "NIST", "CAL", "Met", "M-Mix",
                        "SPO", "PO", "PO50", "PO12.5", "PO25", "PO100", "BPO" ),
    calibration.sample.type = c( "CA00", "CA01", "CA02","CA03", "CA04",
                                 "CA05", "CA06", "CA07", "CA08", "CA09" ),

    # mzML files
    path.mzml = NULL,
    standard.name = TRUE,
    target = NULL,

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
    path.lib.is = NULL,
    path.msp = NULL

  ){

    if (is.null(params)) params <- list()

    # system

    if (!is.null(path.mzmine)){

      params$path.mzmine = path.mzmine

    } else if ( file.exists( "/home/projects/ku_00007/" )){

      params$path.mzmine = "/home/projects/ku_00007/apps/MZmine/MZmine-3.4.27-Linux/bin/start_mzmine3"

    } else {

      params$path.mzmine = "C:/Users/Public/Documents/MZmine-3.4.27/MZmine_console.exe"

    }

    if (!is.null(path.msdial)){

      params$path.msdial = path.msdial

    } else if ( file.exists( "/home/projects/ku_00007/" )){

      params$path.msdial = "/home/projects/ku_00007/apps/MSDIAL/MSDIAL_ver.4.9/MsdialConsoleApp"

    } else {

      params$path.msdial = "C:/Users/Public/Documents/MSDIAL_v4.9/MsdialConsoleApp.exe"

    }

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
      params$method = toupper(strsplit(strsplit(path.mzml, "mzML/")[[1]][2], "_")[[1]][2])
    } else {
      params$method = toupper(method)
    }

    # target

    if (is.null(target)){
      if (params$method == "LIP"){
        params$target = "Lipidomics"
      } else if (params$method == "GC"){
        params$target = "GC"
      } else {
        params$target = "Metabolomics"
      }

    } else {
      params$target <- target
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

    # Path setup
    if (!dir.exists(params$path.result)) dir.create(params$path.result)

    # Generate sample info
    if (is.data.frame(sample.info.pos)){

      params$sample.info.pos <- sample.info.pos

    } else if ( !"No_data" %in% params$path.mzml.pos ){

      sample.info.pos <-
        extract_sample_info( path = params$path.mzml.pos,
                             path.meta = params$path.meta.pos,
                             meta.match.col = params$meta.match.col,
                             sample.pattern = params$sample.pattern,
                             standard.name = params$standard.name,
                             qc.sample.type = params$qc.sample.type,
                             calibration.sample.type = params$calibration.sample.type,
                             export.rds = paste0(params$path.result, "sample_info_pos"),
                             export.xlsx = paste0(params$path.result, "sample_info_pos")) %>%
        filter( !grepl("ms2p", tolower(Sample.name)))

      params$sample.info.pos <- sample.info.pos
    }

    if (is.data.frame(sample.info.neg)){

      params$sample.info.neg <- sample.info.neg

    } else if ( !"No_data" %in% params$path.mzml.neg ){

      sample.info.neg <-
        extract_sample_info( path = params$path.mzml.neg,
                             path.meta = params$path.meta.neg,
                             meta.match.col = params$meta.match.col,
                             sample.pattern = params$sample.pattern,
                             standard.name = params$standard.name,
                             qc.sample.type = params$qc.sample.type,
                             calibration.sample.type = params$calibration.sample.type,
                             export.rds = paste0(params$path.result, "sample_info_neg"),
                             export.xlsx = paste0(params$path.result, "sample_info_neg")) %>%
        filter( !grepl("ms2n", tolower(Sample.name)))

      params$sample.info.neg <- sample.info.neg

    }

    params$run.pos <- exists("sample.info.pos")
    params$run.neg <- exists("sample.info.neg")

    if (params$target == "GC") {
      params$run.neg <- FALSE
      params$path.mzml.pos = path.mzml
    }

    # Import library
    if ( grepl(".xlsx", path.lib) ){

      lib <- list()

      lib[[params$method]]$pos <- openxlsx::read.xlsx( path.lib, "POS" )
      lib[[params$method]]$neg <- openxlsx::read.xlsx( path.lib, "NEG" )

    } else {

      lib <- readRDS(path.lib)

    }

    if ( grepl(".xlsx", path.lib.is) ){

      lib.is <- list()

      lib.is[[params$method]]$pos <- openxlsx::read.xlsx( path.lib.is, "POS" )
      lib.is[[params$method]]$neg <- openxlsx::read.xlsx( path.lib.is, "NEG" )

    } else {

      lib.is <- readRDS(path.lib.is)

    }

    params$lib <- lib
    params$lib.is <- lib.is


    # MSP path
    params$msp <- readRDS(path.msp)

    # Set input parameters

    params$Extract_precursor <-
      c(
        "path.result",
        "method",
        "path.mzml.pos",
        "path.mzml.neg",
        "run.pos",
        "run.neg",
        "type.use.to.optimize",
        "sample.info.pos",
        "sample.info.neg",
        "lib",
        "extract_precursor",

        "optimize.xcms.parameters",
        "BPPARAM_set",
        "para.pos",
        "para.neg",
        "rt_tolerance",
        "mz_tolerance",
        "mz_tol_pos",
        "rt_tol_pos",
        "mz_tol_neg",
        "rt_tol_neg",
        "ri_tol",
        "ri_tol_bigger")

    params$Preprocessing_IS <-
      c(
        "path.result",
        "method",
        "path.mzml.pos",
        "path.mzml.neg",
        "run.pos",
        "run.neg",
        "lib.is",
        "path.mzmine",

        "preprocessing.is",
        "mzmine.mz.tol.pos",
        "mzmine.rt.tol.pos",
        "mzmine.mz.tol.neg",
        "mzmine.rt.tol.neg",
        "mzmine.min.peaks.in.row.pos",
        "mzmine.min.peaks.in.row.pos.percent",
        "mzmine.min.peaks.in.row.neg",
        "mzmine.min.peaks.in.row.neg.percent"
    )

    params$QC_IS <-
      c(
        "path.result",
        "method",
        "run.pos",
        "run.neg",
        "lib.is",
        "sample.info.pos",
        "sample.info.neg",

        "path.peaktable.IS.pos",
        "path.peaktable.IS.neg",
        "sample.info.pos.qc",
        "sample.info.neg.qc",
        "figure_height",
        "reference.type"
      )

    params$Preprocessing <-
      c(
        "path.result",
        "method",
        "path.mzml.pos",
        "path.mzml.neg",
        "run.pos",
        "run.neg",
        "lib.is",
        "lib",
        "path.mzmine",

        "preprocessing.is",
        "precursor.include",
        "mzmine.mz.tol.pos",
        "mzmine.rt.tol.pos",
        "mzmine.mz.tol.neg",
        "mzmine.rt.tol.neg",
        "mzmine.min.peaks.in.row.pos",
        "mzmine.min.peaks.in.row.pos.percent",
        "mzmine.min.peaks.in.row.neg",
        "mzmine.min.peaks.in.row.neg.percent"
      )

    params$Untargeted_preprocessing <-
      c(
        "path.result",
        "method",
        "sample.pattern",
        "path.mzml.pos",
        "path.mzml.neg",
        "target",
        "run.pos",
        "run.neg",
        "lib.is",
        "lib",
        "msp",
        "path.msdial",
        "sample.info.pos",
        "sample.info.neg",
        "po.sample.to.use",
        "extract_precursor",

        "optimize.xcms.parameters",
        "BPPARAM_set",
        "para.pos",
        "para.neg",
        "rt_tolerance",
        "mz_tolerance",
        "mz_tol_pos",
        "rt_tol_pos",
        "mz_tol_neg",
        "rt_tol_neg",
        "ri_tol",
        "ri_tol_bigger"
      )

    params$Clean_peaktable <-
      c(
        "path.result",
        "run.pos",
        "run.neg",
        "sample.info.pos",
        "sample.info.neg",
        "po.sample.to.use",

        "path.peaktable.pos",
        "path.peaktable.neg",
        "peaktable.sep",
        "peaktable.sample.pattern",
        "mz_col_nr",
        "rt_col_nr",
        "identity_col_nr",
        "others",
        "bl.thres",
        "rsd.po.thres",
        "mean.po.thres",
        "rt.range",
        "mean.thres",
        "filter_by_missing_feature_pct",
        "missing.impute.method"
      )

    params$Normalization_comparison <-
      c(
        "path.result",
        "run.pos",
        "run.neg",
        "po.sample.to.use",
        "sample.info.pos",
        "sample.info.neg",

        "path.peaktable.pos",
        "path.peaktable.neg",
        "peaktable.sep",
        "peaktable.sample.pattern",
        "mz_col_nr",
        "rt_col_nr",
        "identity_col_nr",
        "sample_col_nr",
        "others",
        "type.to.remove",
        "missing.sample.thres",
        "missing.feature.thres",
        "missing.impute.method.sample",
        "missing.impute.method.is",
        "outliers.sample",
        "outliers.feature",
        "outliers.is",
        "norm.method"
      )

    params$Merge_and_map_names <-
      c(
        "path.result",
        "method",
        "run.pos",
        "run.neg",
        "lib",
        "sample.info.pos",
        "sample.info.neg",
        "po.sample.to.use",

        "path.datatable.pos",
        "path.datatable.neg",
        "datatable.sep",
        "feature_col_nr"
      )

    return(params)
  }
