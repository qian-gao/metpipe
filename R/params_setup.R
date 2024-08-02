#' @title params_setup
#'
#' @description Set up parameters for data processing
#' 
#' Path to data files   
#' @param path.mzml Path to mzML files
#' @param standard.name If standardized sample names are used, TRUE/FALSE
#' @param path.meta.pos Path to metadata file for samples
#' @param path.meta.neg Path to metadata file for samples!!!!
#' @param sample.info.pos Path to metadata file for samples!!!
#' @param sample.info.neg Path to metadata file for samples!!!!!
#' 
#' @param meta.match.col Column name to match sample info and metadata, default is "Sample"
#' 
#' Specification sample types used for QC and calibration
#' @param qc.sample.type QC sample types, default is c( "sol", "BL", "CP", "IQ",
#'    "NIST", "CAL", "Met", "M-Mix", "SPO", "PO", "PO50", "PO12.5", "PO25", "PO100", "BPO" )
#' @param calibration.sample.type Calibration sample types, default is c( "CA00",
#'    "CA01", "CA02","CA03", "CA04", "CA05", "CA06", "CA07", "CA08", "CA09" )
#' 
#' Parameters for preprocessing
#' @param rt_tolerance Retention time tolerance for matching library
#' @param mz_tolerance m/z tolerance for matching library
#' @param po.sample.to.use Sample type used for evaluating normalization, default is "PO"
#' 
#' Path to preprocessing software
#' @param path.mzmine Path to MZmine3 software, default is 
#' @param path.msdial Path to MSDIAL ConsoleApp
#' @param BPPARAM_set Parallel processing setup
#' 
#' Other parameters: no need to change for standard workflow
#' @param method Data acqusition method, choose from c("LIP", "RP", "HILIC")
#' @param path.result Path to output folder
#' @param sample.pattern Sample pattern of data files, default is ".mzML"
#' 
#' Path to library
#' @param path.lib Path to in house library in rds/xlsx format. In xlsx format, 
#'    sheets are labelled as POS and NEG
#' @param path.lib.is Path to in house library for internal standards in rds/xlsx format.
#'    In xlsx format, sheets are labelled as POS and NEG
#' @param path.msp Path to public MS2 database file in rds
#' 
#' If the rds file for public MS2 database is not available, msp format can be provided
#' @param path.msp.pos Path to public MS2 positive mode database file in msp format
#' @param path.msp.neg Path to public MS2 negative mode database file in msp format
#'  
#' @param author Author of the analysis
#' @return A list of parameters used for data processing
#' 
#' @export

## @param type.use.to.optimize Sample type used for optimizing XCMS parameters, default is "PO"

params_setup <-
  function(
    path.mzml = NULL,
    standard.name = TRUE,
    path.meta.pos = NULL,
    path.meta.neg = NULL,
    sample.info.pos = NULL,
    sample.info.neg = NULL,
    meta.match.col = "Sample",
    qc.sample.type = c( "sol", "BL", "CP", "IQ", "NIST", "CAL", "Met", "M-Mix",
                        "SPO", "PO", "PO50", "PO12.5", "PO25", "PO100", "BPO" ),
    calibration.sample.type = c( "CA00", "CA01", "CA02","CA03", "CA04",
                                 "CA05", "CA06", "CA07", "CA08", "CA09" ),
    rt_tolerance = 0.3,
    mz_tolerance = 0.01,
    #type.use.to.optimize = "PO",
    po.sample.to.use = "PO",
    path.mzmine = NULL,
    path.msdial = NULL,
    BPPARAM_set = NULL,
    method = NULL,
    path.result = NULL,
    sample.pattern = ".mzML",
    path.lib = NULL,
    path.lib.is = NULL,
    path.msp = NULL,
    path.msp.pos = NULL,
    path.msp.neg = NULL,
    
    author = NULL

  ){

    params <- list()

    ##### Only modify if not standard workflow
    ### method
    if (is.null(method)) {
      params$method = toupper(strsplit(strsplit(path.mzml, "mzML/")[[1]][2], "_")[[1]][2])
    } else {
      params$method = toupper(method)
    }
    
    ### path.result
    if (is.null(path.result)) {
      params$path.result = gsub("mzML", "peaktable", path.mzml)
    } else {
      params$path.result = path.result
    }
    
    ### sample pattern
    params$sample.pattern = sample.pattern
    
    ### path setup
    if (!dir.exists(params$path.result)) dir.create(params$path.result)
    params$path.data <- paste0(params$path.result, "/data/")
    if (!dir.exists(params$path.data)) dir.create(params$path.data)
    
    ### import MS1 library
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
    
    
    ### Import MS2 database
    if (!is.null(path.msp)) params$msp <- readRDS(path.msp)
    if (!is.null(path.msp.pos)) params$msp[[params$target]]$pos <- path.msp.pos
    if (!is.null(path.msp.neg)) params$msp[[params$target]]$neg <- path.msp.neg
    
    ### Path to preprocessing software
    ## mzmine
    if (!is.null(path.mzmine)){
      
      params$path.mzmine = path.mzmine
      
    } else if ( file.exists( "/home/projects/ku_00007/" )){
      
      #params$path.mzmine = "/home/projects/ku_00007/apps/MZmine/MZmine-2.53-Linux/bin/start_mzmine2"
      params$path.mzmine = "/home/projects/ku_00007/apps/MZmine/MZmine-3.4.27-Linux/bin/start_mzmine3"
      
    } else {
      
      #params$path.mzmine = "C:/Users/Public/Documents/MZmine-2.53/startMZmine-Windows.bat"
      params$path.mzmine = "C:/Users/Public/Documents/MZmine-3.4.27/MZmine_console.exe"
      
    }
    
    ## msdial
    if (!is.null(path.msdial)){
      
      params$path.msdial = path.msdial
      
    } else if ( file.exists( "/home/projects/ku_00007/" )){
      
      params$path.msdial = "/home/projects/ku_00007/apps/MSDIAL/MSDIAL_ver.4.9/MsdialConsoleApp"
      
    } else {
      
      params$path.msdial = "C:/Users/Public/Documents/MSDIAL_v4.9/MsdialConsoleApp.exe"
      
    }
    
    # target for msdial setting
    if (params$method == "LIP"){
      params$target = "Lipidomics"
    } else if (params$method == "GC"){
      params$target = "GC"
    } else {
      params$target = "Metabolomics"
    }
    if (params$target == "GC") {
      params$run.neg <- FALSE
      params$path.mzml.pos = path.mzml
    }
    
    ### parallel processing
    if (is.null(BPPARAM_set)){
      
      params$BPPARAM_set = switch( Sys.info()["sysname"],
                                   Windows = BiocParallel::SnowParam(max(1, parallel::detectCores()-1), progressbar = TRUE),
                                   BiocParallel::MulticoreParam(max(1, parallel::detectCores()-1), progressbar = TRUE))
      
    } else {
      params$BPPARAM_set = BPPARAM_set
    }
    
    ### Parameters for preprocessing
    params$rt_tolerance = rt_tolerance
    params$mz_tolerance = mz_tolerance
    #params$type.use.to.optimize = type.use.to.optimize
    params$po.sample.to.use = po.sample.to.use
    
    ##### Generate sample info
    ### Path to data files
    # standard sample names
    params$standard.name = standard.name
    
    # metadata
    params$path.meta.pos = path.meta.pos
    params$path.meta.neg = path.meta.neg
    params$meta.match.col = meta.match.col
    
    # if pre-loaded sample.info
    params$sample.info.pos <- sample.info.pos
    params$sample.info.neg <- sample.info.neg
    
    # mzML files
    params$path.mzml <- path.mzml
    
    if (is.null(params$path.mzml)){
      params$path.mzml.pos <- "No_data"
      params$path.mzml.neg <- "No_data"
      
      if (!is.null(params$path.meta.pos)){
        sample.info.pos <-
          openxlsx::read.xlsx( params$path.meta.pos ) %>%
          mutate(across(where(is.character), stringr::str_trim))
        
        params$sample.info.pos <- sample.info.pos
      }
      
      if (!is.null(params$path.meta.neg)){
        sample.info.neg <-
          openxlsx::read.xlsx( params$path.meta.neg ) %>%
          mutate(across(where(is.character), stringr::str_trim))
        
        params$sample.info.neg <- sample.info.neg
      }
      
    } else {
      params$path.mzml.pos <- paste0(path.mzml, "/", "pos/")
      if (!dir.exists(params$path.mzml.pos)) {
        params$path.mzml.pos <- "No_data"
      }
      
      params$path.mzml.neg <- paste0(path.mzml, "/", "neg/")
      if (!dir.exists(params$path.mzml.neg)) {
        params$path.mzml.neg <- "No_data"
      }
    }
    
    # Specification sample types used for QC and calibration
    params$qc.sample.type = qc.sample.type
    params$calibration.sample.type = calibration.sample.type
    
    ### Generate sample info
    if ( !"No_data" %in% params$path.mzml.pos ){
      
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
    
    if ( !"No_data" %in% params$path.mzml.neg ){
      
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

    return(params)
  }
