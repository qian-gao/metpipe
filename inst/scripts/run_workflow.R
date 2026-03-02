##### Run workflow

# Input
config_file <- commandArgs(trailingOnly = TRUE)[1]

if (is.na(config_file) || !nzchar(config_file)) {
  stop("Missing required config file argument")
}

if (!file.exists(config_file)) {
  stop("Config file does not exist: ", config_file)
}

is_blank <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(trimws(as.character(x[1])))
}

cfg_required <- function(cfg, name) {
  val <- cfg[[name]]
  if (is_blank(val)) stop("Missing required config field: ", name)
  as.character(val)[1]
}

resolve_rmd <- function(path_workflow, file_name) {
  if (is_blank(path_workflow)) {
    stop("path_workflow is missing or empty")
  }

  package_mode <- tolower(trimws(Sys.getenv("METPIPE_PACKAGE_MODE", unset = "local")))
  if (!package_mode %in% c("local", "installed")) {
    package_mode <- "local"
  }

  local_pkg_root <- Sys.getenv("METPIPE_LOCAL_ROOT", unset = "")
  local_pkg_rmd <- if (nzchar(local_pkg_root)) file.path(local_pkg_root, "inst", "scripts", file_name) else ""

  pkg_rmd <- system.file("scripts", file_name, package = "metpipe")
  local_rmd <- file.path(path_workflow, "scripts", file_name)

  if (identical(package_mode, "local")) {
    if (nzchar(local_pkg_rmd) && file.exists(local_pkg_rmd)) return(normalizePath(local_pkg_rmd, winslash = "/", mustWork = TRUE))
    if (file.exists(local_rmd)) return(normalizePath(local_rmd, winslash = "/", mustWork = TRUE))
    if (nzchar(pkg_rmd) && file.exists(pkg_rmd)) return(normalizePath(pkg_rmd, winslash = "/", mustWork = TRUE))
  } else {
    if (nzchar(pkg_rmd) && file.exists(pkg_rmd)) return(normalizePath(pkg_rmd, winslash = "/", mustWork = TRUE))
    if (file.exists(local_rmd)) return(normalizePath(local_rmd, winslash = "/", mustWork = TRUE))
    if (nzchar(local_pkg_rmd) && file.exists(local_pkg_rmd)) return(normalizePath(local_pkg_rmd, winslash = "/", mustWork = TRUE))
  }

  stop("Cannot find workflow module: ", file_name)
}

render_module <- function(file_name, output_prefix, config_file, path_workflow, path_result, path_temp) {
  module_input <- resolve_rmd(path_workflow, file_name)

  rmarkdown::render(
    input = module_input,
    params = list(config = config_file),
    intermediates_dir = path_temp,
    output_file = file.path(path_result, paste0(output_prefix, "_", Sys.Date(), ".html"))
  )
}

# Set path
configurations <- yaml::read_yaml(config_file)
if (is.null(configurations) || !is.list(configurations)) {
  stop("Invalid YAML config: expected a named list")
}

path_result <- cfg_required(configurations, "path_result")
path_workflow <- cfg_required(configurations, "path_workflow")
path_temp <- configurations$path_temp

path_result <- normalizePath(path_result, winslash = "/", mustWork = FALSE)
path_workflow <- normalizePath(path_workflow, winslash = "/", mustWork = FALSE)

if (!dir.exists(path_result)) dir.create(path_result, recursive = TRUE, showWarnings = FALSE)

if (is_blank(path_temp)) {
  path_temp <- tempdir()
} else {
  path_temp <- normalizePath(as.character(path_temp)[1], winslash = "/", mustWork = FALSE)
}
if (!dir.exists(path_temp)) dir.create(path_temp, recursive = TRUE, showWarnings = FALSE)

if (!nzchar(Sys.getenv("METPIPE_LOCAL_ROOT", unset = ""))) {
  local_root <- normalizePath(file.path(path_workflow, ".."), winslash = "/", mustWork = FALSE)
  if (dir.exists(local_root) && file.exists(file.path(local_root, "DESCRIPTION"))) {
    Sys.setenv(METPIPE_LOCAL_ROOT = local_root)
  }
}

# Processing modules
# 1: Preprocessing
render_module("preprocessing_mzmine.Rmd", "preprocessing_mzmine", config_file, path_workflow, path_result, path_temp)


# 2: Output conversion
render_module("convert_output.Rmd", "convert_output", config_file, path_workflow, path_result, path_temp)


# 3: QC
render_module("qc_istd.Rmd", "qc_report", config_file, path_workflow, path_result, path_temp)


# 4: Post-processing
render_module("post_processing.Rmd", "post_processing", config_file, path_workflow, path_result, path_temp)


# 5: Evaluation
render_module("evaluation.Rmd", "evaluation", config_file, path_workflow, path_result, path_temp)

# 6: Extra lipidomics processing
if (configurations$extra_processing_lip == "Y"){
  render_module("extra_processing_lip.Rmd", "extra_processing_lip", config_file, path_workflow, path_result, path_temp)
}
