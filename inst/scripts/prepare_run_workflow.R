##### Prepare and run workflow (QMD)

args <- commandArgs(trailingOnly = TRUE)

script_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", script_args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

helper_candidates <- c(
  file.path(script_dir, "helpers_workflow.R"),
  file.path(getwd(), "helpers_workflow.R"),
  file.path(getwd(), "inst", "scripts", "helpers_workflow.R"),
  system.file("scripts", "helpers_workflow.R", package = "metpipe")
)
helper_candidates <- helper_candidates[file.exists(helper_candidates)]
if (length(helper_candidates) == 0) {
  stop("Cannot locate shared helper script helpers_workflow.R")
}
source(helper_candidates[[1]], local = TRUE)

get_arg <- function(name) {
  i <- which(args == name)
  if (length(i) == 0) return(NULL)
  args[i + 1]
}

is_blank <- wf_is_blank

package_mode <- tolower(trimws(ifelse(
  is_blank(get_arg("--package-mode")),
  ifelse(
    is_blank(Sys.getenv("METPIPE_PACKAGE_MODE", unset = "")),
    ifelse(dir.exists("/wd"), "installed", "local"),
    Sys.getenv("METPIPE_PACKAGE_MODE", unset = "")
  ),
  get_arg("--package-mode")
)))

if (!package_mode %in% c("local", "installed")) {
  stop("--package-mode must be one of: local, installed")
}

Sys.setenv(METPIPE_PACKAGE_MODE = package_mode)

local_pkg_root <- ifelse(
  is_blank(get_arg("--local-pkg-root")),
  "H:/Documents/CBMR_workflow/packages/metpipe",
  get_arg("--local-pkg-root")
)

if (identical(package_mode, "local")) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is required for local source loading. Install with install.packages('pkgload').")
  }
  if (!dir.exists(local_pkg_root)) {
    stop("Local metpipe package root not found: ", local_pkg_root)
  }
  Sys.setenv(METPIPE_LOCAL_ROOT = local_pkg_root)
  pkgload::load_all(path = local_pkg_root, export_all = FALSE, quiet = TRUE)
} else {
  Sys.unsetenv("METPIPE_LOCAL_ROOT")
  if (!requireNamespace("metpipe", quietly = TRUE)) {
    stop("METPIPE_PACKAGE_MODE='installed' but package 'metpipe' is not installed")
  }
}

pick_first <- function(...) {
  vals <- list(...)
  for (v in vals) {
    if (!is.null(v) && nzchar(v)) return(v)
  }
  NULL
}

resolve_yaml <- function(path_workflow, path_yaml = NULL) {
  if (!is.null(path_yaml)) return(path_yaml)
  wf_resolve_module_script(path_workflow, "prep_yaml.qmd")
}

normalize_start_from <- function(x) {
  if (is_blank(x)) return(NULL)
  key <- tolower(trimws(as.character(x)[1]))
  aliases <- c(
    "prep"                  = "prep_yaml",
    "prep_yaml"             = "prep_yaml",
    "preprocess"            = "preprocessing_mzmine",
    "preprocessing"         = "preprocessing_mzmine",
    "preprocessing_mzmine"  = "preprocessing_mzmine",
    "convert"               = "convert_output",
    "convert_output"        = "convert_output",
    "qc"                    = "qc_istd",
    "qc_istd"               = "qc_istd",
    "post"                  = "post_processing",
    "post_processing"       = "post_processing",
    "eval"                  = "evaluation",
    "evaluation"            = "evaluation",
    "extra"                 = "extra_processing_lip",
    "extra_processing_lip"  = "extra_processing_lip"
  )
  resolved <- unname(aliases[key])
  if (is.na(resolved) || is_blank(resolved)) {
    stop("Invalid --start-from value: ", x,
         ". Allowed: prep_yaml, preprocessing_mzmine, convert_output, qc_istd, post_processing, evaluation, extra_processing_lip")
  }
  resolved
}

path_raw    <- get_arg("--raw")
start_from  <- get_arg("--start-from")
run_module  <- get_arg("--run-module")
start_key   <- normalize_start_from(start_from)
run_key     <- normalize_start_from(run_module)
if (!is_blank(start_from) && !is_blank(run_module)) {
  stop("Use either --start-from or --run-module, not both")
}
target_key      <- if (!is.null(run_key)) run_key else start_key
requires_prepare <- is.null(target_key) || identical(target_key, "prep_yaml")

default_workflow <- if (dir.exists("/wd")) "/wd" else getwd()
default_result   <- if (!is.null(path_raw)) gsub("raw|mzML", "peaktable", path_raw) else NULL
default_temp     <- if (dir.exists("/home/Temp")) "/home/Temp" else tempdir()

path_workflow <- pick_first(get_arg("--workflow"), default_workflow)
path_result   <- pick_first(get_arg("--result"),   default_result)
path_yaml     <- get_arg("--yaml")

default_mzmine <- file.path(path_workflow, "mzmine_linux", "bin", "mzmine")
if (!file.exists(default_mzmine)) default_mzmine <- "/wd/mzmine_linux/bin/mzmine"

path_mzmine     <- pick_first(get_arg("--mzmine"), default_mzmine)
path_temp       <- pick_first(get_arg("--temp"), default_temp)
qc              <- pick_first(get_arg("--qc"), "BL,NIST,PO,sol,CP,IQ,BPL,MMix,PB,SP0625,SP1125,SP25,SP5,SP10,SP,SP40,POJ,POK")
author          <- pick_first(get_arg("--author"), "CBMR Metabolomics Platform")
config_override <- get_arg("--config")
software        <- get_arg("--software")
if (is.null(software) || is_blank(software)) {
  software <- "mzmine"
} else {
  software <- tolower(software)
}
if (!software %in% c("mzmine", "msdialui")) {
  stop("--software must be one of: mzmine, msdialui")
}

run_workflow <- wf_resolve_module_script(path_workflow, "run_workflow.R")

if (!is_blank(config_override)) {
  config_file <- config_override
  if (!file.exists(config_file)) {
    stop("--config file does not exist: ", config_file)
  }
} else {
  if (is_blank(path_result)) {
    stop("`--result` is required when --config is not provided")
  }

  if (requires_prepare) {
    if (is_blank(path_raw)) {
      stop("Missing required argument: --raw <path> when running from preprocessing")
    }
    path_yaml     <- resolve_yaml(path_workflow = path_workflow, path_yaml = path_yaml)
    path_result   <- normalizePath(path_result,   winslash = "/", mustWork = FALSE)
    path_yaml     <- normalizePath(path_yaml,     winslash = "/", mustWork = FALSE)
    path_temp     <- normalizePath(path_temp,     winslash = "/", mustWork = FALSE)
    path_workflow <- normalizePath(path_workflow, winslash = "/", mustWork = FALSE)

    if (!dir.exists(path_result)) dir.create(path_result, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path_temp))   dir.create(path_temp,   recursive = TRUE, showWarnings = FALSE)

    qc_types <- trimws(unlist(strsplit(qc, ",")))

    wf_render_qmd_document(
      input = path_yaml,
      render_params = list(
        path_raw      = path_raw,
        path_result   = path_result,
        path_workflow = path_workflow,
        path_mzmine   = path_mzmine,
        path_temp     = path_temp,
        author        = author,
        qc_types      = qc_types,
        software      = software
      ),
      intermediates_dir = path_temp,
      output_file = file.path(path_result, paste0("generate_yml_", Sys.Date(), ".html"))
    )
  } else {
    config_candidate <- file.path(path_result, "config.yml")
    if (!file.exists(config_candidate)) {
      stop("Skipping prepare because target module is not prep_yaml, but config was not found: ",
           config_candidate,
           ". Provide --config <path/to/config.yml> or run with --start-from prep_yaml.")
    }
  }

  config_file <- file.path(path_result, "config.yml")
}

if (identical(target_key, "prep_yaml")) {
  message("Target module is prep_yaml; skipping run_workflow.R")
  quit(save = "no", status = 0)
}

run_args <- c(shQuote(run_workflow), shQuote(config_file))
if (!is_blank(start_from) && normalize_start_from(start_from) != "prep_yaml") {
  run_args <- c(run_args, "--start-from", shQuote(start_from))
}
if (!is_blank(run_module) && normalize_start_from(run_module) != "prep_yaml") {
  run_args <- c(run_args, "--run-module", shQuote(run_module))
}

status_run <- system2("Rscript", args = run_args)

if (!identical(status_run, 0L)) {
  stop("run_workflow.R failed with exit code: ", status_run)
}
