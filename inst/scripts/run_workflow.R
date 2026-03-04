##### Run workflow (QMD)

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name) {
  i <- which(args == name)
  if (length(i) == 0) return(NULL)
  args[i + 1]
}

config_file <- args[1]
start_from <- get_arg("--start-from")
run_module <- get_arg("--run-module")

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

if (is.na(config_file) || !nzchar(config_file)) stop("Missing required config file argument")
if (!file.exists(config_file)) stop("Config file does not exist: ", config_file)
config_file <- normalizePath(config_file, winslash = "/", mustWork = TRUE)
config_dir <- dirname(config_file)

is_absolute_path <- function(path) {
  if (wf_is_blank(path)) return(FALSE)
  grepl("^([A-Za-z]:|/|\\\\)", as.character(path)[1])
}

render_module <- function(file_name, output_prefix, config_file, path_workflow, path_result, path_temp) {
  module_input <- wf_resolve_module_script(path_workflow, file_name)
  wf_render_qmd_document(
    input = module_input,
    render_params = list(config = config_file),
    intermediates_dir = path_temp,
    output_file = file.path(path_result, paste0(output_prefix, "_", Sys.Date(), ".html"))
  )
}

normalize_start_from <- function(x) {
  if (wf_is_blank(x)) return(NULL)
  key <- tolower(trimws(as.character(x)[1]))
  aliases <- c(
    "preprocess" = "preprocessing_mzmine",
    "preprocessing" = "preprocessing_mzmine",
    "preprocessing_mzmine" = "preprocessing_mzmine",
    "convert" = "convert_output",
    "convert_output" = "convert_output",
    "qc" = "qc_istd",
    "qc_istd" = "qc_istd",
    "post" = "post_processing",
    "post_processing" = "post_processing",
    "eval" = "evaluation",
    "evaluation" = "evaluation",
    "extra" = "extra_processing_lip",
    "extra_processing_lip" = "extra_processing_lip"
  )
  resolved <- unname(aliases[key])
  if (is.na(resolved) || wf_is_blank(resolved)) {
    stop("Invalid --start-from value: ", x,
         ". Allowed: preprocessing_mzmine, convert_output, qc_istd, post_processing, evaluation, extra_processing_lip")
  }
  resolved
}

configurations <- wf_read_yaml_config(config_file, label = "config file")

path_result <- wf_cfg_required(configurations, "path_result")
path_workflow <- wf_cfg_required(configurations, "path_workflow")
path_temp <- configurations$path_temp

if (!is_absolute_path(path_result)) {
  path_result <- file.path(config_dir, as.character(path_result)[1])
}
path_result <- normalizePath(path_result, winslash = "/", mustWork = FALSE)
path_workflow <- normalizePath(path_workflow, winslash = "/", mustWork = FALSE)

if (!dir.exists(path_result)) dir.create(path_result, recursive = TRUE, showWarnings = FALSE)
path_result <- normalizePath(path_result, winslash = "/", mustWork = TRUE)
if (wf_is_blank(path_temp)) {
  path_temp <- tempdir()
} else {
  if (!is_absolute_path(path_temp)) {
    path_temp <- file.path(config_dir, as.character(path_temp)[1])
  }
  path_temp <- normalizePath(as.character(path_temp)[1], winslash = "/", mustWork = FALSE)
}
if (!dir.exists(path_temp)) dir.create(path_temp, recursive = TRUE, showWarnings = FALSE)
path_temp <- normalizePath(path_temp, winslash = "/", mustWork = TRUE)

wf_ensure_local_root_from_workflow(path_workflow)

module_plan <- list(
  list(key = "preprocessing_mzmine", file = "preprocessing_mzmine.qmd", output = "preprocessing_mzmine", enabled = TRUE),
  list(key = "convert_output", file = "convert_output.qmd", output = "convert_output", enabled = TRUE),
  list(key = "qc_istd", file = "qc_istd.qmd", output = "qc_report", enabled = TRUE),
  list(key = "post_processing", file = "post_processing.qmd", output = "post_processing", enabled = TRUE),
  list(key = "evaluation", file = "evaluation.qmd", output = "evaluation", enabled = TRUE),
  list(key = "extra_processing_lip", file = "extra_processing_lip.qmd", output = "extra_processing_lip", enabled = wf_as_flag(configurations$extra_processing_lip))
)

start_key <- normalize_start_from(start_from)
run_key <- normalize_start_from(run_module)
if (!wf_is_blank(start_from) && !wf_is_blank(run_module)) {
  stop("Use either --start-from or --run-module, not both")
}
plan_keys <- vapply(module_plan, function(x) x$key, character(1))
if (!is.null(run_key)) {
  run_idx <- match(run_key, plan_keys)
  if (is.na(run_idx)) {
    stop("Unable to resolve --run-module module index")
  }
  step <- module_plan[[run_idx]]
  if (!isTRUE(step$enabled)) {
    stop("Requested module is disabled by config: ", step$key)
  }
  message("Running module: ", step$key)
  render_module(step$file, step$output, config_file, path_workflow, path_result, path_temp)
  quit(save = "no", status = 0)
}

start_idx <- if (is.null(start_key)) 1 else match(start_key, plan_keys)
if (is.na(start_idx)) {
  stop("Unable to resolve --start-from module index")
}

for (step in module_plan[start_idx:length(module_plan)]) {
  if (!isTRUE(step$enabled)) next
  message("Running module: ", step$key)
  render_module(step$file, step$output, config_file, path_workflow, path_result, path_temp)
}
