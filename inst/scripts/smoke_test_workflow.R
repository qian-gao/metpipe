##### Workflow smoke test

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name) {
  i <- which(args == name)
  if (length(i) == 0) return(NULL)
  args[i + 1]
}

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

config_file <- get_arg("--config")
if (wf_is_blank(config_file)) stop("Missing required argument: --config <path/to/config.yml>")
if (!file.exists(config_file)) stop("Config file does not exist: ", config_file)

format <- "qmd"

package_mode <- tolower(trimws(ifelse(
  wf_is_blank(get_arg("--package-mode")),
  Sys.getenv("METPIPE_PACKAGE_MODE", unset = "local"),
  get_arg("--package-mode")
)))
if (!package_mode %in% c("local", "installed")) {
  stop("--package-mode must be one of: local, installed")
}
Sys.setenv(METPIPE_PACKAGE_MODE = package_mode)

local_pkg_root_arg <- get_arg("--local-pkg-root")
if (!wf_is_blank(local_pkg_root_arg)) {
  Sys.setenv(METPIPE_LOCAL_ROOT = local_pkg_root_arg)
}

config <- wf_read_yaml_config(config_file, label = "--config")
path_workflow <- wf_cfg_required(config, "path_workflow")
path_result <- wf_cfg_required(config, "path_result")
path_temp <- config$path_temp

wf_ensure_local_root_from_workflow(path_workflow)

if (wf_is_blank(path_temp)) {
  path_temp <- tempdir()
}

cat("[OK] config:", normalizePath(config_file, winslash = "/", mustWork = TRUE), "\n")
cat("[OK] path_workflow:", normalizePath(path_workflow, winslash = "/", mustWork = FALSE), "\n")
cat("[OK] path_result:", normalizePath(path_result, winslash = "/", mustWork = FALSE), "\n")
cat("[OK] path_temp:", normalizePath(path_temp, winslash = "/", mustWork = FALSE), "\n")
cat("[OK] package mode:", package_mode, "\n")

module_override <- get_arg("--module")

if (wf_is_blank(module_override)) {
  modules <- c(
    paste0("preprocessing_mzmine.", format),
    paste0("convert_output.", format),
    paste0("qc_istd.", format),
    paste0("post_processing.", format),
    paste0("evaluation.", format),
    paste0("extra_processing_lip.", format)
  )
} else {
  modules <- as.character(module_override)
}

for (module_name in modules) {
  resolved <- wf_resolve_module_script(path_workflow, module_name)
  cat("[OK] module:", module_name, "->", resolved, "\n")
}

cat("Smoke check completed successfully.\n")
