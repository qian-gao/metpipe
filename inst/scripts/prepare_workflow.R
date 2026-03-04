##### Prepare workflow (QMD)

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

path_raw <- get_arg("--raw")

defaults <- list(
  path_result = gsub("raw|mzML", "peaktable", path_raw),
  path_workflow = "/wd",
  path_mzmine = "/wd/mzmine_linux/bin/mzmine",
  path_temp = "/home/Temp"
)
defaults$path_yaml <- paste0(defaults$path_workflow, "/scripts/prep_yaml.qmd")
defaults$qc <- "BL,NIST,PO,sol,CP,IQ,BPL,MMix,PB,SP0625,SP1125,SP25,SP5,SP10,SP,SP40,POJ,POK"

path_workflow <- dplyr::coalesce(get_arg("--workflow"), defaults$path_workflow)
path_mzmine <- dplyr::coalesce(get_arg("--mzmine"), defaults$path_mzmine)
path_result <- dplyr::coalesce(get_arg("--result"), defaults$path_result)
path_yaml <- dplyr::coalesce(get_arg("--yaml"), defaults$path_yaml)
path_temp <- dplyr::coalesce(get_arg("--temp"), defaults$path_temp)
qc <- dplyr::coalesce(get_arg("--qc"), defaults$qc)
qc_types <- unlist(strsplit(qc, ","))
author <- dplyr::coalesce(get_arg("--author"), "CBMR Metabolomics Platform")

package_mode <- tolower(trimws(dplyr::coalesce(get_arg("--package-mode"), Sys.getenv("METPIPE_PACKAGE_MODE", unset = "local"))))
if (!package_mode %in% c("local", "installed")) {
  stop("--package-mode must be one of: local, installed")
}
Sys.setenv(METPIPE_PACKAGE_MODE = package_mode)

local_pkg_root_arg <- get_arg("--local-pkg-root")
if (!is.null(local_pkg_root_arg) && nzchar(local_pkg_root_arg)) {
  Sys.setenv(METPIPE_LOCAL_ROOT = local_pkg_root_arg)
}

if (!nzchar(Sys.getenv("METPIPE_LOCAL_ROOT", unset = ""))) {
  wf_ensure_local_root_from_workflow(path_workflow)
}

path_workflow <- normalizePath(path_workflow, winslash = "/", mustWork = FALSE)
path_result <- normalizePath(path_result, winslash = "/", mustWork = FALSE)
path_yaml <- normalizePath(path_yaml, winslash = "/", mustWork = FALSE)
path_temp <- normalizePath(path_temp, winslash = "/", mustWork = FALSE)

if (!dir.exists(path_result)) dir.create(path_result, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(path_temp)) dir.create(path_temp, recursive = TRUE, showWarnings = FALSE)

wf_render_qmd_document(
  input = path_yaml,
  render_params = list(
    path_raw = path_raw,
    path_result = path_result,
    path_workflow = path_workflow,
    path_mzmine = path_mzmine,
    path_temp = path_temp,
    author = author,
    qc_types = qc_types
  ),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/generate_yml_", Sys.Date(), ".html")
)
