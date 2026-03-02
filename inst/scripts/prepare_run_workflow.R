##### Prepare and run workflow

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name) {
  i <- which(args == name)
  if (length(i) == 0) return(NULL)
  return(args[i + 1])
}

is_blank <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(trimws(as.character(x[1])))
}

package_mode <- tolower(trimws(ifelse(
  is_blank(get_arg("--package-mode")),
  Sys.getenv("METPIPE_PACKAGE_MODE", unset = "local"),
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

resolve_workflow_script <- function(path_workflow, script_name, package_mode) {
  local_source_script <- file.path(Sys.getenv("METPIPE_LOCAL_ROOT", unset = ""), "inst", "scripts", script_name)
  local_script <- file.path(path_workflow, script_name)
  pkg_script <- system.file("scripts", script_name, package = "metpipe")

  if (identical(package_mode, "local")) {
    if (nzchar(Sys.getenv("METPIPE_LOCAL_ROOT", unset = "")) && file.exists(local_source_script)) return(local_source_script)
    if (file.exists(local_script)) return(local_script)
    if (nzchar(pkg_script) && file.exists(pkg_script)) return(pkg_script)
  } else {
    if (nzchar(pkg_script) && file.exists(pkg_script)) return(pkg_script)
    if (file.exists(local_script)) return(local_script)
    if (nzchar(Sys.getenv("METPIPE_LOCAL_ROOT", unset = "")) && file.exists(local_source_script)) return(local_source_script)
  }

  stop("Cannot find script: ", script_name)
}

resolve_yaml <- function(path_workflow, path_yaml = NULL, package_mode = "local") {
  if (!is.null(path_yaml)) return(path_yaml)

  local_source_yaml <- file.path(Sys.getenv("METPIPE_LOCAL_ROOT", unset = ""), "inst", "scripts", "prep_yaml.Rmd")
  pkg_yaml <- system.file("scripts", "prep_yaml.Rmd", package = "metpipe")
  local_yaml <- file.path(path_workflow, "scripts", "prep_yaml.Rmd")

  if (identical(package_mode, "local")) {
    if (nzchar(Sys.getenv("METPIPE_LOCAL_ROOT", unset = "")) && file.exists(local_source_yaml)) return(local_source_yaml)
    if (file.exists(local_yaml)) return(local_yaml)
    if (nzchar(pkg_yaml) && file.exists(pkg_yaml)) return(pkg_yaml)
  } else {
    if (nzchar(pkg_yaml) && file.exists(pkg_yaml)) return(pkg_yaml)
    if (file.exists(local_yaml)) return(local_yaml)
    if (nzchar(Sys.getenv("METPIPE_LOCAL_ROOT", unset = "")) && file.exists(local_source_yaml)) return(local_source_yaml)
  }

  stop("Cannot find prep_yaml.Rmd in workflow folder or installed package")
}

path_raw <- get_arg("--raw")

if (is.null(path_raw)) {
  stop("Missing required argument: --raw <path>")
}

default_workflow <- if (dir.exists("/wd")) "/wd" else getwd()
default_result <- gsub("raw|mzML", "peaktable", path_raw)
default_temp <- if (dir.exists("/home/Temp")) "/home/Temp" else tempdir()

path_workflow <- pick_first(get_arg("--workflow"), default_workflow)
path_result <- pick_first(get_arg("--result"), default_result)
path_yaml <- resolve_yaml(path_workflow = path_workflow, path_yaml = get_arg("--yaml"), package_mode = package_mode)

default_mzmine <- file.path(path_workflow, "mzmine_linux", "bin", "mzmine")
if (!file.exists(default_mzmine)) {
  default_mzmine <- "/wd/mzmine_linux/bin/mzmine"
}

path_mzmine <- pick_first(get_arg("--mzmine"), default_mzmine)
path_temp <- pick_first(get_arg("--temp"), default_temp)
qc <- pick_first(get_arg("--qc"), "BL,NIST,PO,sol,CP,IQ,BPL,MMix,PB,SP0625,SP1125,SP25,SP5,SP10,SP,SP40,POJ,POK")

prepare_workflow <- resolve_workflow_script(path_workflow, "prepare_workflow.R", package_mode = package_mode)
run_workflow <- resolve_workflow_script(path_workflow, "run_workflow.R", package_mode = package_mode)

# Prepare workflow
status_prepare <- system2(
  "Rscript",
  args = c(
    shQuote(prepare_workflow),
    "--raw", shQuote(path_raw),
    "--temp", shQuote(path_temp),
    "--workflow", shQuote(path_workflow),
    "--mzmine", shQuote(path_mzmine),
    "--result", shQuote(path_result),
    "--yaml", shQuote(path_yaml),
    "--qc", shQuote(qc),
    "--package-mode", shQuote(package_mode),
    "--local-pkg-root", shQuote(local_pkg_root)
  )
)

if (!identical(status_prepare, 0L)) {
  stop("prepare_workflow.R failed with exit code: ", status_prepare)
}

# Run workflow
config_file <- paste0(path_result, "/config.yml")
status_run <- system2(
  "Rscript",
  args = c(shQuote(run_workflow), shQuote(config_file))
)

if (!identical(status_run, 0L)) {
  stop("run_workflow.R failed with exit code: ", status_run)
}
