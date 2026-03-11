wf_is_blank <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(trimws(as.character(x[1])))
}

wf_as_flag <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  if (is.numeric(x)) return(!is.na(x[1]) && x[1] != 0)
  if (is.character(x)) return(tolower(trimws(x[1])) %in% c("true", "t", "1", "y", "yes"))
  FALSE
}

wf_cfg_required <- function(cfg, name) {
  val <- cfg[[name]]
  if (wf_is_blank(val)) stop("Missing required config field: ", name)
  val
}

wf_read_yaml_config <- function(config_path, label = "config") {
  if (wf_is_blank(config_path)) stop(label, " is required")
  config_path <- as.character(config_path)[1]
  if (!file.exists(config_path)) stop("Config file does not exist: ", config_path)

  cfg <- yaml::read_yaml(config_path)
  if (is.null(cfg) || !is.list(cfg)) {
    stop("Invalid YAML config: expected a named list")
  }
  cfg
}

wf_require_path <- function(path, name) {
  if (wf_is_blank(path)) stop(name, " is required")
  path <- as.character(path)[1]
  if (!dir.exists(path)) stop(name, " does not exist: ", path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

wf_join_path <- function(base, rel) {
  if (wf_is_blank(rel) || is.na(rel)) return(NA_character_)
  normalizePath(file.path(base, as.character(rel)[1]), winslash = "/", mustWork = FALSE)
}

wf_as_meta_df <- function(x) {
  if (is.null(x)) return(NULL)
  out <- data.frame(x, check.names = FALSE)
  if (nrow(out) == 0) return(NULL)
  out
}

wf_ensure_parent_dirs <- function(paths) {
  for (p in paths) {
    if (wf_is_blank(p)) next
    out_dir <- dirname(as.character(p)[1])
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

wf_load_metpipe_runtime <- function() {
  package_mode <- tolower(trimws(Sys.getenv("METPIPE_PACKAGE_MODE", unset = "local")))
  if (!package_mode %in% c("local", "installed")) package_mode <- "local"

  if (identical(package_mode, "installed")) {
    if (!requireNamespace("metpipe", quietly = TRUE)) {
      stop("METPIPE_PACKAGE_MODE='installed' but package 'metpipe' is not installed")
    }
    library(metpipe)
    return("installed")
  }

  candidates <- unique(Filter(nzchar, c(
    Sys.getenv("METPIPE_LOCAL_ROOT", ""),
    "H:/Documents/CBMR_workflow/packages/metpipe",
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
  )))

  is_pkg_root <- function(path) {
    dir.exists(path) &&
      file.exists(file.path(path, "DESCRIPTION")) &&
      dir.exists(file.path(path, "R"))
  }

  roots <- candidates[vapply(candidates, is_pkg_root, logical(1))]
  if (length(roots) == 0) {
    stop("Cannot locate local metpipe package root. Set METPIPE_LOCAL_ROOT to your metpipe folder.")
  }

  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is required for local source loading. Install with install.packages('pkgload').")
  }

  Sys.setenv(METPIPE_LOCAL_ROOT = roots[[1]])
  pkgload::load_all(path = roots[[1]], export_all = FALSE, quiet = TRUE)
  "local"
}

wf_package_mode <- function(default = "local") {
  mode <- tolower(trimws(Sys.getenv("METPIPE_PACKAGE_MODE", unset = default)))
  if (!mode %in% c("local", "installed")) mode <- default
  mode
}

wf_ensure_local_root_from_workflow <- function(path_workflow) {
  if (!wf_is_blank(Sys.getenv("METPIPE_LOCAL_ROOT", unset = ""))) {
    return(Sys.getenv("METPIPE_LOCAL_ROOT"))
  }

  if (wf_is_blank(path_workflow)) return("")

  local_root <- normalizePath(file.path(as.character(path_workflow)[1], ".."), winslash = "/", mustWork = FALSE)
  if (dir.exists(local_root) && file.exists(file.path(local_root, "DESCRIPTION"))) {
    Sys.setenv(METPIPE_LOCAL_ROOT = local_root)
    return(local_root)
  }
  ""
}

wf_resolve_module_script <- function(path_workflow, file_name) {
  if (wf_is_blank(path_workflow)) stop("path_workflow is missing or empty")
  if (wf_is_blank(file_name)) stop("file_name is required")

  package_mode <- wf_package_mode(default = "local")
  local_pkg_root <- Sys.getenv("METPIPE_LOCAL_ROOT", unset = "")

  local_pkg_file <- if (nzchar(local_pkg_root)) file.path(local_pkg_root, "inst", "scripts", file_name) else ""
  pkg_file <- system.file("scripts", file_name, package = "metpipe")
  local_file_in_scripts <- file.path(as.character(path_workflow)[1], "scripts", file_name)
  local_file_direct <- file.path(as.character(path_workflow)[1], file_name)

  if (identical(package_mode, "local")) {
    if (nzchar(local_pkg_file) && file.exists(local_pkg_file)) return(normalizePath(local_pkg_file, winslash = "/", mustWork = TRUE))
    if (file.exists(local_file_in_scripts)) return(normalizePath(local_file_in_scripts, winslash = "/", mustWork = TRUE))
    if (file.exists(local_file_direct)) return(normalizePath(local_file_direct, winslash = "/", mustWork = TRUE))
    if (nzchar(pkg_file) && file.exists(pkg_file)) return(normalizePath(pkg_file, winslash = "/", mustWork = TRUE))
  } else {
    if (nzchar(pkg_file) && file.exists(pkg_file)) return(normalizePath(pkg_file, winslash = "/", mustWork = TRUE))
    if (file.exists(local_file_in_scripts)) return(normalizePath(local_file_in_scripts, winslash = "/", mustWork = TRUE))
    if (file.exists(local_file_direct)) return(normalizePath(local_file_direct, winslash = "/", mustWork = TRUE))
    if (nzchar(local_pkg_file) && file.exists(local_pkg_file)) return(normalizePath(local_pkg_file, winslash = "/", mustWork = TRUE))
  }

  stop("Cannot find workflow module: ", file_name)
}

wf_render_qmd_document <- function(input, render_params, intermediates_dir, output_file) {
  output_file <- normalizePath(as.character(output_file)[1], winslash = "/", mustWork = FALSE)
  intermediates_dir <- normalizePath(as.character(intermediates_dir)[1], winslash = "/", mustWork = FALSE)

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(intermediates_dir)) dir.create(intermediates_dir, recursive = TRUE, showWarnings = FALSE)

  quarto_bin <- Sys.which("quarto")
  if (!nzchar(quarto_bin)) {
    stop("Quarto CLI was not found on PATH. Install Quarto or add 'quarto' to PATH.")
  }

  rscript_candidate <- file.path(R.home("bin"), ifelse(.Platform$OS.type == "windows", "Rscript.exe", "Rscript"))
  if (file.exists(rscript_candidate)) {
    rscript_bin <- normalizePath(rscript_candidate, winslash = "/", mustWork = TRUE)
  } else {
    rscript_bin <- Sys.which("Rscript")
    if (nzchar(rscript_bin) && file.exists(rscript_bin)) {
      file_name <- basename(rscript_bin)
      if (.Platform$OS.type == "windows" && tolower(file_name) == "rscript") {
        rscript_bin <- ""
      }
    }
  }
  if (!nzchar(rscript_bin) || !file.exists(rscript_bin)) {
    stop("Unable to locate a valid Rscript executable for Quarto")
  }

  exec_dir <- intermediates_dir
  input_abs <- normalizePath(input, winslash = "/", mustWork = TRUE)
  input_base <- basename(input_abs)
  temp_qmd <- file.path(intermediates_dir, input_base)
  file.copy(input_abs, temp_qmd, overwrite = TRUE)

  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    if (file.exists(temp_qmd)) file.remove(temp_qmd)
  }, add = TRUE)
  setwd(exec_dir)

  params_file <- tempfile(pattern = "qmd-params-", tmpdir = intermediates_dir, fileext = ".yml")
  yaml::write_yaml(render_params, params_file)

  old_quarto_r <- Sys.getenv("QUARTO_R", unset = "")
  on.exit(if (nzchar(old_quarto_r)) Sys.setenv(QUARTO_R = old_quarto_r) else Sys.unsetenv("QUARTO_R"), add = TRUE)
  Sys.setenv(QUARTO_R = rscript_bin)

  args <- c(
    "render",
    temp_qmd,
    "--no-clean",
    "--execute-params", normalizePath(params_file, winslash = "/", mustWork = TRUE),
    "--execute-dir", exec_dir,
    "--output", basename(output_file)
  )

  status <- system2(quarto_bin, args = args, stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop("quarto render failed with exit code: ", status)
  }

  produced_file <- file.path(exec_dir, basename(output_file))
  if (!file.exists(produced_file)) {
    default_name <- paste0(tools::file_path_sans_ext(input_base), ".html")
    produced_file <- file.path(exec_dir, default_name)
  }

  if (file.exists(produced_file) && normalizePath(produced_file, winslash = "/", mustWork = TRUE) != normalizePath(output_file, winslash = "/", mustWork = FALSE)) {
    ok <- file.copy(produced_file, output_file, overwrite = TRUE)
    if (!ok) {
      stop("Rendered file was produced but could not be copied to expected output path: ", output_file)
    }
  }

  if (!file.exists(output_file)) {
    stop("Quarto render completed but expected output was not found: ", output_file)
  }
}
