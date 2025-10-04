##### Prepare and run workflow

# Input
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name) {
  i <- which(args == name)
  if (length(i) == 0) return(NULL)
  return(args[i + 1])
}

path_raw <- get_arg("--raw")
path_temp <- get_arg("--temp")

defaults <- list(
  path_result = gsub("raw", "peaktable", path_raw),
  path_workflow = "/wd",
  path_mzmine = "/wd/mzmine_linux/bin/mzmine",
  path_yaml = system.file("scripts", "prep_yaml.Rmd", package = "metpipe")
)

path_workflow <- dplyr::coalesce(get_arg("--workflow"), defaults$path_workflow)
path_mzmine <- dplyr::coalesce(get_arg("--mzmine"), defaults$path_mzmine)
path_result <- dplyr::coalesce(get_arg("--result"), defaults$path_result)
path_yaml <- dplyr::coalesce(get_arg("--yaml"), defaults$path_yaml)

# Prepare workflow
# Configuration
if (!dir.exists(path_result)) dir.create(path_result)

rmarkdown::render(
  input = path_yaml,
  #input = paste0(path_workflow, "/scripts/prep_yaml.Rmd"),
  params = list(path_raw = path_raw,
                path_result = path_result,
                path_workflow = path_workflow,
                path_mzmine = path_mzmine,
                path_temp = path_temp),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/generate_yml_", Sys.Date(), ".html")
)


# Run workflow
config_file <- paste0(path_result, "/config.yml")

# Set path
configurations <- yaml::read_yaml(config_file)
path_result <- configurations$path_result
path_workflow <- configurations$path_workflow
path_temp <- configurations$path_temp

# Processing modules
# 1: Preprocessing
rmarkdown::render(
  input = system.file("scripts", "preprocessing_mzmine.Rmd", package = "metpipe"),
  #input = paste0(path_workflow, "/scripts/preprocessing_mzmine.Rmd"),
  params = list(config = config_file),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/preprocessing_mzmine_", Sys.Date(), ".html")
)


# 2: Output conversion
rmarkdown::render(
  input = system.file("scripts", "convert_output.Rmd", package = "metpipe"),
  #input = paste0(path_workflow, "/scripts/convert_output.Rmd"),
  params = list(config = config_file),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/convert_output_", Sys.Date(), ".html")
)


# 3: QC
rmarkdown::render(
  input = system.file("scripts", "qc_istd.Rmd", package = "metpipe"),
  #input = paste0(path_workflow, "/scripts/qc_istd.Rmd"),
  params = list(config = config_file),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/qc_report_", Sys.Date(), ".html")
)


# 4: Post-processing
rmarkdown::render(
  input = system.file("scripts", "post_processing.Rmd", package = "metpipe"),
  #input = paste0(path_workflow, "/scripts/post_processing.Rmd"),
  params = list(config = config_file),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/post_processing_", Sys.Date(), ".html")
)


# 5: Evaluation
rmarkdown::render(
  input = system.file("scripts", "evaluation.Rmd", package = "metpipe"),
  #input = paste0(path_workflow, "/scripts/evaluation.Rmd"),
  params = list(config = config_file),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/evaluation_", Sys.Date(), ".html")
)