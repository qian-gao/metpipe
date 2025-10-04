##### Run workflow

# Input
config_file <- commandArgs(trailingOnly = TRUE)[1]

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