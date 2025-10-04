##### Prepare workflow

# Input
args <- commandArgs(trailingOnly = TRUE)
path_raw <- args[1]
path_temp <- args[2]

defaults <- list(
  path_result = gsub("raw", "peaktable", path_raw),
  path_workflow = "/wd",
  path_mzmine = "/wd/mzmine_linux/bin/mzmine"
)

path_workflow <- ifelse(length(args) >= 3, args[3], defaults$path_workflow)
path_mzmine <- ifelse(length(args) >= 4, args[4], defaults$path_mzmine)
path_result <- ifelse(length(args) >= 5, args[5], defaults$path_result)

# Configuration
if (!dir.exists(path_result)) dir.create(path_result)

rmarkdown::render(
  input = system.file("scripts", "prep_yaml.Rmd", package = "metpipe"),
  #input = paste0(path_workflow, "/scripts/prep_yaml.Rmd"),
  params = list(path_raw = path_raw,
                path_result = path_result,
                path_workflow = path_workflow,
                path_mzmine = path_mzmine,
                path_temp = path_temp),
  intermediates_dir = path_temp,
  output_file = paste0(path_result, "/generate_yml_", Sys.Date(), ".html")
)

