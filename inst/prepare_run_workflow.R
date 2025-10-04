##### Prepare and run workflow

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

# Prepare workflow
prepare_workflow <- system.file("prepare_workflow.R", package = "metpipe")
system(paste0("Rscript ", prepare_workflow, 
              " ", path_raw, 
              " ", path_temp,
              " ", path_workflow,
              " ", path_mzmine,
              " ", path_result))

# Run workflow
config_file <- paste0(path_result, "/config.yml")
run_workflow <- system.file("run_workflow.R", package = "metpipe")
system(paste0("Rscript ", run_workflow, 
              " ", config_file))
