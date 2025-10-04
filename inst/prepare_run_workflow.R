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
prepare_workflow <- system.file("prepare_workflow.R", package = "metpipe")

system(paste0("Rscript ", prepare_workflow, 
              " --raw ", path_raw, 
              " --temp ", path_temp
              # " --workflow ", path_workflow,
              # " --mzmine ", path_mzmine,
              # " --result ", path_result,
              # " --yaml ", path_yaml
))

# Run workflow
config_file <- paste0(path_result, "/config.yml")
run_workflow <- system.file("run_workflow.R", package = "metpipe")
system(paste0("Rscript ", run_workflow, 
              " ", config_file))
