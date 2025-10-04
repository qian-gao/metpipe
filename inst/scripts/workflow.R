
# Set working directory
setwd("/maps/projects/metabolomics/people/zfj107")

# Global input
raw_dir <- "/maps/projects/metabolomics/data/MP000_demo/raw/PL_LIP_PASEF/"
workflow_dir <- "/maps/projects/metabolomics/apps/script"
mzmine_dir <- "/maps/projects/metabolomics/apps/mzmine_linux/bin/mzmine"
temp_dir <- "/maps/projects/metabolomics/data/Temp/"

# raw_dir <- "N:/SUN-CBMR-Metabolomics/Projects/AMP_test_project/raw/PL_LIP_PASEF"
# workflow_dir <- "N:/SUN-CBMR-Metabolomics/Workflow/script"
# mzmine_dir <- "C:/Users/Public/Documents/mzmine_Windows_portable-4.7.27/mzmine_console.exe"
# temp_dir <- "E:/Temp"

# Create result folder
result_dir <- gsub("raw", "peaktable", raw_dir)
dir.create(result_dir)


# Processing modules
# 1: Preprocessing
rmarkdown::render(
  input = "scripts/preprocessing_mzmine.Rmd",
  params = list(raw_dir = raw_dir, result_dir = result_dir, workflow_dir = workflow_dir,
                mzmine_dir = mzmine_dir, temp_dir = temp_dir),
  output_file = paste0(result_dir, "/preprocessing_mzmine_", Sys.Date(), ".html")
)


# 2: Output conversion
rmarkdown::render(
  input = "scripts/convert_output.Rmd",
  params = list(result_dir = result_dir),
  output_file = paste0(result_dir, "/convert_output_", Sys.Date(), ".html")
)


# 3: QC
rmarkdown::render(
  input = "scripts/qc_istd.Rmd",
  params = list(result_dir = result_dir, workflow_dir = workflow_dir),
  output_file = paste0(result_dir, "/qc_report_", Sys.Date(), ".html")
)


# 4: Post-processing
rmarkdown::render(
  input = "scripts/post_processing.Rmd",
  params = list(result_dir = result_dir),
  output_file = paste0(result_dir, "/post_processing_", Sys.Date(), ".html")
)


# 5: Evaluation
rmarkdown::render(
  input = "scripts/evaluation.Rmd",
  params = list(result_dir = result_dir),
  output_file = paste0(result_dir, "/evaluation_", Sys.Date(), ".html")
)



