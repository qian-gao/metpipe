# metpipe

`metpipe` is an R package for untargeted metabolomics preprocessing workflows.

It provides utilities for:

- importing and harmonizing peak tables and metadata
- cleaning and filtering features
- imputing missing values
- normalization with multiple methods
- QC/evaluation summaries and visualization
- merging mapped positive/negative mode outputs

## Package organization

- Reusable data-processing functions are provided as package API in `R/`.
- End-to-end pipeline entry scripts are distributed in `inst/scripts/`.
- Workflow templates and reports are in `inst/scripts/*.qmd`.

The package is designed so users can either call API functions directly, or run the scripted workflow process.

## Installation

Install from source in a local clone:

```r
install.packages("remotes")
remotes::install_local(".")
```

## Quick start

```r
library(metpipe)

# 1) Import peak table
datalist <- run_import_peaktable(
	peak_table = "path/to/peaktable.xlsx",
	feature_col = 1,
	sample_col_start = 2
)

# 2) Clean/filter
datalist <- run_clean_peaktable(datalist = datalist)

# 3) Normalize
datalist <- run_normalization(datalist = datalist)
```

## Workflow runner (R package usage)

The canonical process is:

1. prepare workflow configuration (`prepare_workflow_qmd.R`)
2. execute workflow modules (`run_workflow_qmd.R`)

You can run the combined wrapper script:

```r
runner <- system.file("scripts", "prepare_run_workflow_qmd.R", package = "metpipe")

system2(
	"Rscript",
	c(
		runner,
		"--raw", "path/to/raw_or_mzML_folder",
		"--result", "path/to/output_folder"
	)
)
```

Optional arguments include `--workflow`, `--mzmine`, `--temp`, `--yaml`, and `--qc`.

## Workflow templates

Package templates and scripts are available in:

- `inst/scripts/`

Key scripts:

- `inst/scripts/prepare_run_workflow_qmd.R`
- `inst/scripts/prepare_workflow_qmd.R`
- `inst/scripts/run_workflow_qmd.R`

## Workflow smoke check

Use the smoke checker to validate config parsing and module resolution without executing full preprocessing.

```r
smoke <- system.file("scripts", "smoke_test_workflow.R", package = "metpipe")

# Qmd modules
system2("Rscript", c(smoke, "--config", "path/to/config.yml"))
```

Optional flags: `--module`, `--package-mode`, `--local-pkg-root`.

## Development notes

- Build/install with `R CMD build .` and `R CMD INSTALL metpipe_*.tar.gz`
- Re-generate docs with `roxygen2` after changing function headers