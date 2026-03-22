# metpipe

`metpipe` is an R package for reproducible processing of untargeted metabolomics/lipidomics data. It handles the full process from raw data files to analysis-ready feature matrices, with native support for two widely used software platforms: **mzMine** and **MS-DIAL**. Detailed documentation is available at [qian-gao.github.io/metpipe_docs](https://qian-gao.github.io/metpipe_docs/).

The package is designed for researchers who need a consistent pipeline across studies, either working interactively in R or running automated batch workflows.

## What it does

- **Import** raw data files and harmonize metadata from standardized filenames or user-specified metadata files
- **Preprocess** raw data files with software-specific parameters, including peak picking, alignment, and gap filling
- **QC** internal standards and pooled samples to assess data quality and guide filtering decisions
- **Clean and filter** features by RSD, missing value rate, and dilution series
- **Impute** missing values (half-minimum, limit of detection, median, mean, kNN)
- **Normalize** with multiple strategies including internal standard–based and probabilistic quotient normalization
- **Merge** positive and negative mode outputs and map features to metabolite identities
- **Evaluate** data quality with QC summaries, PCA plots, and IS diagnostics — all exported as HTML reports

## Package organization

- Reusable data-processing functions are provided as package API in `R/`.
- End-to-end pipeline entry scripts are distributed in `inst/scripts/`.
- Workflow templates and reports are in `inst/scripts/*.qmd`.

The package is designed so users can either call API functions directly, or run the scripted workflow process.

## Source

The package source is available on GitHub at [qian-gao/metpipe](https://github.com/qian-gao/metpipe).

## Installation

### From GitHub

```r
install.packages("remotes")
remotes::install_github("qian-gao/metpipe")
```
### From Docker

Docker images are available for users who want to run the full workflow without installing R and dependencies. Available on Docker Hub at [qiangao/metpipe](https://hub.docker.com/r/qiangao/metpipe).

## Usage

There are two ways to use `metpipe`: Script mode and Docker mode.

### Script mode

For batch processing, `metpipe` ships a scripted runner in `inst/scripts/`. A single call handles configuration generation and full pipeline execution:

```r
runner <- system.file("scripts", "prepare_run_workflow.R", package = "metpipe")

system2("Rscript", c(
	runner,
	"--raw",      "path/to/mzML_folder",
	"--result",   "path/to/output_folder",
	"--software", "mzmine"        # or "msdialui"
))
```

Optional arguments include `--workflow`, `--mzmine`, `--temp`, `--yaml`, and `--qc`.

### Docker mode

Execute a workflow script directly:

```bash
docker run --rm \
  qiangao/metpipe:0.8 \
  Rscript /wd/prepare_workflow.R --raw /path/to/raw_folder --result /path/to/output_folder
```

## Output reports

The workflow script mode produces a set of Quarto HTML reports:

| Report | Contents |
|---|---|
| `qc_istd.html` | QC of internal standard and QC samples |
| `post_processing.html` | Clean and filtering steps tracking of samples and features |
| `evaluation.html` | Missing distribution, normalization comparison, PCA, identification summary |
