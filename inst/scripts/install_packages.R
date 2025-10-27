install.packages(c(
  "rmarkdown",   # for rendering Rmd reports
  "tidyverse",   # common data manipulation and plotting
  "data.table",  # efficient data handling
  "knitr",        # required by rmarkdown
  "devtools",
  "plotly",
  "openxlsx",
  "xml2",
  "VIM"
), repos = "https://cloud.r-project.org/")

remotes::install_github("https://github.com/qian-gao/metpipe",
                         auth_token = "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")
