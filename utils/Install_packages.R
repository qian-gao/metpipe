### Check required packages. If not installed, install it

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

installed.pkgs <- installed.packages()[,"Package"]

pkgs.from.cran <- c("devtools", "openxlsx", "tidyverse",
                    "ggsci", "pmd", "kableExtra"
                    )
new.pkgs.from.cran <- pkgs.from.cran[ !pkgs.from.cran %in% installed.pkgs ]

if( length(new.pkgs.from.cran) ) {
  
  install.packages( new.pkgs.from.cran,
                    dependencies = TRUE)
  
}

installed.pkgs <- installed.packages()[,"Package"]

pkgs.from.bioc <- c("CAMERA", #"limma", "sva"
                    )

new.pkgs.from.bioc <- pkgs.from.bioc[ !pkgs.from.bioc %in% installed.pkgs ]

if( length(new.pkgs.from.bioc) ) {
  
  BiocManager::install( new.pkgs.from.bioc )
  
}

devtools::install_github("https://github.com/qian-gao/metpipe", 
                         auth_token = "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")


install.packages("C:/Users/zfj107/Downloads/ggrepel_0.9.2.tar.gz", repos = NULL, type="source")

installed.pkgs <- installed.packages()[,"Package"]

pkgs.from.cran <- c("ggrepel",
                    "ggh4x",
                    "pheatmap",
                    "circlize",
                    "lmerTest",
                    "emmeans",
                    "car",
                    "tableone",
                    "rstatix",
  
  "ggplot2",
                    #"tidyverse",
                    "dplyr",
                    "tidyr",
                    "tibble",
                    "stringr",
                    "purrr",
                    "reshape2",
                    "openxlsx",
                    "viridis",
                    "plotly",
                    "BiocManager",
                    "shiny",
                    "shinyjs",
                    "shinyFiles",
                    "DT",
                    "data.table",
                    "mdatools",
                    "crmn",
                    "car",
                    "lmerTest",
                    "merTools",
                    "colortools",
                    "tableone",
                    "tidymv",
                    "visreg",
                    "ggsci",
                    "emmeans",
                    "ordinal",
                    "ggeffects",
                    "rmarkdown",
                    "R.utils",
                    "pmd"
                  )

BiocManager::install("ComplexHeatmap")
remotes::install_gitlab("tidymass/metpath")

new.pkgs.from.cran <- pkgs.from.cran[ !pkgs.from.cran %in% installed.pkgs ]

if( length(new.pkgs.from.cran) ) {

  install.packages( new.pkgs.from.cran,
                    dependencies = TRUE)

}

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.17")

installed.pkgs <- installed.packages()[,"Package"]

pkgs.from.bioc <- c(
                    "limma",
                    
                    
                    
                    "affy",
                    "sva",
                    "ropls",
                    "mixOmics",
                    "rstatix",
                    "xcms",
                    "Rdisop",
                    "CAMERA"
                  )

new.pkgs.from.bioc <- pkgs.from.bioc[ !pkgs.from.bioc %in% installed.pkgs ]

if( length(new.pkgs.from.bioc) ) {

  BiocManager::install( new.pkgs.from.bioc )

}


