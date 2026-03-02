FROM rocker/tidyverse:4.3.2

# install system deps for typical R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/2025-10-01'))" \
      -e "install.packages(c('remotes', 'plotly', 'openxlsx', 'ggsci', 'reshape2', 'data.table', 'laeken', 'BiocManager', 'tinytex', 'kableExtra'))"
#RUN R -e "install.packages(c('grid', 'readxl','xml2', 'rmarkdown', 'knitr', 'curl', 'htmltools', 'scales', 'stringi', 'yaml'), repos='https://cloud.r-project.org/')"
RUN R -e "BiocManager::install('VIM')"
RUN R -e "tinytex::install_tinytex()"

# Install GitHub packages
RUN R -e "remotes::install_github('qian-gao/metpipe', auth_token = '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')"

# Set work directory
WORKDIR /wd
COPY workflow/ /wd/

