docker run -it `
    -v E:/:/mnt/e `
    qiangao/metpipe:0.1 `
    Rscript /wd/prepare_run_workflow_qmd.R --raw "/mnt/e/Projects/MP_workshop/raw/PL_LIPS_PASEF"

pause