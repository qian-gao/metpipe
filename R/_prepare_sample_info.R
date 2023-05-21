prepare_sample_info <-
  function( path = NULL,
            files = NULL,
            path.meta = NULL,
            meta.match.col = NULL,
            group.var = NULL,
            sample.pattern = NULL,
            standard.name = NULL,
            #po.sample.type = NULL,
            qc.sample.type = NULL,
            calibration.sample.type = NULL,
            mode = NULL,
            path.result = NULL,
            prefix = ""
  ){

    if ( !file.exists(paste0(path.result, "sample_info_", mode, ".rds")) ){

      sample.info <-
        extract_sample_info( path = path,
                             files = files,
                             sample.pattern = sample.pattern,
                             standard.name = standard.name,
                             path.meta = path.meta,
                             meta.match.col = meta.match.col,
                             group.var = group.var,
                             #po.sample.type = po.sample.type,
                             qc.sample.type = qc.sample.type,
                             calibration.sample.type = calibration.sample.type
        ) %>%
        mutate( Sample.name = gsub( "[.]", "-", Sample.name))

      saveRDS( sample.info,
               file = paste0(path.result, prefix, "sample_info_", mode, ".rds") )

      openxlsx::write.xlsx( sample.info,
                            file = paste0(path.result, prefix, "sample_info_", mode, ".xlsx") )

    } else {

      sample.info <- readRDS( paste0(path.result, "sample_info_", mode, ".rds") )

    }

    return(sample.info)

  }

extract_sample_info <-
  function( path.file = NULL,
            sample.pattern = NULL,
            standard.name = TRUE,
            path.meta = NULL,
            meta.match.col = "Sample.name",
            group.var = NULL,
            #po.sample.type = c("PO"),
            qc.sample.type = c("sol", "BL", "CP", "BP", "NIST", "CAL"),
            calibration.sample.type = c("CA00", "CA01", "CA02","CA03", "CA04",
                                        "CA05", "CA06", "CA07", "CA08", "CA09")
  ){

    # sample.pattern <- ".mzML"
    # path.file <- 'Test_files/pos'

    `%>%` <- magrittr::`%>%`

    if ( !is.null(path.file)){
      if (!is.null(sample.pattern)){

        file.path <-
          path.file[
            grepl(
              x = path.file,
              pattern = sample.pattern
            ) ]

      } else {

        file.path <- path.file

      }

      file.name <-
        stringr::str_split_i(
          string = file.path,
          pattern = "/",
          i = -1
        )

      file.batch <-
        stringr::str_split_i(
          string = file.path,
          pattern = "/",
          i = -2
        )

      if (!is.null(sample.pattern)){

        sample.name <-
          gsub(
            pattern = sample.pattern,
            replacement = "",
            x = file.name
          )

      } else {

        sample.name <- file.name

      }

      if (standard.name){

        sample.info <-
          data.frame(
            File.path = file.path,
            File.name = file.name,
            File.batch = file.batch,
            Sample.name = sample.name,
            stringr::str_split_fixed(
              string = sample.name,
              pattern = "_",
              n = Inf
            )[, 1:7],
            stringsAsFactors = FALSE
          )

        colnames(sample.info) <-
          c("File.path", "File.name", "File.batch", "Sample.name",
            "Project", "Method", "Date", "Run.order", "Sample", "Extract.rep", "Tech.rep" )

        sample.info <-
          sample.info %>%
          arrange(Run.order)

      } else {

        sample.info <-
          data.frame(
            File.path = file.path,
            File.name = file.name,
            File.batch = file.batch,
            Sample.name = sample.name,
            Sample = sample.name,
            stringsAsFactors = FALSE
          )

      }

      if (!is.null(path.meta)){

        sample.info.other <-
          openxlsx::read.xlsx( path.meta ) %>%
          dplyr::mutate(across(where(is.character), stringr::str_trim))

        if ("Sample" %in% colnames(sample.info.other)){

          sample.info.other <-
            sample.info.other %>%
            mutate( Sample = as.character(Sample))

        }

        sample.info <-
          sample.info %>%
          dplyr::mutate( Sample.name = gsub( "[.]", "-", Sample.name)) %>%
          dplyr::left_join( sample.info.other, by = meta.match.col )

        if (!is.null(group.var)){

          sample.info <-
            sample.info %>%
            mutate( Sample.group = paste( group.var, sep = "-"))

        }

      }

    } else if ( is.null(path.file) & !is.null(path.meta)){

      sample.info <-
        openxlsx::read.xlsx( path.meta ) %>%
        dplyr::mutate(across(where(is.character), stringr::str_trim)) %>%
        mutate( Sample = Sample.name)

    } else {

      print("Please provide file names or metadata.")

    }

    if (!"Batch" %in% colnames(sample.info)){

      batches <- unique(sample.info$File.batch)

      if (length(batches) > 1){

        sample.info <-
          sample.info %>%
          rowwise() %>%
          mutate( Batch = paste0("Batch ", grep(File.batch, batches))) %>%
          ungroup()

      } else {

        sample.info[, "Batch"] <- "No batch"

      }

    }

    if (!"Run.order" %in% colnames(sample.info)){

      sample.info[, "Run.order"] <- sprintf("%03d", 1:nrow(sample.info))

    }

    if (!"Sample.type" %in% colnames(sample.info)){
      sample.info <-
        sample.info %>%
        dplyr::arrange( Batch, Run.order) %>%
        dplyr::mutate(
          Sample.temp = stringr::str_split_fixed(
            string = Sample,
            pattern = "-",
            n = Inf)[, 1],
          Sample.type = dplyr::case_when( #Sample.temp %in% po.sample.type           ~ "PO",
            Sample.temp %in% qc.sample.type           ~ Sample.temp,
            Sample.temp %in% calibration.sample.type  ~ "CA",
            TRUE                                      ~ "Sample")) %>%
        dplyr::select( -Sample.temp )

    }

    if (!"Sample.group" %in% colnames(sample.info)){

      sample.info[, "Sample.group"] <- sample.info$Sample.type

    } else {

      sample.info$Sample.group <-
        ifelse( is.na(sample.info$Sample.group),
                sample.info$Sample.type,
                sample.info$Sample.group )

    }

    sample.info <-
      sample.info %>%
      mutate( Sample.id = as.character(dplyr::row_number()),
              Run.order = ifelse( Batch == "No batch",
                                  Run.order,
                                  paste0( substr(Batch, 7, nchar(Batch)), "_", Run.order)),
              Sample.unique = ifelse( Sample.type == "Sample",
                                      paste( sep = "_", Project, Sample),
                                      paste( sep = "_", Project, Sample, substr(Method, 1, 3), Extract.rep,	Tech.rep) ))

    return(sample.info)

  }
