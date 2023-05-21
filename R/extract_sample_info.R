#' @title extract_sample_info
#'
#' @description Extract sample information from the standardized sample names
#'
#' @param path Path to files
#' @param files File names
#' @param sample.pattern Sample file format, e.g. ".mzML"
#' @param standard.name If standardized sample names are used
#' @param path.meta Metadata file, which should have a key column to match with
#'    sample names
#' @param meta.match.col Column used as key column for matching
#' @param group.var The name of the variable indicating groups of samples
#' @param qc.sample.type QC sample types
#' @param calibration.sample.type Calibration sample types
#' @param export.rds rds name to export
#' @param export.xlsx xlsx name to export
#'
#'
#' @return A data frame object that contains sample names and related information
#'
#' @examples
#'
#'
#' @export
#' @importFrom dplyr "%>%" mutate left_join arrange select
#'
extract_sample_info <-
  function( path = NULL,
            files = NULL,
            sample.pattern = NULL,
            standard.name = TRUE,
            path.meta = NULL,
            meta.match.col = "Sample.name",
            group.var = NULL,
            #po.sample.type = c("PO"),
            qc.sample.type = c("sol", "BL", "CP", "BP", "NIST", "CAL"),
            calibration.sample.type = c("CA00", "CA01", "CA02","CA03", "CA04",
                                        "CA05", "CA06", "CA07", "CA08", "CA09"),
            export.rds = NULL,
            export.xlsx = NULL
  ){

    # sample.pattern <- ".mzML"
    # path.file <- 'Test_files/pos'

    if ( is.null(files) & !is.null(path) ){

      files <-
        list.files( path = path,
                    pattern = sample.pattern,
                    recursive = T,
                    full.names = T )

    }

    if ( !is.null(files)){
      if (!is.null(sample.pattern)){

        file.path <-
          files[
            grepl(
              x = files,
              pattern = sample.pattern
            ) ]

      } else {

        file.path <- files

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

        if (is.data.frame(path.meta)){

          sample.info.other <-
            path.meta %>%
            mutate(across(where(is.character), stringr::str_trim))

        } else {

          sample.info.other <-
            openxlsx::read.xlsx( path.meta ) %>%
            mutate(across(where(is.character), stringr::str_trim))

        }

        if ("Sample" %in% colnames(sample.info.other)){

          sample.info.other <-
            sample.info.other %>%
            mutate( Sample = as.character(Sample))

        }

        sample.info <-
          sample.info %>%
          mutate( Sample.name = gsub( "[.]", "-", Sample.name)) %>%
          left_join( sample.info.other, by = meta.match.col )

        if (!is.null(group.var)){

          sample.info <-
            sample.info %>%
            mutate( Sample.group = paste( group.var, sep = "-"))

        }

      }

    } else if ( is.null(path.file) & !is.null(path.meta)){

      sample.info <-
        openxlsx::read.xlsx( path.meta ) %>%
        mutate(across(where(is.character), stringr::str_trim)) %>%
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
        arrange( Batch, Run.order) %>%
        mutate(
          Sample.temp = stringr::str_split_fixed(
                                          string = Sample,
                                          pattern = "-",
                                          n = Inf)[, 1],
          Sample.type = dplyr::case_when( #Sample.temp %in% po.sample.type           ~ "PO",
                                          Sample.temp %in% qc.sample.type           ~ Sample.temp,
                                          Sample.temp %in% calibration.sample.type  ~ "CA",
                                          TRUE                                      ~ "Sample")) %>%
        select( -Sample.temp )

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


    ### export

    if (!is.null(export.rds)){

      saveRDS( sample.info,
               file = paste0(export.rds, ".rds") )

    }

    if (!is.null(export.xlsx)){

      openxlsx::write.xlsx( sample.info,
                            file = paste0(export.xlsx, ".xlsx") )

    }

    return(sample.info)

  }
