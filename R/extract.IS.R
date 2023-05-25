#' @title extract.is
#'
#' @description Extract internal standard measurements from MZmine3 output
#'
#' @param sample.info A dataframe containing sample info
#' @param reference.type mz and RT to compare to, choose from c("reference", "median")
#' @param lib.istd A dataframe of library for internal standards
#' @param path.peaktable.IS Path to peaktable for internal standards in samples
#' @param path.result Path to output files
#'
#' @return A list of summaries for all types of samples
#'
#' @examples
#'
#' @export extract.is
#' @import dplyr
#'
extract.is <-
  function( sample.info = NULL,
            reference.type = NULL,
            lib.istd = NULL,
            path.peaktable.IS = NULL,
            path.result = NULL
  ){

    ### Internal standard

    istd <-
      lib.istd %>%
      dplyr::rename( ISTD = Library.name,
                     Theoretical.rt = Library.rt,
                     Theoretical.mz = Library.mz)

    peaktable.is <-
      read.csv( path.peaktable.IS, sep = ";", stringsAsFactors = FALSE ) %>%
      dplyr::rename( mz = row.m.z,
                     rt = row.retention.time,
                     ISTD = row.identity..main.ID.)

    # colnames.peak.area <- grep(".Peak.area", colnames(peaktable.is)) # mzmine3
    # colnames.mz <- grep(".Feature.m.z", colnames(peaktable.is))
    # colnames.rt <- grep(".Feature.RT", colnames(peaktable.is))

    colnames.peak.area <- grep("Peak.area", colnames(peaktable.is))
    colnames.mz <- grep("Feature.m.z", colnames(peaktable.is))
    colnames.rt <- grep("Feature.RT", colnames(peaktable.is))

    all_file <- data.frame()
    for (i in 1:length(colnames.mz)) {

      sample <-
        peaktable.is %>%
        select( colnames.mz[i],
                colnames.rt[i],
                ISTD,
                colnames.peak.area[i]) %>%
        mutate( ISTD = substr(ISTD, 1, regexpr("\\:[^\\:]*$", ISTD)-1))

      Sample.name <- gsub( ".mzML.Feature.m.z", "", colnames(sample)[1]) # mzmine3
      #Sample.name <- gsub( ".mzML.Peak.m.z", "", colnames(sample)[1])

      sample <-
        sample %>%
        mutate( Sample.name = Sample.name) %>%
        dplyr::rename( mz       = 1,
                       rt        = 2,
                       Intensity = 4 )

      all_file <- rbind(all_file, sample)

    }

    data.plot <-
      all_file %>%
      mutate( Sample.name = gsub("[.]", "-", Sample.name)) %>%
      left_join(sample.info, by = "Sample.name") %>%
      filter(!Sample.type %in% c("Met", "sol"))

    # Remove not detected IS
    data.plot <-
      data.plot[!data.plot$mz == 0, ]

    # Combine with theoretical ISTD
    combined_file <-
      inner_join(data.plot, istd, by= "ISTD") %>%
      arrange(Batch, Run.order) %>%
      group_by(Sample.name, ISTD) %>%
      mutate( n = n(),
              ISTD = ifelse( n > 1,
                             paste0( ISTD, "-rep", row_number()),
                             ISTD)) %>%
      group_by(ISTD) %>%
      mutate(
        median.rt        = median(rt,  na.rm = TRUE),
        median.mz        = median(mz, na.rm = TRUE),
        median.intensity = median(Intensity, na.rm = TRUE),
        RSD.intensity    = sd(Intensity, na.rm = TRUE) / mean(Intensity, na.rm = TRUE)*100,
        Order = as.numeric(Sample.id)) %>%
      ungroup()

    if (reference.type == "reference") {

      combined_file <-
        combined_file %>%
        mutate(
          rt.dev = rt - Theoretical.rt,
          mz.dev = (mz - Theoretical.mz)/Theoretical.mz*1E6,
          Intensity.dev = (Intensity - median.intensity)/median.intensity*100)

    } else if (reference.type == "median") {

      combined_file <-
        combined_file %>%
        mutate(
          rt.dev = rt - median.rt,
          mz.dev = (mz - median.mz)/median.mz*1E6,
          Intensity.dev = (Intensity - median.intensity)/median.intensity*100)
    }

    return(combined_file)

  }
