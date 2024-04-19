#' @title calculate_rsd
#'
#' @description Calculate summary for subsets of dataset
#'
#' @param data A data frame object, sample x feature
#' @param type A vector indicating sample types
#' @param impute If impute missing values
#' @param names.suffix Suffix to add on output variable names
#' @param other.eval If calculate other summary (MAD and VAR)
#'
#' @return A list of summaries for all types of samples
#'
#' @examples
#'
#' @export
#' @importFrom dplyr "%>%" mutate left_join arrange select summarise group_by
#'     ungroup rename_all
#' @importFrom tidyr pivot_longer pivot_wider

calculate_rsd <-
  function( data,
            type,
            impute = FALSE,
            names.suffix = NULL,
            other.eval = FALSE
  ){

    ### Impute missing
    if (impute){
      hm <- min(data, na.rm = T) / 2
      data[ is.na(data) ] <- hm
    }

    ### Calculate

    map.names <- colnames(data)
    names(map.names) <- make.names(map.names)

    rsd <-
      data.frame( Sample.type = type, data) %>%
      pivot_longer( -Sample.type, names_to = "Identity", values_to = "Intensity") %>%
      group_by( Identity, Sample.type) %>%
      summarise( mean = mean(Intensity, na.rm = TRUE),
                 sd = sd(Intensity, na.rm = TRUE),
                 rsd = sd/mean ) %>%
      ungroup() %>%
      mutate( names.suf = ifelse( is.null(names.suffix), "", names.suffix),
              Sample.type = ifelse( names.suf == "",
                                    Sample.type,
                                    paste0(Sample.type, ".", names.suffix)) ) %>%
      pivot_wider(names_from = Sample.type, values_from = c(mean, sd, rsd),
                  names_glue = "{.value}.{Sample.type}") %>%
      arrange(match(Identity, names(map.names))) %>%
      mutate(Identity = map.names[Identity])


    mean <-
      rsd %>%
      select(Identity, starts_with("mean.")) %>%
      rename_all(~stringr::str_replace(.,"^mean.",""))

    sd <-
      rsd %>%
      select(Identity, starts_with("sd.")) %>%
      rename_all(~stringr::str_replace(.,"^sd.",""))

    rsd <-
      rsd %>%
      select(Identity, starts_with("rsd.")) %>%
      rename_all(~stringr::str_replace(.,"^rsd.",""))


    output <-
      list( type.mean = mean,
            type.sd   = sd,
            type.rsd  = rsd )

    if (other.eval){

      other <-
        data.frame( Sample.type = type, data) %>%
        pivot_longer( -Sample.type, names_to = "Identity", values_to = "Intensity") %>%
        group_by( Identity, Sample.type) %>%
        summarise( mad = mad(Intensity, na.rm = TRUE),
                   var = var(Intensity, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate( names.suf = ifelse( is.null(names.suffix), "", names.suffix),
                Sample.type = ifelse( names.suf == "",
                                      Sample.type,
                                      paste0(Sample.type, ".", names.suffix)) ) %>%
        pivot_wider(names_from = Sample.type, values_from = c(mad, var),
                    names_glue = "{.value}.{Sample.type}") %>%
        arrange(match(Identity, names(map.names))) %>%
        mutate(Identity = map.names[Identity])

      mad <-
        other %>%
        select(Identity, starts_with("mad.")) %>%
        rename_all(~stringr::str_replace(.,"^mad.",""))

      var <-
        other %>%
        select(Identity, starts_with("var.")) %>%
        rename_all(~stringr::str_replace(.,"^var.",""))

      output$type.mad <- mad
      output$type.var <- var

    }

    return(output)

  }
