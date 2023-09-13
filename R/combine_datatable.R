combine_datatable <-
  function( list = NULL,
            list_df = NULL,
            sample_name_col_nr = NULL,
            feature_col_nr = NULL){

    if (is.null(list_df)) {
      names <- names(list)
      list_df <-
        lapply( list, function(x){

          df <- readxl::read_excel(x)
          df <- df[, c(sample_name_col_nr, feature_col_nr:ncol(df))]
          colnames(df)[1] <- "Sample"
          return(df)
        })

      names(list_df) <- names
    }

    rsd_df <-
      data.table::rbindlist(
        lapply( list_df, function(x){
          rsd <-
            calculate_add_rsd( data = x[, -1],
                               type = "Sample")
        }),
        idcol = "Method")

    features <-
      rsd_df %>%
      group_by(Identity) %>%
      arrange(Identity, desc(mean.Sample)) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      as.data.frame()

    list_df_s <-
      lapply( names, function(x){

        df <- list_df[[x]][, c("Sample", features[ features$Method == x, ]$Identity)]

      })


    final <-
      list_df_s[[1]] %>%
      as.data.frame()

    for (i in 2:length(names)){

      final <-
        final %>%
        full_join(list_df_s[[i]], by = "Sample")

    }

    features.final <-
      features[ match(colnames(final)[-1], features$Identity), ]

    result <-
      list( data = final,
            feature.info = features.final)

    return(result)

  }
