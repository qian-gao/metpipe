##### Utils preprocessing
### Setup

### -------------------------- Extract IS ---------------------------------- ###

extract.IS <-
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


integer_breaks <- function(n = 50, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

### -------------------------- Create QC ----------------------------------- ###

create.qc <-
  function(sampled_data = NULL,
           figure_height = NULL) {

    sample_nr <- max(sampled_data$Order, na.rm = TRUE)
    figure_width <- sample_nr*40

    if (figure_width < 600) {
      figure_width <- 600
    }

    plt <- htmltools::tagList()
    plt[[1]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, rt, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Retention Time as a Function of Running Time",
                                       x = "",
                                       y = "RT (min)",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))

    plt[[2]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, rt.dev, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "RT deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: RT (min)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))
    plt[[2]] <- style(plt[[2]], showlegend = FALSE)

    plt[[3]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, mz, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "m/z as a Function of Running Time",
                                       x = "",
                                       y = "m/z",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))

    plt[[4]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, mz.dev, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "m/z deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: m/z (ppm)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))
    plt[[4]] <- style(plt[[4]], showlegend = FALSE)

    plt[[5]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, Intensity, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Peak area as a Function of Running Time",
                                       x = "Running order",
                                       y = "Peak area",
                                       color = "Internal standard", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))

    plt[[6]] <- as_widget(ggplotly(ggplot(sampled_data, aes(Order, Intensity.dev, color = ISTD, text = Sample.name)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_point(aes(shape = ISTD)) + scale_shape_manual(values = c(1:25)) +
                                     theme_bw() +
                                     labs(#title = "Peak area deviation as a Function of Running Time",
                                       x = "Running order",
                                       y = "Deviation: Peak area (%)",
                                       color = "", shape ="") +
                                     scale_x_continuous(limits = c(0, sample_nr), breaks = integer_breaks(sample_nr))

                                   , width = figure_width, height = figure_height
    ))
    plt[[6]] <- style(plt[[6]], showlegend = FALSE)

    summary <-
      sampled_data %>%
      group_by( ISTD, Sample.type) %>%
      summarize( RSD = sd(Intensity, na.rm = TRUE) / mean(Intensity, na.rm = TRUE)*100 )

    plt[[7]] <- as_widget(ggplotly(ggplot(summary, aes(ISTD, RSD, fill = Sample.type)) +
                                     #geom_text(aes(label = ISTD.ID), hjust=0.5, vjust=0.5) +
                                     geom_bar(stat = "Identity", position = "dodge") +
                                     scale_x_discrete(limits=rev) +
                                     theme_bw() +
                                     labs( #title = "RSD of internal standards",
                                       x = "Internal standards",
                                       y = "RSD (%)") +
                                     coord_flip()

                                   , width = 1000
    ))

    summary <-
      sampled_data %>%
      select(ISTD, Theoretical.rt, median.rt, Theoretical.mz, median.mz, median.intensity, RSD.intensity) %>%
      unique()

    plt[[8]] <- as_widget(ggplotly(ggplot(summary, aes(x = Theoretical.rt, y = median.rt, color = ISTD )) +
                                     geom_abline(intercept = 0, slope = 1, color = "gray") +
                                     geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue", alpha = 0.1) +
                                     geom_point(size = 2, aes(shape = ISTD)) + scale_shape_manual(values = c(1:25))

                                   , width = 800
    ))

    return(plt)
  }
