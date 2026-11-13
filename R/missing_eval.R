#' missing_eval
#'
#' Plot missing-value distributions: an overall heatmap, missingness per
#' sample and missingness per feature.
#'
#' @param data_cleaned Feature x sample intensity data frame (features in rows,
#'   rownames used as feature IDs).
#' @param sample.type Optional vector of sample types. Either unnamed and in the
#'   column order of `data_cleaned`, or named by sample. When given, the
#'   per-sample plot is coloured by sample type.
#' @param feature.type Optional vector of feature annotations (e.g. Known /
#'   Unknown), in the row order of `data_cleaned`. When given, the per-feature
#'   plot is coloured by it.
#'
#' @return A list of ggplot objects `p1`, `p2`, `p3`.
#' @import dplyr tidyr
#' @export
missing_eval <- 
  function(
    data_cleaned = NULL,
    sample.type = NULL,
    feature.type = NULL
    ){
    
    figs <- list()
    
    df <- 
      data_cleaned %>%
      rownames_to_column("Feature") %>% 
      pivot_longer(-Feature, names_to = "Sample", values_to = "var") %>%
      mutate(isna = (is.na(var)|var<=0))
    
    figs$p1 <- ggplot(df, aes(x = Sample, y = Feature, fill = isna)) +
      geom_raster() +
      scale_fill_manual(name = "",
                        values = c('#132b43', '#4fa3e4'),
                        labels = c("Present", "Missing")) +
      labs(title = "Missing values") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())
    
    data.plot <- data_cleaned
    pct_missing <- function(x) sum(is.na(x) | x <= 0) / length(x) * 100
    
    # ---- Missing per sample, coloured by sample type ----
    miss.sample <- data.frame(Sample = colnames(data.plot),
                              missing = apply(data.plot, 2, pct_missing))
    
    if (!is.null(sample.type)) {
      if (!is.null(names(sample.type))) {
        st <- unname(sample.type[miss.sample$Sample])
      } else if (length(sample.type) == nrow(miss.sample)) {
        st <- sample.type
      } else {
        stop("`sample.type` must be named by sample or match the number of samples.")
      }
      st <- as.character(st)
      st[is.na(st) | st == ""] <- "Unknown type"
      miss.sample$Sample.type <- factor(st, levels = unique(sort(st)))
      miss.sample <- miss.sample[order(miss.sample$Sample.type), ]
      miss.sample$Sample <- factor(miss.sample$Sample, levels = miss.sample$Sample)
      
      figs$p2 <- ggplot(miss.sample, aes(x = Sample, y = missing, fill = Sample.type)) +
        geom_bar(stat = "identity") +
        labs(x = "Samples",
             y = "Proportion of missing values (%)",
             fill = "Sample type")
    } else {
      figs$p2 <- ggplot(miss.sample, aes(x = Sample, y = missing)) +
        geom_bar(stat = "identity") +
        labs(x = "Samples",
             y = "Proportion of missing values (%)")
    }
    figs$p2 <- figs$p2 +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank())
    
    # ---- Missing per feature, coloured by Known / Unknown ----
    miss.feature <- data.frame(Feature = rownames(data.plot),
                               missing = apply(data.plot, 1, pct_missing))
    
    if (!is.null(feature.type)) {
      if (length(feature.type) != nrow(miss.feature)) {
        stop("`feature.type` must have one value per feature (row) of `data_cleaned`.")
      }
      ft <- as.character(feature.type)
      ft[is.na(ft) | ft == ""] <- "Unknown"
      lv <- c(intersect(c("Known", "Unknown"), ft),
              sort(setdiff(unique(ft), c("Known", "Unknown"))))
      miss.feature$Feature_type <- factor(ft, levels = lv)
      miss.feature <- miss.feature[order(miss.feature$Feature_type), ]
      miss.feature$Feature <- factor(miss.feature$Feature, levels = unique(miss.feature$Feature))
      
      figs$p3 <- ggplot(miss.feature, aes(x = Feature, y = missing, fill = Feature_type)) +
        geom_bar(stat = "identity") +
        labs(x = "Features",
             y = "Proportion of missing values (%)",
             fill = "Feature type")
    } else {
      figs$p3 <- ggplot(miss.feature, aes(x = Feature, y = missing)) +
        geom_bar(stat = "identity") +
        labs(x = "Features",
             y = "Proportion of missing values (%)")
    }
    figs$p3 <- figs$p3 +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank())
  
    return(figs)
    
  }
