make_mode_data <- function(values, sample.type, rt, mz = seq(100, by = 50, length.out = nrow(values))) {
  ids <- rownames(values)
  samples <- paste0("S", seq_along(sample.type))
  peaks <- as.data.frame(values)
  colnames(peaks) <- samples
  rownames(peaks) <- NULL
  list(
    peaks = peaks,
    features = data.frame(
      rt = rt, mz = mz, Identity = ids,
      Feature_type = "Known", Feature_ID = paste0("FT", seq_along(ids)),
      stringsAsFactors = FALSE
    ),
    meta = data.frame(Sample = samples, Sample.type = sample.type, stringsAsFactors = FALSE)
  )
}

test_that("clean_peaktable records which step removed each feature", {
  st <- c(rep("BL", 3), rep("PO", 4), rep("Sample", 6))
  good <- c(10, 10, 10, 1000, 1050, 950, 1000, rep(1000, 6))
  v <- rbind(
    good  = good,
    blank = c(800, 800, 800, 1000, 1050, 950, 1000, rep(1000, 6)),
    low   = c(10, 10, 10, 1000, 1050, 950, 1000, rep(100, 6)),
    rsd   = c(10, 10, 10, 100, 2000, 500, 3000, rep(1000, 6)),
    late  = good,
    miss  = c(10, 10, 10, 1000, 1050, 950, 1000, 1000, 1000, NA, NA, NA, NA),
    good2 = good
  )
  md <- make_mode_data(v, st, rt = c(5, 5, 5, 5, 35, 5, 5))

  # YAML-style character rules
  out <- suppressWarnings(capture.output(res <- clean_peaktable(
    md,
    mean.filter = list(c("2", "BL", "<", "PO"), c("1", "Sample", ">", "500")),
    rsd.filter = list(c("1", "PO", "<", "0.3")),
    rt.range = c(0, 30),
    filter_by_missing_feature_pct = 50,
    filter_by_missing_sample_type = "Sample",
    po.sample.to.use = "PO"
  )))

  tr <- res$clean_trace$features
  expect_equal(
    setNames(tr$removed_at, tr$Identity),
    c(good = NA, blank = "mean_1", low = "mean_2", rsd = "rsd_1",
      late = "rt", miss = "missing", good2 = NA)
  )
  expect_setequal(tr$Identity[tr$status == "Kept"], res$features$Identity)
  expect_equal(vapply(res$clean_trace$steps, `[[`, "", "id"),
               c("mean_1", "mean_2", "rsd_1", "rt", "missing"))
  expect_equal(tr$missing.pct[tr$Identity == "miss"], 4 / 6 * 100)

  plots <- plot_cleaning_steps(res, mode = "pos")
  expect_named(plots, c("funnel", "mean_1", "mean_2", "rsd_1", "rt", "missing",
                        "rsd_before_after_1"))
  expect_true(all(vapply(plots, ggplot2::is_ggplot, logical(1))))

  widgets <- cleaning_plotly(plots)
  expect_length(widgets, length(plots))
})

test_that("cleaning plots take sample types and thresholds from the rules", {
  st <- c("Blank", "Blank", "QC", "QC", "QC", rep("Study", 4), "QC50", "QC100", "QC200")
  v <- rbind(
    g1 = c(5, 5, 1000, 1010, 990, rep(1000, 4), 500, 1000, 2000),
    g2 = c(5, 5, 1000, 1010, 990, rep(1000, 4), 2000, 1000, 500),
    g3 = c(5, 5, 100, 3000, 900, rep(1000, 4), 500, 1000, 2000),
    g4 = c(500, 500, 1000, 1010, 990, rep(1000, 4), 500, 1000, 2000)
  )
  md <- make_mode_data(v, st, rt = rep(2, 4))

  out <- suppressWarnings(capture.output(res <- clean_peaktable(
    md,
    mean.filter = list(c("3", "Blank", "<", "QC")),
    rsd.filter = list(c("1", "QC", "<", "0.25")),
    dilution.filter = list(dilution.types = c("QC50", "QC100", "QC200"),
                           levels = c(50, 100, 200), min.cor = 0.6),
    filter_by_missing_feature_pct = 50,
    filter_by_missing_sample_type = "Study",
    po.sample.to.use = "QC"
  )))

  tr <- res$clean_trace$features
  expect_equal(setNames(tr$removed_at, tr$Identity),
               c(g1 = NA, g2 = "dilution", g3 = "rsd_1", g4 = "mean_1"))
  expect_equal(tr$dilution.cor[tr$Identity == "g2"], -1)

  plots <- plot_cleaning_steps(res$clean_trace)
  expect_named(plots, c("funnel", "mean_1", "rsd_1", "dilution", "missing",
                        "rsd_before_after_1"))
  expect_match(ggplot2::get_labs(plots$rsd_1)$x, "RSD in QC")
  expect_match(ggplot2::get_labs(plots$mean_1)$y, "QC")
  expect_match(ggplot2::get_labs(plots$missing)$x, "Study")
})
