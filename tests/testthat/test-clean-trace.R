make_mode_data <- function(values, sample.type, rt, mz = seq(100, by = 50, length.out = nrow(values)),
                           feature_type = "Known") {
  ids <- rownames(values)
  samples <- paste0("S", seq_along(sample.type))
  peaks <- as.data.frame(values)
  colnames(peaks) <- samples
  rownames(peaks) <- NULL
  list(
    peaks = peaks,
    features = data.frame(
      rt = rt, mz = mz, Identity = ids,
      Feature_type = feature_type, Feature_ID = paste0("FT", seq_along(ids)),
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
    good2 = good,
    # never detected in blanks: must pass 2*BL < PO
    noblank = c(NA, NA, NA, 1000, 1050, 950, 1000, rep(1000, 6)),
    # absent from blanks and PO: must still fail 2*BL < PO
    nopo  = c(NA, NA, NA, NA, NA, NA, NA, rep(1000, 6))
  )
  types <- c("Known", "Unknown", "Known", "Unknown", "Known", "Unknown", "Unknown", "Known", "Known")
  md <- make_mode_data(v, st, rt = c(5, 5, 5, 5, 35, 5, 5, 5, 5), feature_type = types)

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
      late = "rt", miss = "missing", good2 = NA, noblank = NA, nopo = "mean_1")
  )
  expect_true(is.nan(tr$mean.BL[tr$Identity == "noblank"]))
  expect_setequal(tr$Identity[tr$status == "Kept"], res$features$Identity)
  expect_equal(vapply(res$clean_trace$steps, `[[`, "", "id"),
               c("mean_1", "mean_2", "rsd_1", "rt", "missing"))
  expect_equal(tr$missing.pct[tr$Identity == "miss"], 4 / 6 * 100)

  plots <- plot_cleaning_steps(res, mode = "pos")
  expect_named(plots, c("funnel", "mean_1", "mean_2", "rsd_1", "rt", "missing",
                        "rsd_before_after_1"))
  expect_true(all(vapply(plots, ggplot2::is_ggplot, logical(1))))

  # Known / Unknown / All are shown in every figure
  expect_equal(tr$Feature_type, types)
  funnel <- plots$funnel$data
  expect_setequal(as.character(funnel$group), c("All", "Known", "Unknown"))
  kept <- funnel[funnel$step == levels(funnel$step)[nlevels(funnel$step)], ]
  expect_equal(setNames(kept$remaining, kept$group), c(All = 3, Known = 2, Unknown = 1))
  for (nm in setdiff(names(plots), "funnel")) {
    panels <- levels(droplevels(plots[[nm]]$data$panel))
    expect_length(panels, 3)
    expect_match(panels, "^(All|Known|Unknown)")
  }
  # RSD step: the high-RSD feature is Unknown
  expect_true(any(grepl("^Unknown: 1 of", levels(plots$rsd_1$data$panel))))
  expect_true(any(grepl("^Known: 0 of", levels(plots$rsd_1$data$panel))))

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
