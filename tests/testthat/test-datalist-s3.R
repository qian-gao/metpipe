test_that("as_metpipe_datalist preserves existing fields", {
  x <- as_metpipe_datalist(list(pos = list(raw = 1), neg = NULL, extra = 5), stage = "imported")
  expect_true(is_metpipe_datalist(x))
  expect_identical(x$extra, 5)
  expect_identical(attr(x, "stage"), "imported")
})

test_that("validate_metpipe_datalist enforces merged stage fields", {
  x <- new_metpipe_datalist(pos = list(raw = 1), neg = NULL)
  expect_error(validate_metpipe_datalist(x, stage = "merged"), "missing merged outputs")
})

test_that("print.metpipe_datalist is callable", {
  x <- new_metpipe_datalist(
    pos = list(
      peaks = matrix(1, nrow = 2, ncol = 2),
      features = data.frame(id = 1:2),
      meta = data.frame(Sample = c("S1", "S2"))
    ),
    neg = NULL,
    stage = "imported"
  )
  expect_invisible(print(x))
})

test_that("summary.metpipe_datalist has printable output", {
  x <- new_metpipe_datalist(
    pos = list(
      peaks = matrix(1, nrow = 2, ncol = 2),
      features = data.frame(id = 1:2),
      meta = data.frame(Sample = c("S1", "S2"))
    ),
    neg = NULL,
    stage = "imported"
  )
  s <- summary(x)
  expect_true(inherits(s, "summary.metpipe_datalist"))
  expect_invisible(print(s))
})

test_that("validate_metpipe_datalist catches mode dimension mismatch", {
  bad <- list(
    pos = list(
      peaks = matrix(1, nrow = 3, ncol = 2),
      features = data.frame(id = 1:2),
      meta = data.frame(Sample = c("S1", "S2"))
    ),
    neg = NULL
  )

  expect_error(
    as_metpipe_datalist(bad, stage = "imported", validate = TRUE),
    "inconsistent dimensions"
  )
})

test_that("has_mode and get_mode work for pos/neg", {
  x <- new_metpipe_datalist(
    pos = list(
      peaks = matrix(1, nrow = 2, ncol = 2),
      features = data.frame(id = 1:2),
      meta = data.frame(Sample = c("S1", "S2"))
    ),
    neg = NULL,
    stage = "imported"
  )

  expect_true(has_mode(x, "pos"))
  expect_false(has_mode(x, "neg"))
  expect_true(is.list(get_mode(x, "pos")))
  expect_error(get_mode(x, "neg", required = TRUE), "requested mode")
  expect_null(get_mode(x, "neg", required = FALSE))
})

test_that("is_merged reflects merged output presence", {
  x <- new_metpipe_datalist(
    pos = list(
      peaks = matrix(1, nrow = 2, ncol = 2),
      features = data.frame(id = 1:2),
      meta = data.frame(Sample = c("S1", "S2"))
    ),
    neg = NULL,
    stage = "imported"
  )

  expect_false(is_merged(x))

  x$datatable <- data.frame(Sample = c("S1", "S2"), F1 = c(1, 2))
  x$sample.info <- data.frame(Sample = c("S1", "S2"))
  x$feature.info <- data.frame(Identity = "F1")
  expect_true(is_merged(x))
})
